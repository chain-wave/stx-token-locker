(use-trait ft-trait .trait-sip-010.sip-010-trait)

;; ERRS
(define-constant ERR-INVALID-BLOCK (err u5000))
(define-constant ERR-INSUFFICIENT_AMOUNT (err u5001))
(define-constant ERR-INSUFFICIENT_BALANCE (err u5002))
(define-constant ERR-TRANSFER-FAILED (err u5003))
(define-constant ERR-BURN-FAILED (err u5004))
(define-constant ERR-INVALID-TOKEN (err u5005))
(define-constant ERR-INVALID-TOKEN-ID (err u5006))
(define-constant ERR-INVALID-TOKEN-LOCK (err u5007))
(define-constant ERR-LOCK_MISMATCH (err u5008))
(define-constant ERR-NOT-AUTHORIZED (err u5009))
(define-constant ERR-INVALID-AMOUNT (err u6001))
(define-constant ERR-INVALID-POOL-TOKEN (err u6002))
(define-constant ERR-INVALID-LOCK (err u6003))
(define-constant ERR-FAILED (err u6004))
(define-constant ERR-OUT-OF-BOUNDS (err u6005))

;; DATA MAPS AND VARS

;; set caller as contract owner
(define-data-var contract-owner principal tx-sender)

;; maps user-addr and pool-id to lock-ids
(define-map users-token-locks
    { user-addr: principal, pool-id: uint }
    (list 200 uint) 
)

;; maps pool id of token pairs to tokenlocks
(define-map token-lock-map
    { lock-id: uint }
    {
        lock-block: uint, ;; the date the token was locked
        amount: uint,   ;; the amount of tokens still locked (initial minus withdrawals)
        initial-amount: uint, ;; the initial amount
        unlock-block: uint, ;; the date the token can be withdrawn
        lock-owner: principal, ;; the lock owner
        withdrawer: principal, ;; the address to be withdrawn to
        pool-id: uint ;; pool id from amm pool
    }
)

;; nonce of token locks
(define-data-var lock-nonce uint u0)

;; define the locker parameters
(define-data-var stx-fee uint u1000000) ;; small stacks fee to prevent spams
(define-data-var secondary-fee-token principal .memegoat) ;; in this case memegoat
(define-data-var secondary-token-fee uint u100000) ;; option memegoat ~ 10,000 memegoat

;; management calls

(define-public (set-contract-owner (owner principal))
  (begin
    (try! (check-is-owner)) 
    (ok (var-set contract-owner owner))
  )
)

;; read-only calls

(define-read-only (get-user-token-locks (user-addr principal) (pool-id uint)) 
  (default-to (list) (map-get? users-token-locks {user-addr: user-addr, pool-id: pool-id}))
)

;; returns the contract owner
(define-read-only (get-contract-owner)
  (ok (var-get contract-owner))
)

(define-read-only (get-token-lock-by-id (lock-id uint))
  (ok (unwrap! (map-get? token-lock-map {lock-id: lock-id}) ERR-INVALID-LOCK))
)

;; private calls

(define-private (check-is-owner)
  (ok (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED))
)

;; set secondary fee token
(define-public (set-secondary-fee-token (secondary-token-trait <ft-trait>)) 
  (begin 
    (try! (check-is-owner))
    
    (var-set secondary-fee-token (contract-of secondary-token-trait)) 
    
    (ok true)
  )
)

;; fees for locking tokens
(define-public (set-fees (stx-fee_ uint) (secondary-token-fee_ uint)) 
  (begin 
    (try! (check-is-owner))
    (var-set stx-fee stx-fee_)
    (var-set secondary-token-fee  secondary-token-fee_)
    (ok true)
  )
)

(define-private (add-lock-id (lock-id uint) (pool-id uint) (user-addr principal))
  (begin
    (and 
      (is-none (index-of (get-user-token-locks user-addr pool-id) lock-id))
      (map-set users-token-locks {user-addr: user-addr, pool-id: pool-id} (unwrap! (as-max-len? (append (get-user-token-locks user-addr pool-id) lock-id) u200) ERR-FAILED))
    )
    (ok true)
  )
)

(define-private (remove-lock-id (index uint) (lock-id uint) (pool-id uint) (user-addr principal))
  (begin
    (let (
          (lock-ids (get-user-token-locks user-addr pool-id))
          (length (len lock-ids))
          (last-item (unwrap! (element-at lock-ids (- length u1)) ERR-OUT-OF-BOUNDS))
          (item-to-remove (unwrap! (element-at lock-ids index) ERR-OUT-OF-BOUNDS))
          (updated-lists-v1 (unwrap! (replace-at? lock-ids (- length u1) item-to-remove) ERR-OUT-OF-BOUNDS)) 
          (updated-lists-v2 (unwrap! (replace-at? updated-lists-v1 index last-item) ERR-OUT-OF-BOUNDS)) 
        )
        (map-set users-token-locks {user-addr: user-addr, pool-id: pool-id} (unwrap! (as-max-len? (unwrap-panic (slice? updated-lists-v2 u0 (- length u1))) u200) ERR-FAILED))
    )
    (ok true)
  )
)

;; lockToken
(define-public (lock-token (token-x-trait <ft-trait>) (token-y-trait <ft-trait>) (factor uint) (amount uint) (unlock-block uint) (fee-in-stx bool) (secondary-token-trait <ft-trait>) (withdrawer principal)) 
  (begin     
    (asserts! (> unlock-block block-height) ERR-INVALID-BLOCK)
    (asserts! (> amount u0) ERR-INSUFFICIENT_AMOUNT)

    ;; check that pool exists
    (asserts! (is-some (contract-call? .amm-swap-pool-v1-1 get-pool-exists (contract-of token-x-trait) (contract-of token-y-trait) factor)) ERR-INVALID-POOL-TOKEN)
  
    (let 
      (
        (token-x (contract-of token-x-trait))
        (token-y (contract-of token-y-trait))
        (pool (try! (contract-call? .amm-swap-pool-v1-1 get-pool-details token-x token-y factor)))
        (pool-id (get pool-id pool))
        (token-balance (unwrap-panic (contract-call? .token-amm-swap-pool-v1-1 get-balance-fixed pool-id tx-sender)))
        (stxfee (var-get stx-fee))
        (secondarytokenfee (var-get secondary-token-fee))
        (sender tx-sender)
        (next-lock-id (+ (var-get lock-nonce) u1))
      )

      ;; check that user has enought balance
      (asserts! (<= amount token-balance) ERR-INSUFFICIENT_BALANCE)

      (if fee-in-stx
        ;; Pay fee in STX
        (try! (stx-transfer? stxfee tx-sender .memegoat-vault))
        ;; Burn token
        (begin
          (asserts! (is-eq (var-get secondary-fee-token) (contract-of secondary-token-trait)) ERR-INVALID-TOKEN)
          (try! (contract-call? secondary-token-trait burn-fixed secondarytokenfee sender))
        )
      )

      ;; transfer token to vault
      (try! (contract-call? .token-amm-swap-pool-v1-1 transfer-fixed pool-id amount sender .memegoat-vault))
      
      ;; create token lock record
      (map-set token-lock-map {lock-id: next-lock-id} { lock-block: block-height, amount: amount, initial-amount: amount, unlock-block: unlock-block, lock-owner: sender , pool-id: pool-id, withdrawer: withdrawer})

      ;; add lock id
      (try! (add-lock-id next-lock-id pool-id sender))

      ;; update lock nonce
      (var-set lock-nonce next-lock-id)
    )
    (ok true)  
  )
)

;; relockToken
(define-public (relock-token (token-x-trait <ft-trait>) (token-y-trait <ft-trait>) (factor uint) (index uint) (new-unlock-block uint)) 
  (begin 
    ;; check that pool exists
    (asserts! (is-some (contract-call? .amm-swap-pool-v1-1 get-pool-exists (contract-of token-x-trait) (contract-of token-y-trait) factor)) ERR-INVALID-POOL-TOKEN)
  
    (let
      (
        (token-x (contract-of token-x-trait))
        (token-y (contract-of token-y-trait))
        (pool (try! (contract-call? .amm-swap-pool-v1-1 get-pool-details token-x token-y factor)))
        (pool-id (get pool-id pool))
        (sender tx-sender)
        (lock-id (unwrap! (element-at (get-user-token-locks sender pool-id) index) ERR-OUT-OF-BOUNDS))
        (token-lock (unwrap! (map-get? token-lock-map { lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
        (token-lock-updated (merge token-lock {
          unlock-block: new-unlock-block
        }))
      )

      (asserts! (and (> new-unlock-block (get lock-block token-lock)) (> new-unlock-block block-height)) ERR-INVALID-BLOCK)

      (asserts! (is-some (index-of (get-user-token-locks sender pool-id) lock-id)) ERR-OUT-OF-BOUNDS)

      (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)

      (map-set token-lock-map { lock-id: lock-id } token-lock-updated)
    )
    (ok true)
  )
)

;; withdraw
(define-public (withdraw-token (token-x-trait <ft-trait>) (token-y-trait <ft-trait>) (factor uint) (index uint) (amount uint)) 
  (begin 
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    ;; check that pool exists
    (asserts! (is-some (contract-call? .amm-swap-pool-v1-1 get-pool-exists (contract-of token-x-trait) (contract-of token-y-trait) factor)) ERR-INVALID-POOL-TOKEN)
  
    (let
      (
        (token-x (contract-of token-x-trait))
        (token-y (contract-of token-y-trait))
        (pool (try! (contract-call? .amm-swap-pool-v1-1 get-pool-details token-x token-y factor)))
        (pool-id (get pool-id pool))
        (sender tx-sender)
        (lock-id (unwrap! (element-at (get-user-token-locks sender pool-id) index) ERR-OUT-OF-BOUNDS))
        (token-lock (unwrap! (map-get? token-lock-map { lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
        (locked-balance (get amount token-lock))
        (unlock-block (get unlock-block token-lock))
        (withdrawer (get withdrawer token-lock))
        (token-lock-updated (merge token-lock {
          amount: (if (<= locked-balance amount) u0 (- locked-balance amount))
        }))
      )

      (asserts! (>= block-height unlock-block) ERR-INVALID-BLOCK)
      (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)
      (asserts! (<= amount locked-balance) ERR-INVALID-AMOUNT)  

      (asserts! (is-some (index-of (get-user-token-locks sender pool-id) lock-id)) ERR-OUT-OF-BOUNDS)

      ;; transfer token from vault
      (as-contract (try! (contract-call? .memegoat-vault withdraw-liquidity-token pool-id amount withdrawer))) 

      (map-set token-lock-map { lock-id: lock-id} token-lock-updated)
    )
    (ok true)
  )
)

;; incrementlock
(define-public (increment-lock (token-x-trait <ft-trait>) (token-y-trait <ft-trait>) (factor uint) (index uint) (amount uint)) 
  (begin 
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    ;; check that pool exists
    (asserts! (is-some (contract-call? .amm-swap-pool-v1-1 get-pool-exists (contract-of token-x-trait) (contract-of token-y-trait) factor)) ERR-INVALID-POOL-TOKEN)

    (let
      (
        (token-x (contract-of token-x-trait))
        (token-y (contract-of token-y-trait))
        (pool (try! (contract-call? .amm-swap-pool-v1-1 get-pool-details token-x token-y factor)))
        (pool-id (get pool-id pool))
        (token-balance (unwrap-panic (contract-call? .token-amm-swap-pool-v1-1 get-balance-fixed pool-id tx-sender)))
        (sender tx-sender)
        (lock-id (unwrap! (element-at (get-user-token-locks sender pool-id) index) ERR-OUT-OF-BOUNDS))
        (token-lock (unwrap! (map-get? token-lock-map { lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
        (locked-balance (get amount token-lock))
        (token-lock-updated (merge token-lock {
          amount: (+ locked-balance amount)
        }))
      )

      ;; check that caller is owner
      (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)

      ;; check that user has enought balance
      (asserts! (<= amount token-balance) ERR-INSUFFICIENT_BALANCE)

      (asserts! (is-some (index-of (get-user-token-locks sender pool-id) lock-id)) ERR-OUT-OF-BOUNDS)

      ;; transfer token to vault
      (try! (contract-call? .token-amm-swap-pool-v1-1 transfer-fixed pool-id amount sender .memegoat-vault))

      (map-set token-lock-map { lock-id: lock-id} token-lock-updated)
    )
    (ok true)
  )
)

;; splitlock
(define-public (split-lock (token-x-trait <ft-trait>) (token-y-trait <ft-trait>) (factor uint) (index uint) (amount uint)) 
  (begin 
      (asserts! (> amount u0) ERR-INVALID-AMOUNT)

      ;; check that pool exists
      (asserts! (is-some (contract-call? .amm-swap-pool-v1-1 get-pool-exists (contract-of token-x-trait) (contract-of token-y-trait) factor)) ERR-INVALID-POOL-TOKEN)
  
    (let
      (
        (token-x (contract-of token-x-trait))
        (token-y (contract-of token-y-trait))
        (pool (try! (contract-call? .amm-swap-pool-v1-1 get-pool-details token-x token-y factor)))
        (pool-id (get pool-id pool))
        (sender tx-sender)
        (lock-id (unwrap! (element-at (get-user-token-locks sender pool-id) index) ERR-OUT-OF-BOUNDS))
        (token-lock (unwrap! (map-get? token-lock-map { lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
        (locked-balance (get amount token-lock))
        (unlock-block (get unlock-block token-lock))
        (lock-block (get lock-block token-lock))
        (withdrawer (get withdrawer token-lock))
        (token-lock-updated (merge token-lock {
          amount: (if (<= locked-balance amount) u0 (- locked-balance amount))
        }))
        (next-lock-id (+ (var-get lock-nonce) u1))
      )

      (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)
      (asserts! (<= amount locked-balance) ERR-INVALID-AMOUNT)  

      (asserts! (is-some (index-of (get-user-token-locks sender pool-id) lock-id)) ERR-OUT-OF-BOUNDS)

      (map-set token-lock-map {lock-id: lock-id} token-lock-updated)

      ;; create new token lock record
      (map-set token-lock-map {lock-id: next-lock-id} { lock-block: block-height, amount: amount, initial-amount: amount, unlock-block: unlock-block, lock-owner: sender , pool-id: pool-id, withdrawer: withdrawer})

      ;; add lock id
      (try! (add-lock-id next-lock-id pool-id sender))

      (var-set lock-nonce next-lock-id)
    )
    (ok true)
  )
)

;; transferlockownership
(define-public (transfer-lock-ownership (token-x-trait <ft-trait>) (token-y-trait <ft-trait>) (factor uint) (index uint) (new-owner principal) (new-withdrawer principal)) 
  (begin
      
    ;; check that pool exists
    (asserts! (is-some (contract-call? .amm-swap-pool-v1-1 get-pool-exists (contract-of token-x-trait) (contract-of token-y-trait) factor)) ERR-INVALID-POOL-TOKEN)

    (let
      (
        (token-x (contract-of token-x-trait))
        (token-y (contract-of token-y-trait))
        (pool (try! (contract-call? .amm-swap-pool-v1-1 get-pool-details token-x token-y factor)))
        (pool-id (get pool-id pool))
        (token-balance (unwrap-panic (contract-call? .token-amm-swap-pool-v1-1 get-balance-fixed pool-id tx-sender)))
        (sender tx-sender)
        (lock-id (unwrap! (element-at (get-user-token-locks sender pool-id) index) ERR-OUT-OF-BOUNDS))
        (token-lock (unwrap! (map-get? token-lock-map { lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
        (token-lock-updated (merge token-lock {
          lock-owner: new-owner,
          withdrawer: new-withdrawer
        }))
      )

      (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)

      (asserts! (is-some (index-of (get-user-token-locks sender pool-id) lock-id)) ERR-OUT-OF-BOUNDS)

      (map-set token-lock-map {lock-id: lock-id} token-lock-updated)

      ;; add lock id
      (try! (add-lock-id lock-id pool-id new-owner))

      (try! (remove-lock-id index lock-id pool-id sender))
    )
    (ok true)
  )
)

;; TODOs
;; add fees calculations