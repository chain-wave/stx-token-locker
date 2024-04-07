(impl-trait .trait-ownable.ownable-trait)
(use-trait ft-trait .trait-sip-010.sip-010-trait)

;; ERRS
(define-constant ERR-INVALID-BLOCK (err u1000))
(define-constant ERR-INSUFFICIENT_AMOUNT (err u3000))
(define-constant ERR-INSUFFICIENT_BALANCE (err u3001))
(define-constant ERR-TRANSFER-FAILED (err u3001))
(define-constant ERR-BURN-FAILED (err u3002))
(define-constant ERR-INVALID-TOKEN (err u2026))
(define-constant ERR-INVALID-TOKEN-ID (err u2036))
(define-constant ERR-INVALID-TOKEN-LOCK (err u2046))
(define-constant ERR-LOCK_MISMATCH (err u2056))
(define-constant ERR-NOT-AUTHORIZED (err u3031))
(define-constant ERR-INVALID-AMOUNT (err u3032))
(define-constant ERR-INVALID-POOL-TOKEN (err u3050))
(define-constant ERR-FAILED (err u3060))

;; null principal place holder
(define-constant NULL_PRINCIPAL tx-sender)

;; DATA MAPS AND VARS

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
        lock-owner: principal ;; the lock owner
    }
)

;; nonce of token locks
(define-data-var lock-nonce uint u0)


;; define the locker parameters
(define-data-var stx-fee uint u1000000) ;; small stacks fee to prevent spams
(define-data-var secondary-fee-token principal NULL_PRINCIPAL) ;; in this case goat-coin
(define-data-var secondary-token-fee uint u100000000) ;; option goat-coin
(define-data-var secondary-token-discount uint u200) ;; discount on liquidity fee for burning secondary token

;; MANAGEMENT FUNCTIONS
(define-data-var contract-owner principal tx-sender)


;; read-only calls

(define-read-only (get-user-token-locks (user-addr principal) (pool-id uint)) 
  (default-to (list) (map-get? users-token-locks {user-addr: user-addr, pool-id: pool-id}))
)

;; returns the contract owner
(define-read-only (get-contract-owner)
  (ok (var-get contract-owner))
)

;; set contract owner only by current contract owner
(define-public (set-contract-owner (owner principal))
  (begin
    (try! (check-is-owner))
    (ok (var-set contract-owner owner))
  )
)

;; helper function to check if caller is owner
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
(define-public (set-fees (stx-fee_ uint) (secondary-token-fee_ uint) (secondary-token-discount_ uint)) 
  (begin 
    (try! (check-is-owner))
    
    (var-set stx-fee stx-fee_)
    (var-set secondary-token-fee  secondary-token-fee_)
    (var-set secondary-token-discount secondary-token-discount_)
    
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

;; lockToken
(define-public (lock-token (token-x-trait <ft-trait>) (token-y-trait <ft-trait>) (factor uint) (amount uint) (unlock-block uint) (fee-in-stx bool) (secondary-token-trait <ft-trait>) (withdrawer principal)) 
  (begin     
    (asserts! (< unlock-block block-height) ERR-INVALID-BLOCK)
    (asserts! (> amount u0) ERR-INSUFFICIENT_AMOUNT)

    ;; check that pool exists
    (asserts! (is-some (contract-call? 'SP3K8BC0PPEVCV7NZ6QSRWPQ2JE9E5B6N3PA0KBR9.amm-swap-pool-v1-1 get-pool-exists (contract-of token-x-trait) (contract-of token-y-trait) factor)) ERR-INVALID-POOL-TOKEN)
  
    (let 
      (
        (token-x (contract-of token-x-trait))
        (token-y (contract-of token-y-trait))
        (pool (try! (contract-call? 'SP3K8BC0PPEVCV7NZ6QSRWPQ2JE9E5B6N3PA0KBR9.amm-swap-pool-v1-1 get-pool-details token-x token-y factor)))
        (pool-id (get pool-id pool))
        (balance (unwrap-panic (contract-call? 'SP3K8BC0PPEVCV7NZ6QSRWPQ2JE9E5B6N3PA0KBR9.token-amm-swap-pool-v1-1 get-balance-fixed (pool-id) tx-sender)))
        (stxfee (var-get stx-fee))
        (secondarytokenfee (var-get secondary-token-fee))
        (sender tx-sender)
        (next-lock-id (var-get lock-nonce))
        (pool-id u0)
      )

      ;; check that user has enought balcne
      (asserts! (<= amount balance) ERR-INSUFFICIENT_BALANCE)

      (if fee-in-stx
        ;; Pay fee in STX
        (unwrap! (stx-transfer? stxfee tx-sender (as-contract sender)) ERR-TRANSFER-FAILED)
        ;; Burn token
        (begin
          (asserts! (is-eq (var-get secondary-fee-token) (contract-of secondary-token-trait)) ERR-INVALID-TOKEN)
          (unwrap! (contract-call? secondary-token-trait burn-fixed secondarytokenfee sender) ERR-BURN-FAILED)
        )
      )

      ;; transfer token to vault
      (as-contract (try! (contract-call? 'SP3K8BC0PPEVCV7NZ6QSRWPQ2JE9E5B6N3PA0KBR9.token-amm-swap-pool-v1-1 transfer-fixed pool-id amount sender .vault)))

      ;; create token lock record
      (map-set token-lock-map {lock-id: next-lock-id} { lock-block: block-height, amount: amount, initial-amount: amount, unlock-block: unlock-block, lock-owner: sender })
      (var-set lock-nonce (+ next-lock-id u1))

      ;; add lock id
      (try! (add-lock-id next-lock-id pool-id sender))
    )
    (ok true)  
  )
)


;; ;; relockToken
;; (define-public (relock-token (token-trait <ft-trait>) (token-id uint) (new-unlock-block uint)) 
;;   (begin 
;;     (asserts! (< new-unlock-block block-height) ERR-INVALID-BLOCK)

;;     (let
;;       (
;;         (sender tx-sender)
;;         (user-token-lock (unwrap! (map-get? users-token-locks { user-addr: sender, token-id: token-id }) ERR-INVALID-TOKEN-ID))
;;         (lock-id (get lock-id user-token-lock))
;;         (token (get token-addr user-token-lock))
;;         (token-lock (unwrap! (map-get? token-locks { token-addr: token, lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
;;         (token-lock-updated (merge token-lock {
;;           unlock-block: new-unlock-block
;;         }))
;;       )

;;       (asserts! (is-eq token (contract-of token-trait)) ERR-INVALID-TOKEN)
;;       (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)
;;       (asserts! (> new-unlock-block (get unlock-block token-lock)) ERR-INVALID-BLOCK)


;;       (map-set token-locks {token-addr: token, lock-id: lock-id} token-lock-updated)
;;     )

;;     (ok true)
;;   )
;; )

;; ;; withdraw
;; (define-public (withdraw-token (token-trait <ft-trait>) (token-id uint) (amount uint)) 
;;   (begin 
;;     (asserts! (> amount u0) ERR-INVALID-AMOUNT)
;;     (let
;;       (
;;         (sender tx-sender)
;;         (user-token-lock (unwrap! (map-get? users-token-locks { user-addr: sender, token-id: token-id }) ERR-INVALID-TOKEN-ID))
;;         (lock-id (get lock-id user-token-lock))
;;         (token (get token-addr user-token-lock))
;;         (token-lock (unwrap! (map-get? token-locks { token-addr: token, lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
;;         (balance-amount (get amount token-lock))
;;         (unlock-block (get unlock-block token-lock))
;;         (token-lock-updated (merge token-lock {
;;           amount: (if (<= balance-amount amount) u0 (- balance-amount amount))
;;         }))
;;       )

;;       (asserts! (>= block-height unlock-block) ERR-INVALID-BLOCK)
;;       (asserts! (is-eq token (contract-of token-trait)) ERR-INVALID-TOKEN)
;;       (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)
;;       (asserts! (<= amount balance-amount) ERR-INVALID-AMOUNT)  

;;       (as-contract (try! (contract-call? .vault transfer-ft token-trait amount sender)))

;;       (map-set token-locks {token-addr: token, lock-id: lock-id} token-lock-updated)
;;     )
;;     (ok true)
;;   )
;; )

;; ;; incrementlock
;; (define-public (increment-lock (token-trait <ft-trait>) (token-id uint) (amount uint)) 
;;   (begin 
;;     (let
;;       (
;;         (sender tx-sender)
;;         (user-token-lock (unwrap! (map-get? users-token-locks { user-addr: sender, token-id: token-id }) ERR-INVALID-TOKEN-ID))
;;         (lock-id (get lock-id user-token-lock))
;;         (token (get token-addr user-token-lock))
;;         (token-lock (unwrap! (map-get? token-locks { token-addr: token, lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
;;         (balance-amount (get amount token-lock))
;;         (unlock-block (get unlock-block token-lock))
;;         (token-lock-updated (merge token-lock {
;;           amount: (+ balance-amount amount)
;;         }))
;;       )

;;       (asserts! (is-eq token (contract-of token-trait)) ERR-INVALID-TOKEN)
;;       (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)
;;       (asserts! (> unlock-block block-height ) ERR-INVALID-BLOCK)
;;       (asserts! (<= amount balance-amount) ERR-INVALID-AMOUNT)  

;;        ;; transfer token to vault
;;       (unwrap! (contract-call? token-trait transfer-fixed amount sender .vault none) ERR-TRANSFER-FAILED)

;;       (map-set token-locks {token-addr: token, lock-id: lock-id} token-lock-updated)
;;     )
;;     (ok true)
;;   )
;; )

;; ;; splitlock
;; (define-public (split-lock (token-trait <ft-trait>) (token-id uint) (amount uint)) 
;;   (begin 
;;     (let
;;       (
;;         (sender tx-sender)
;;         (user-token-lock (unwrap! (map-get? users-token-locks { user-addr: sender, token-id: token-id }) ERR-INVALID-TOKEN-ID))
;;         (lock-id (get lock-id user-token-lock))
;;         (token (get token-addr user-token-lock))
;;         (token-lock (unwrap! (map-get? token-locks { token-addr: token, lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
;;         (balance-amount (get amount token-lock))
;;         (unlock-block (get unlock-block token-lock))
;;         (lock-block (get lock-block token-lock))
;;         (token-lock-updated (merge token-lock {
;;           amount: (if (<= balance-amount amount) u0 (- balance-amount amount))
;;         }))
;;         (next-token-id (default-to u0 (map-get? locked-tokens-count { token-addr: (contract-of token-trait) })))
;;         (next-user-token-lock-id (default-to u0 (map-get? user-locked-tokens-count { user-addr: sender })))
;;       )

;;       (asserts! (is-eq token (contract-of token-trait)) ERR-INVALID-TOKEN)
;;       (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)
;;       (asserts! (> unlock-block block-height ) ERR-INVALID-BLOCK)
;;       (asserts! (<= amount balance-amount) ERR-INVALID-AMOUNT)  

;;       (map-set token-locks {token-addr: token, lock-id: lock-id} token-lock-updated)

;;       ;; create new token lock record
;;       (map-set token-locks { token-addr: token, lock-id: next-token-id } { lock-block: lock-block, amount: amount, initial-amount: amount, unlock-block: unlock-block, lock-owner: sender })
;;       (map-set locked-tokens-count { token-addr: token } (+ next-token-id u1))

;;       ;; create new user token lock record
;;       (map-set users-token-locks { user-addr: sender, token-id: next-user-token-lock-id } { token-addr: token, lock-id: next-token-id })
;;       (map-set user-locked-tokens-count { user-addr: sender } (+ next-user-token-lock-id u1) )

;;     )
;;     (ok true)
;;   )
;; )

;; ;; transferlockownership
;; (define-public (transfer-lock-ownership (token-trait <ft-trait>) (token-id uint) (new-owner principal)) 
;;   (begin
;;     (let
;;       (
;;         (sender tx-sender)
;;         (user-token-lock (unwrap! (map-get? users-token-locks { user-addr: sender, token-id: token-id }) ERR-INVALID-TOKEN-ID))
;;         (lock-id (get lock-id user-token-lock))
;;         (token (get token-addr user-token-lock))
;;         (token-lock (unwrap! (map-get? token-locks { token-addr: token, lock-id: lock-id }) ERR-INVALID-TOKEN-LOCK))
;;         (next-user-token-lock-id (default-to u0 (map-get? user-locked-tokens-count { user-addr: sender })))
;;         (token-lock-updated (merge token-lock {
;;           lock-owner: new-owner
;;         }))
;;       )
;;       (asserts! (is-eq token (contract-of token-trait)) ERR-INVALID-TOKEN)
;;       (asserts! (is-eq (get lock-owner token-lock) sender) ERR-LOCK_MISMATCH)

;;       (map-set token-locks {token-addr: token, lock-id: lock-id} token-lock-updated)

;;       ;; create new user token lock record
;;       (map-set users-token-locks { user-addr: sender, token-id: next-user-token-lock-id } { token-addr: token, lock-id: lock-id })
;;       (map-set user-locked-tokens-count { user-addr: sender } (+ next-user-token-lock-id u1) )

;;       ;; delete previous user record
;;       (map-delete users-token-locks {user-addr: sender, token-id: lock-id})
;;     )
;;     (ok true)
;;   )
;; )

;; TODOs
;; add fees calculations
;; getter functions
;; fix transfer-lock-ownership prev-owner cleanup