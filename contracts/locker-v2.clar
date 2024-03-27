(impl-trait .trait-ownable.ownable-trait)
(use-trait ft-trait .trait-sip-010.sip-010-trait)

;; ERRS

(define-constant ERR-INVALID-TIME (err u1000))
(define-constant ERR-INSUFFIENCIENT_AMOUNT (err u1001))
(define-constant ERR-TRANSFER-FAILED (err u3000))

(define-constant ERR-INVALID-POOL (err u2001))
(define-constant ERR-INVALID-LIQUIDITY (err u2003))
(define-constant ERR-POOL-ALREADY-EXISTS (err u2000))
(define-constant ERR-TOO-MANY-POOLS (err u2004))
(define-constant ERR-PERCENT-GREATER-THAN-ONE (err u5000))
(define-constant ERR-EXCEEDS-MAX-SLIPPAGE (err u2020))
(define-constant ERR-ORACLE-NOT-ENABLED (err u7002))
(define-constant ERR-ORACLE-ALREADY-ENABLED (err u7003))
(define-constant ERR-ORACLE-AVERAGE-BIGGER-THAN-ONE (err u7004))
(define-constant ERR-INVALID-TOKEN (err u2026))


;; DATA MAPS AND VARS

;; maps user-addr and token_id from count to the token address
(define-map users-info
    {user-addr: principal, token-id: uint}
    {pair-addr: principal, lock-id: uint}
)

;; maps the number of tokens user has locked in locker
(define-map user-locked-tokens principal uint)

;; maps tokenPairs to tokenlocks
(define-map token-locks
    {pair-addr: principal, lock_id: uint}
    {
        lock-date: uint, ;; the date the token was locked
        amount: uint,   ;; the amount of tokens still locked (initial minus withdrawals)
        initial-amount: uint, ;; the initial amount
        unlock-date: uint, ;; the date the token can be withdrawn
    }
)

;; maps the pair to the number of token locks
(define-map locked-tokens-count principal uint)

;; maps whitelisted principals (whitelisted principals don't pay flatrate fees on locking)
(define-map fee-white-list { user: principal } { whitelisted: bool })

;; null principal place holder
(define-constant NULL_PRINCIPAL tx-sender)

;; define the locker parameters
(define-data-var stx-fee uint u1000000) ;; small stacks fee to prevent spams
(define-data-var secondary-fee-token principal NULL_PRINCIPAL) ;; in this case goat-coin
(define-data-var secondary-token-fee uint u100000000) ;; option goat-coin
(define-data-var secondary-token-discount uint u200) ;; discount on liquidity fee for burning secondary token
(define-data-var liquidity-fee uint u10) ;; fee on liquidity tokens
(define-data-var referral-percent uint u250) ;; fee for referrals
(define-data-var referral-token principal NULL_PRINCIPAL)  ;; token the refferrer must hold to qualify as a referrer
(define-data-var referral-hold uint u10000000) ;; balance the referrer must hold to quality as a referrer
(define-data-var referral-discount uint u100)  ;; discount on flatrate fees for using a valid referral address


;; MANAGEMENT FUNCTIONS
(define-data-var contract-owner principal tx-sender)

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
    (
      (var-set secondary-fee-token (contract-of secondary-token-trait)) 
    )
    (ok true)
  )
)

;; referrers need to hold the specified token and hold amount to be elegible for referral fees
(define-public (set-referral-token-and-hold (referral-token-trait <ft-trait>) (min-hold uint) )
  (begin
    (try! (check-is-owner))
    (
      (var-set referral-token (contract-of referral-token-trait))
      (var-set referral-hold min-hold)
    )
    (ok true)
  )
)

;; fees for carrying each
(define-public (set-fees (referral-percent_ uint) (referral-discount_ uint) (stx-fee_ uint) (secondary-token-fee_ uint) (secondary-token-discount_ uint) (liquidity-fee_ uint)) 
  (begin 
    (try! (check-is-owner))
    (
      (var-set referral-percent referral-percent_)
      (var-set referral-discount referral-discount_)
      (var-set stx-fee stx-fee_)
      (var-set secondary-fee-token  secondary-token-fee_)
      (var-set secondary-token-discount secondary-token-discount_)
      (var-set liquidity-fee liquidity-fee_)
    )
    (ok true)
  )
)

;; whitelisted accounts dont pay flatrate fees on locking
(define-public (whitelist-fee-account (user principal) (add bool)) 
  (begin
    (if (add) (map-insert fee-white-list {user: user} {whitelisted: add}) (map-delete fee-white-list {user: user}))
    (ok true)
  )
)

;; lockToken
(define-public (lock-token (token-trait <ft-trait>) (amount uint) (unlock-block uint) (referral principal) (referral-trait <ft-trait>) (fee-in-stx bool) (withdrawer principal)) 
  (let 
    (
      (token (contract-of token-trait))
      (stxfee (var-get stx-fee))
    )

    (asserts! (< unlock-block block-height) ERR-INVALID-TIME)
    (asserts! (> amount u0) ERR-INSUFFIENCIENT_AMOUNT)

    ;; TODO: check if token is an lp token
    
    
    ;; check if referral principal has a set token hold
    (if (not (is-none referral)) 
      (
        (asserts! (>= (contract-call? referral-trait get-balance-fixed (as-contract referral)) (var-get referral-hold)) ERR-INSUFFIENCIENT_AMOUNT)
      )
    )

    (if (is-some (map-get? fee-white-list tx-sender)) 
      (
        ;; use flat rate
        (if (fee-in-stx) 
          ;; check if there's a referral and calculate referral fee to send
          (if (not (is-none referral)) ())
          (unwrap! (stx-transfer? stxfee tx-sender .vault) ERR-TRANSFER-FAILED)
        )
      ) 
      
      (

        ;; charge fee in token


      )
    )








    
  )
)

;; relockToken

;; withdraw

;; incrementlock

;; splitlock

;; transferlockownership

;; getter functions