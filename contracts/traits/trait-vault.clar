(use-trait ft-trait .trait-sip-010.sip-010-trait)

(define-trait vault-trait
    (   
        ;; returns the balance of token
        (get-balance (<ft-trait>) (response uint uint))
        
    )
)
