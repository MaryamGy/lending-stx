;; lending-stx.clar Over-collateralized STX borrowing against SIP-010 collateral
;; Collateral: SIP-010 token (e.g., a USD stablecoin)
;; Liquidation: repay borrower STX and seize collateral at discount
;; Oracle returns microSTX price for the collateral token.

;; ----- Traits -----
(define-trait sip010-ft-trait
  (
    (transfer? (uint principal principal) (response bool uint))
    (get-balance (principal) (response uint uint))
    (get-decimals () (response uint uint))
  ))

(define-trait price-oracle-trait
  (
    (get-price (principal) (response uint uint))
  ))

;; ----- Constants & errors -----
(define-constant BPS u10000)
(define-constant PRICE_SCALE u1000000)      ;; 1e6
(define-constant ERR_UNAUTHORIZED u100)
(define-constant ERR_BAD_AMOUNT u101)
(define-constant ERR_HEALTH u102)
(define-constant ERR_NOT_UNDERWATER u103)
(define-constant ERR_TRANSFER u104)
(define-constant ERR_PRICE u105)
(define-constant ERR_NO_LIQUIDITY u106)
(define-constant ERR_INSUFF_COLL u107)

;; ----- Config (set at deploy) -----
(define-data-var token-coll principal tx-sender)
(define-data-var oracle principal tx-sender)
(define-data-var ltv-bps uint u7000)            ;; 70% max LTV
(define-data-var liq-threshold-bps uint u7500)  ;; 75% liquidation threshold
(define-data-var liq-bonus-bps uint u500)       ;; 5% extra collateral to liquidator

;; ----- State -----
(define-map user-coll { user: principal } { amount: uint })  ;; units of collateral token
(define-map user-debt { user: principal } { stx: uint })     ;; microSTX owed

;; STX liquidity supplied by lenders/admin sits in the contract balance.
;; Optional accounting for supplier balances could be added later.

;; ----- Views -----
(define-read-only (get-config)
  { token-coll: (var-get token-coll),
    oracle: (var-get oracle),
    ltv: (var-get ltv-bps),
    liq: (var-get liq-threshold-bps),
    bonus: (var-get liq-bonus-bps) })

(define-read-only (collateral-of (who principal))
  (default-to u0 (get amount (map-get? user-coll { user: who })) ))

(define-read-only (debt-of (who principal))
  (default-to u0 (get stx (map-get? user-debt { user: who })) ))

(define-read-only (price-of (token principal))
  (ok PRICE_SCALE))

(define-read-only (values (who principal))
  (let ((c (collateral-of who))
        (p (unwrap-panic (price-of (var-get token-coll))))
        (d (debt-of who)))
    ;; All in microSTX*PRICE_SCALE units for precision: value = c * p
    { coll-v: (* c p), debt-v: (* d PRICE_SCALE) }))

(define-read-only (health (who principal))
  (let ((v (values who)) (ltv (var-get ltv-bps)))
    (let ((cv (get coll-v v)) (dv (get debt-v v)))
      (if (is-eq dv u0)
          (ok u1000000000)
          (ok (/ (/ (* cv ltv) BPS) dv))))))

;; ----- Admin -----
(define-read-only (is-admin (who principal)) (is-eq who tx-sender))

(define-public (set-oracle (o principal))
  (begin 
    (asserts! (is-admin tx-sender) (err ERR_UNAUTHORIZED))
    (var-set oracle o) 
    (ok true)))

(define-public (set-params (ltv uint) (liq uint) (bonus uint))
  (begin 
    (asserts! (is-admin tx-sender) (err ERR_UNAUTHORIZED))
    (var-set ltv-bps ltv)
    (var-set liq-threshold-bps liq)
    (var-set liq-bonus-bps bonus)
    (ok true)))

;; ----- Internals -----
(define-private (ft-transfer (token <sip010-ft-trait>) (amount uint) (from principal) (to principal))
  (match (contract-call? token transfer? amount from to) 
    success (ok true) 
    error (err ERR_TRANSFER)))

;; ----- User flows -----
;; 1) Deposit collateral (SIP-010)
(define-public (deposit-collateral (token <sip010-ft-trait>) (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (try! (ft-transfer token amount tx-sender (as-contract tx-sender)))
    (let ((prev (collateral-of tx-sender)))
      (map-set user-coll { user: tx-sender } { amount: (+ prev amount) })
      (ok true))))

;; 2) Withdraw collateral (health check vs current debt)
(define-public (withdraw-collateral (token <sip010-ft-trait>) (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (let ((prev (collateral-of tx-sender)))
      (asserts! (>= prev amount) (err ERR_INSUFF_COLL))
      (let (
            (p (unwrap-panic (price-of (var-get token-coll))))
            (dv (* (debt-of tx-sender) PRICE_SCALE))
            (new-coll (* (- prev amount) p))
            (ltv (var-get ltv-bps))
           )
        (asserts! (or (is-eq dv u0) (>= (/ (* new-coll ltv) BPS) dv)) (err ERR_HEALTH))
        (map-set user-coll { user: tx-sender } { amount: (- prev amount) })
        (try! (ft-transfer token amount (as-contract tx-sender) tx-sender))
        (ok true)))))

;; 3) Borrow STX (sent from contract to user)
(define-public (borrow-stx (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    ;; Ensure contract has STX liquidity
    (asserts! (>= (as-contract (stx-get-balance tx-sender)) amount) (err ERR_NO_LIQUIDITY))
    ;; LTV check after new debt
    (let ((v (values tx-sender))
          (ltv (var-get ltv-bps)))
      (let ((new-dv (+ (get debt-v v) (* amount PRICE_SCALE))))
        (asserts! (>= (/ (* (get coll-v v) ltv) BPS) new-dv) (err ERR_HEALTH))
        (let ((prev (debt-of tx-sender)))
          (map-set user-debt { user: tx-sender } { stx: (+ prev amount) })
          (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
          (ok true))))))

;; 4) Repay STX
(define-public (repay-stx (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (let ((debt (debt-of tx-sender)))
      (let ((pay (if (> amount debt) debt amount)))
        (try! (stx-transfer? pay tx-sender (as-contract tx-sender)))
        (map-set user-debt { user: tx-sender } { stx: (- debt pay) })
        (ok true)))))

;; 5) Liquidation: repay borrower STX and seize collateral at discount
(define-public (liquidate (token <sip010-ft-trait>) (user principal) (repay uint))
  (begin
    (asserts! (> repay u0) (err ERR_BAD_AMOUNT))
    (let ((v (values user))
          (lt (var-get liq-threshold-bps)))
      (asserts! (< (/ (* (get coll-v v) lt) BPS) (get debt-v v)) (err ERR_NOT_UNDERWATER))
    (let ((user-debt-val (debt-of user))
      (actual (if (> repay user-debt-val) user-debt-val repay))
      (pc (unwrap-panic (price-of (var-get token-coll))))     ;; microSTX per 1 collateral
      (repayValue (* actual PRICE_SCALE))      ;; microSTX*PRICE_SCALE
      ;; seize = repayValue / pc * (1 + bonus)
      (bonus (var-get liq-bonus-bps))
      (base (/ (* repayValue BPS) pc))
      (seize (+ base (/ (* base bonus) BPS))))
        ;; take STX from liquidator
      (try! (stx-transfer? actual tx-sender (as-contract tx-sender)))
      ;; reduce borrower debt
      (map-set user-debt { user: user } { stx: (- user-debt-val actual) })
      ;; move collateral to liquidator
      (let ((uc (collateral-of user)))
        (asserts! (>= uc seize) (err ERR_INSUFF_COLL))
        (map-set user-coll { user: user } { amount: (- uc seize) })
        (try! (ft-transfer token seize (as-contract tx-sender) tx-sender))
        (ok true))))))

;; ----- Funding STX liquidity (admin top-up/withdraw) -----
(define-public (fund-liquidity (amount uint))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_UNAUTHORIZED))
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (ok true)))

(define-public (defund-liquidity (amount uint))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_UNAUTHORIZED))
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (asserts! (>= (as-contract (stx-get-balance tx-sender)) amount) (err ERR_NO_LIQUIDITY))
    (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
    (ok true)))
