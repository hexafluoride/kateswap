(use fungible-v2)
(namespace (read-msg 'ns))
(module KexRouter2 MANAGE_EXCHANGE

  (defcap MANAGE_EXCHANGE () true)

  (defun MINIMUM_LIQUIDITY:decimal ()
    0.0001)

  (defschema liquidity-pool-tokens-schema
    balance:decimal
    guard:guard
    pair:string)

  (deftable liquidity-pool-token-ledger:{liquidity-pool-tokens-schema})

  (defschema supported-tokens
    "Schema for tokens for which we have an account"
    token-module:module{fungible-v2}
    account:string)

  (deftable tokens:{supported-tokens})

  (defschema pair-reserves-schema
    "Reserves for a given token pair, keyed by (get-pair-key token1 token2)"
    reserves-left:decimal
    reserves-right:decimal
    total-supply:decimal)

  (deftable pair-reserves:{pair-reserves-schema})

  (defun round-for-token (token1:module{fungible-v2} amount:decimal)
    (floor amount (token1::precision)))

  (defun get-reserves-for-pair (token1:module{fungible-v2} token2:module{fungible-v2})
    (with-default-read pair-reserves (get-pair-key token1 token2) {"reserves-left": 0.0, "reserves-right": 0.0}
      {"reserves-left" := reserves-left, "reserves-right" := reserves-right}
      {'reserves-left: reserves-left, 'reserves-right: reserves-right}))

  (defun get-amount-in:decimal (
    reserve-in:decimal
    reserve-out:decimal
    amount-out:decimal)
    (let* (
      (numerator (* reserve-in amount-out))
      (denominator (* 0.997 (- reserve-out amount-out)))
      )
      (+ (/ numerator denominator) 1)
    ))

  (defun get-amount-out:decimal (
    reserve-in:decimal
    reserve-out:decimal
    amount-in:decimal)
    (let* (
      (amount-in-with-fee (* amount-in 0.997))
      (numerator (* amount-in-with-fee reserve-out))
      (denominator (+ reserve-in amount-in-with-fee)))
      (/ numerator denominator)))

  (defun get-amount-out-for-pair:decimal (
    token1:module{fungible-v2}
    token2:module{fungible-v2}
    amount-in:decimal)
    (let* (
        (pair-key (get-pair-key token1 token2))
        (reserves (get-reserves-for-pair token1 token2))
        (order-reversed (pair-order token1 token2))
        (left-index (if order-reversed 'reserves-right 'reserves-left))
        (right-index (if order-reversed 'reserves-left 'reserves-right))
        (left-reserve (at left-index reserves))
        (right-reserve (at right-index reserves))
      )
      (get-amount-out left-reserve right-reserve amount-in)
    ))

  (defun get-optimal-liquidity-amounts (
    token1:module{fungible-v2}
    token2:module{fungible-v2}
    amount1desired:decimal
    amount2desired:decimal
    amount1min:decimal
    amount2min:decimal)
     (let*
       ((amounts-calculated (let* (
        (current-reserves (get-reserves-for-pair token1 token2))
        (left-reserve (at 'reserves-left current-reserves))
        (right-reserve (at 'reserves-left current-reserves)))
        (if (and (= left-reserve 0.0) (= right-reserve 0.0))
          { 'amount1: amount1desired, 'amount2: amount2desired }
          (let* (
            (right-optimal (/ (* amount1desired right-reserve) left-reserve))
            (left-optimal (/ (* amount2desired left-reserve) right-reserve)))
            (if (<= right-optimal amount1desired)
              { 'amount1: amount1desired, 'amount2: right-optimal }
              { 'amount1: left-optimal, 'amount2: amount2desired})
            )
          )))
        (amount1 (at 'amount1 amounts-calculated))
        (amount2 (at 'amount2 amounts-calculated)))
        (enforce (>= amount1 amount1min) "INSUFFICIENT_AMOUNT")
        (enforce (>= amount2 amount2min) "INSUFFICIENT_AMOUNT")
        amounts-calculated
      ))

  (defun get-balance:decimal (account:string)
    (with-read liquidity-pool-token-ledger account { 'balance := balance}
      balance))

  (defun update-liquidity-delta (pair-key:string delta-left:decimal delta-right:decimal)
    (enforce-guard (create-module-guard "internal-liquidity-update"))
    (with-read pair-reserves pair-key {'reserves-left := reserves-left, 'reserves-right := reserves-right}
      (update pair-reserves pair-key {'reserves-left: (+ reserves-left delta-left), 'reserves-right: (+ reserves-right delta-right)})))

  (defun add-liquidity (
    token1:module{fungible-v2}
    token2:module{fungible-v2}
    amount1:decimal
    amount2:decimal
    amount1min:decimal
    amount2min:decimal
    from1:string
    from2:string
    to:string
    guard:guard)
    (let* (
        (pair-key (get-pair-key token1 token2))
        (reserves (get-reserves-for-pair token1 token2))
        (order-reversed (pair-order token1 token2))
        (left-token:module{fungible-v2} (if order-reversed token2 token1))
        (right-token:module{fungible-v2} (if order-reversed token1 token2))
        (left-desired (if order-reversed amount2 amount1)) ;; TODO: Determine correct amounts from current reserves
        (right-desired (if order-reversed amount1 amount2))
        (left-min (if order-reversed amount2min amount1min)) ;; TODO: Determine correct amounts from current reserves
        (right-min (if order-reversed amount1min amount2min))
        (optimal-liquidity (get-optimal-liquidity-amounts left-token right-token left-desired right-desired left-min right-min))
        (left-amount (at 'amount1 optimal-liquidity))
        (right-amount (at 'amount2 optimal-liquidity))
        (left-from (if order-reversed from2 from1))
        (right-from (if order-reversed from1 from2)))

      (install-capability (left-token::TRANSFER left-from (get-account-for-token left-token) left-amount))
      (left-token::transfer left-from (get-account-for-token left-token) left-amount)
      (install-capability (right-token::TRANSFER right-from (get-account-for-token right-token) right-amount))
      (right-token::transfer right-from (get-account-for-token right-token) right-amount)

      (let ((liquidity-account (mint-liquidity to left-token right-token guard)))
        (update-liquidity-delta pair-key left-amount right-amount)
        liquidity-account
        )
    ))

  (defun straighten (token1:module{fungible-v2} token2:module{fungible-v2} value1 value2)
    (if (pair-order token1 token2) [value1 value2] [value2 value1]))

  ; (defun sync (token1:module{fungible-v2} token2:module{fungible-v2})
  ;   (let* (
  ;     (straightened-tokens (straighten token1 token2))
  ;     (left-token (at 0 straightened-tokens))
  ;     (right-token (at 1 straightened-tokens))
  ;     (reserves (get-reserves-for-pair token1 token2))
  ;     (left-reserve (at 'reserves-left reserves))
  ;     (right-reserve (at 'reserves-right reserves))
  ;     (left-balance (left-token::balance (get-account-for-token left-token)))
  ;     (right-balance (right-token::balance (get-account-for-token right-token))))
  ;     ()
  ;     (update pair-reserves  { 'reserves-left: left-balance, 'reserves-right: right-balance })
  ;     (update-liquidity-delta (pair-key token1 token2)  )
  ;     )
  ;   )

  (defun burn (
    token1:module{fungible-v2}
    token2:module{fungible-v2}
    account:string
    to1:string
    to2:string)
    (with-read liquidity-pool-token-ledger account { 'balance := liquidity, 'guard := guard }
      (enforce-guard guard)
      (let* (
        (pair-key (get-pair-key token1 token2))
        (balance1 (token1::get-balance (get-account-for-token token1)))
        (balance2 (token2::get-balance (get-account-for-token token2)))
        (total-supply (at 'total-supply (read pair-reserves pair-key ['total-supply])))
        (amount1 (round-for-token token1 (/ (* liquidity balance1) total-supply)))
        (amount2 (round-for-token token2 (/ (* liquidity balance2) total-supply))))
          (install-capability (token1::TRANSFER (get-account-for-token token1) to1 amount1))
          (token1::transfer (get-account-for-token token1) to1 amount1)
          (install-capability (token2::TRANSFER (get-account-for-token token2) to2 amount2))
          (token2::transfer (get-account-for-token token2) to2 amount2)
          (update liquidity-pool-token-ledger account { 'balance: 0.0 })
          (if (< (canonicalize token1) (canonicalize token2))
            (update-liquidity-delta pair-key (- amount1) (- amount2))
            (update-liquidity-delta pair-key (- amount2) (- amount1)))
        )
      )
    )

  (defun min:decimal (a:decimal b:decimal)
    (if (<= a b) a b))

  (defun mint-liquidity:string (to:string token1:module{fungible-v2} token2:module{fungible-v2} guard:guard)
    (with-read pair-reserves (get-pair-key token1 token2)
      {'total-supply := total-supply, 'reserves-left := reserves-left, 'reserves-right := reserves-right}
      (let* (
        (pair (get-pair-key token1 token2))
        (row-key (+ to (+ ":" pair)))
        (amount1 (- (token1::get-balance (get-account-for-token token1)) reserves-left))
        (amount2 (- (token2::get-balance (get-account-for-token token2)) reserves-right))
        (reserve-liquidity (= total-supply 0.0))
        (liquidity (with-read pair-reserves pair {'total-supply := total-supply}
          (if reserve-liquidity
            (- (sqrt (* amount1 amount2)) (MINIMUM_LIQUIDITY))
            (min (/ (* amount1 total-supply) reserves-left) (/ (* amount2 total-supply) reserves-right))))))
        (with-default-read liquidity-pool-token-ledger (+ to (+ ":" pair))
          { 'balance: -1.0, 'guard: guard, 'pair: "" }
          { 'balance := record-balance, 'guard := record-guard, 'pair := record-pair }
          (if (= record-balance -1.0)
            (insert liquidity-pool-token-ledger row-key {'balance: liquidity, 'guard: guard, 'pair: pair })
            (update liquidity-pool-token-ledger row-key {'balance: (+ record-balance liquidity)})))
        (update pair-reserves pair {'total-supply: (+ liquidity total-supply)})
        row-key
      )))

  (defun get-token-from-id:module{fungible-v2} (token-name:string)
    (with-read tokens token-name { "token-module" := token-module }
      token-module))

  (defun get-id-from-token:string (token-module:module{fungible-v2})
    (at 0 (select tokens ['key] (where 'token-module (= token-module)))))

  (defun get-account-for-token:string (token-module:module{fungible-v2})
    (with-read tokens (canonicalize token-module) {"account" := account}
      account))

  (defun index-token (token-module:module{fungible-v2} account:string)
    (token-module::create-account account (create-module-guard 'exchange-spend))
    (insert tokens (canonicalize token-module) { 'token-module: token-module, 'account:account }))

  (defun create-pair (token1:module{fungible-v2} token2:module{fungible-v2})
    (let* (
      (pair-key (get-pair-key token1 token2))
      )
      (insert pair-reserves pair-key { 'reserves-left: 0.0, 'reserves-right: 0.0, 'total-supply: 0.0 })))

  (defun canonicalize:string (token:module{fungible-v2})
    (format "{}" [token]))

  (defun pair-order:bool (token1:module{fungible-v2} token2:module{fungible-v2})
    (< (canonicalize token1) (canonicalize token2)))

  ;; TODO: validate that token names do not contain forbidden characters
  (defun get-pair-key:string (token1:module{fungible-v2} token2:module{fungible-v2})
    (enforce (!= (canonicalize token1) (canonicalize token2)) "tokens cannot be same") ;; require that token1 and token2 are in table tokens
    (if (pair-order token1 token2)
      (+ (canonicalize token1) (+ ":" (canonicalize token2)))
      (+ (canonicalize token2) (+ ":" (canonicalize token1)))))

  (defun swap (
    token1:module{fungible-v2}
    token2:module{fungible-v2}
    amount1out:decimal
    amount2out:decimal
    to1:string
    to2:string
    )
    (let* (
      (pair-key (get-pair-key token1 token2))
      (reserves (get-reserves-for-pair token1 token2))
      (left-reserve (at 'reserves-left reserves))
      (right-reserve (at 'reserves-right reserves))
      (amount1out (round-for-token token1 amount1out))
      (amount2out (round-for-token token2 amount2out))
      )
        (enforce (< amount1out left-reserve) "KexSwap: INSUFFICIENT_LIQUIDITY")
        (enforce (< amount2out right-reserve) "KexSwap: INSUFFICIENT_LIQUIDITY")
        (install-capability (token1::TRANSFER (get-account-for-token token1) to1 amount1out))
        (install-capability (token2::TRANSFER (get-account-for-token token2) to2 amount2out))
        (if (> amount1out 0.0) (token1::transfer (get-account-for-token token1) to1 amount1out) 0.0)
        (if (> amount2out 0.0) (token2::transfer (get-account-for-token token2) to2 amount2out) 0.0)
        (let* (
          (balance1 (token1::get-balance (get-account-for-token token1)))
          (balance2 (token2::get-balance (get-account-for-token token2)))
          (amount1in (if (> balance1 (- left-reserve amount1out)) (- balance1 (- left-reserve amount1out)) 0.0))
          (amount1in (round-for-token token1 amount1in))
          (amount2in (if (> balance2 (- right-reserve amount2out)) (- balance2 (- right-reserve amount2out)) 0.0))
          (amount2in (round-for-token token2 amount2in))
          (balance1adjusted (- balance1 (* amount1in 0.003)))
          (balance1adjusted (round-for-token token1 balance1adjusted))
          (balance2adjusted (- balance2 (* amount2in 0.003)))
          (balance2adjusted (round-for-token token2 balance2adjusted)))
          (enforce (>= (* balance1adjusted balance2adjusted) (* left-reserve right-reserve)) (format "KexSwap: K ({} {} {} {})" [
            balance1adjusted, balance2adjusted, left-reserve, right-reserve
          ]))
          (update pair-reserves pair-key {
            'reserves-left: balance1,
            'reserves-right: balance2
          })
        )
    )
  )

  (defun exchange (
    token1:module{fungible-v2}
    token2:module{fungible-v2}
    amount-in:decimal
    from-account:string
    to-account:string)
    (let* (
      (pair-key (get-pair-key token1 token2))
      (reserves (get-reserves-for-pair token1 token2))
      (order-reversed (pair-order token1 token2))
      (quote (get-amount-out-for-pair token1 token2 amount-in))
      (amount1out (if order-reversed quote 0.0))
      (amount1out (round-for-token token1 amount1out))
      (amount2out (if order-reversed 0.0 quote))
      (amount2out (round-for-token token2 amount2out))
      (token-left:module{fungible-v2} (if order-reversed token2 token1))
      (token-right:module{fungible-v2} (if order-reversed token1 token2))
      )
      (install-capability (token1::TRANSFER from-account (get-account-for-token token1) amount-in))
      (token1::transfer from-account (get-account-for-token token1) amount-in)
      (swap token-left token-right amount1out amount2out to-account to-account)
      )
  )
)

(create-table tokens)
(create-table pair-reserves)
(create-table liquidity-pool-token-ledger)
