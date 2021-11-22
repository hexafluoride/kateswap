(define-keyset 'exchange-user-keyset (read-keyset 'exchange-user-keyset))
(define-keyset 'exchange-operator-keyset (read-keyset 'exchange-operator-keyset))
(ns.write-registry (read-msg 'ns) (keyset-ref-guard 'exchange-operator-keyset) true)
(define-namespace
  (read-msg 'ns)
  (keyset-ref-guard 'exchange-user-keyset)
  (keyset-ref-guard 'exchange-operator-keyset)
)
