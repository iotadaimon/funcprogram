(define l1 '(1 2 3 4 5))
(define l2 '(a b c d e))

(define (my-append! a b)
  (if (null? (cdr a))
      (set-cdr! a b)
      (my-append! (cdr a) b)))

(if (equal? l1 l2)
    '(567 22 57 93 75)
    (my-append! l1 l2))

