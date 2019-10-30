(load "base.scm")
(load "scheme-number.scm")
(load "rational.scm")
(load "complex.scm")

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (check-zero? x)  (apply-generic 'check-zero? x))


