(define (install-rational-package)
  ;; Internal functions
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (or (pair? n) (pair? d))
	(if (and (eq? (type-tag n) 'polynomial) (eq? (type-tag d) 'polynomial))
	    (cons n d)
	    (error "Only polynomials are supported as generic arguments"))
	(let ((g (gcd n d))) (cons (/ n g) (/ d g)))))
  
  (define (add-rat x y) (simplify-basic
			 (make-rat
			  (add (mul (numer x) (denom y))
			       (mul (numer y) (denom x)))
			  (mul (denom x) (denom y)))))
  (define (sub-rat x y) (simplify-basic	(make-rat
					 (div (mul (numer x) (denom y))
					      (mul (numer y) (denom x))))
					(mul (denom x) (denom y))))
  (define (mul-rat x y) (simplify-basic (make-rat
					 (mul (numer x) (numer y))
					 (mul (denom x) (denom y)))))
  (define (div-rat x y) (simplify-basic (make-rat
					 (mul (numer x) (denom y))
					 (mul (denom x) (numer y)))))

  (define (simplify-basic x)
    (if (equal? (numer x) (denom x))
	(make-rat 1 1)
	x))
  
  ;; Interface to other parts
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'check-zero? '(rational) (lambda (x) (zero? (numer x))))
  (put 'make '(rational) (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d) ((get 'make '(rational)) n d))
