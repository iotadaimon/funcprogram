; Replace a character in text with a string
(define rcits
  (lambda (text char str)
    (cond
     ((null? text) '())
     (else (cons (rciss (car text) char str)
		 (rcits (cdr text) char str))))))

; Replace a character in the sentence with a string
(define rciss
  (lambda (sent char str)
    (cond
     ((null? sent) '())
     (else (cons (list->string
		  (rcs (string->list (car sent)) char (string->list str)))
		  (rciss (cdr sent) char str))))))

; Replace all occurences of a in str with the string b
(define rcs
  (lambda (str a b)
    (cond
     ((null? str) '())
     ((eq? a (car str)) (concat b (rcs (cdr str) a b)))
     (else (cons (car str) (rcs (cdr str) a b))))))

; Concatenate two lists together
(define concat
  (lambda (a b)
    (cond
     ((null? a) b)
     (else (concat (init a) (cons (last a) b))))))

; Get all elements of the list, except the last one
(define init
  (lambda (l)
    (cond
     ((null? l) '())
     ((null? (cdr l)) '())
     (else (cons (car l) (init (cdr l)))))))

(define text '(("Hello" "World!") ("Hi!") ("Hello!")))
(rcits text #\! "apple")
