; Q1
(define (sign x)
  (cond 
    ((< x 0) (- 1))
    ((= x 0) 0)
    (else 1)
  )
)

; Q2
(define (square x) (* x x))

(define (pow b n)
  (cond 
    ((= n 0) 1)
    ((even? n)(square(pow b (/ n 2))))
    (else (* b (pow b (- n 1))))
  )
)

; Q3
(define (cddr s)
  (cdr (cdr s)))

(define (cadr s)
  (car (cdr s))
)

(define (caddr s)
  (car(cdr(cdr s)))
)

; Q4
(define (ordered? s)
  (if (null? (cdr s))  #t
  (if (> (car s)(car(cdr s))) #f
    (ordered? (cdr s))
  )
  )
)


; Q5
(define (nodots s)
  (cond ((null? s) (s))
    ((and (pair? s) (pair? (cdr s))) (cons (nodots(car s)) (nodots(cdr s))))
    ((and (pair? s) (null? (cdr s))) (cons (nodots(car s)) nil))
    ((pair? s) (cons (nodots(car s)) (cons (nodots(cdr s)) nil)))
    (else s)
  )
)

; Q6
(define (empty? s) (null? s))

(define (add s v)
  (cond ((empty? s) (cons v nil))
    ((= (car s) v) list s)
    ((< (car s) v) (cons (car s) (add (cdr s) v) ) )
    ((> (car s) v) (cons v s))
  )
)

; Q7
; Sets as sorted lists
(define (contains? s v)
  (cond ((null? s) #f)
    ((= (car s) v) #t)
    (else (contains? (cdr s) v))
  )
)

; Q8
(define (intersect s t)
  (cond 
    ((or (empty? s) (empty? t)) nil)
    ((= (car s)(car t)) (cons(car s) (intersect (cdr s) (cdr t))))
    ((< (car s) (car t)) (intersect(cdr s) t))
    ((> (car s) (car t)) (intersect s (cdr t)))
  )
)

(define (union s t)
  (cond
    ((and (empty? s) (empty? t)) nil)
    ((empty? s) t)
    ((empty? t) s)
    ((= (car s)(car t)) (cons(car s) (union (cdr s)(cdr t))))
    ((< (car s)(car t)) (cons(car s) (union (cdr s) t)))
    ((> (car s)(car t)) (cons(car t) (union s (cdr t))))
  )
)