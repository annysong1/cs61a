;;;;;;;;;;;;;;;
;; Questions ;;
;;;;;;;;;;;;;;;

; Mutable functions in Scheme

(define (make-fib)
  (define curr 0) (define next 1) (define prev 0) 
  (set! prev curr)
  (set! curr next)
  (set! next (+ prev curr))
  prev
)