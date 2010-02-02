;;
;; Example Lambdo/Scheme file
;;

(define x 1)
(define y 2)
(define (inc x) (+ x 1))
(inc x)
(inc y)

(define (fact n)
  (if (= n 0) 1
    (* n (fact (- n 1)))))

(fact 0)
(fact 1)
(fact 2)
(fact 3)
(fact 4)
(fact 5)
(fact 6)

(define (loop from to step f)
  (if (< from to)
    (begin
      (f from)
      (loop (+ from step) to f))
    #f))

(loop 0 10 1 (lambda (n) (fact n)))
