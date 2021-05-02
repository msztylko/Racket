
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
    (cond [(null? xs) (error "list-nth-mod: empty list")]
          [(< n 0)    (error "list-nth-mod: negative number")]
          [#t (let* ([len (length xs)]
                     [i (remainder n len)])
                (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
    (if (= n 0)
    null
    (let ([next-ans (car (s))]
          [next-s (stream-for-n-steps (cdr (s)) (- n 1))])
         (cons next-ans next-s))))

(define funny-number-stream
    (letrec ([ans (lambda (x) (if (= (remainder x 5) 0) (- 0 x) x))]             
             [f (lambda (y) (cons (ans y) (lambda () (f (+ y 1)))))])
            (lambda () (f 1))))

(define dan-then-dog
  (letrec ([dan-st (lambda () (cons "dan.jpg" dog-st))]
           [dog-st (lambda () (cons "dog.jpg" dan-st))])
    dan-st))

(define (stream-add-zero s)
  (lambda ()
    (let ([next (s)])
      (cons (cons 0 (car next)) (stream-add-zero (cdr next))))))

(define (cycle-lists xs ys)
  (letrec ([loop (lambda (n)
                   (cons (cons (list-nth-mod xs n)
                               (list-nth-mod ys n))
                         (lambda () (loop (+ n 1)))))])
    (lambda () (loop 0))))

(define (vector-assoc v vec)
  (letrec ([loop (lambda (i)
                   (if (= i (vector-length vec))
                       #f
                       (let ([x (vector-ref vec i)])
                         (if (and (cons? x) (equal? (car x) v))
                             x
                             (loop (+ i 1))))))])
    (loop 0)))

(define (cached-assoc lst n)
  (let ([cache (make-vector n #f)]
        [next-to-replace 0])
    (lambda (v)
      (or (vector-assoc v cache)
          (let ([ans (assoc v lst)])
            (and ans
                 (begin (vector-set! cache next-to-replace ans)
                        (set! next-to-replace 
                              (if (= (+ next-to-replace 1) n)
                                  0
                                  (+ next-to-replace 1)))
                        ans)))))))
