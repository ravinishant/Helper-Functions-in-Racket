 
Prog 1
-------------

#lang racket

(define (bitCount num)
  (cond
    ((= 0 num) 0)
    ((= 1 num) 1)
    ((= 1 (remainder num 2)) (+ 1 (bitCount (quotient num 2))))
    (else (bitCount (quotient num 2)))
    )
  )

(bitCount 0)
(bitCount 1)
(bitCount 10)
(bitCount 65)
(bitCount 32)
(bitCount (- 32 1))

Prog 2
------------

#lang racket

(define (numCount lat num)
  (cond
    ((null? lat) 0)
    ((pair? lat)
     (cond
       ((not(integer? (car lat))) (numCount (cdr lat) num))
       ((= (car lat) num) (+ 1 (numCount (cdr lat) num)))
       (else (numCount (cdr lat) num))))
    ((= lat num) 1)
    (else 0)))

(numCount '() 0)
(numCount '(1 2 3 a 5 b 6 c "string" 7 5 9) 5)
(numCount '(5 b 6 c "string" 7 5 1 2 3 a 9) 0)
(numCount '(1 c "string" 7 5 2 3 a 5 b (6 (1 18 4) 3) 9) 1)

Prog 3
------------

#lang racket

(define (numCountExp x num)
  (define (help x)
    (cond
      ((null? x) 0)
      ((pair? x) (+ (help (car x)) (help (cdr x))))
      ((eq? x num) 1)
      (else 0)
      ))
  (help x))
  

(numCountExp '(1 2 (3 a 5) (b 6 c "string" 7 (5)) 9) 5)
(numCountExp '(1 2 (3 a 5) (b 6 c ("string") 7 (5)) 9) 0)
(numCountExp '(1 2 (3 a 5) 1 (b 6 () 1 c ("string") 7 (5)) 9) 1)

Prog 4
------------

#lang racket

(define (structurally? x y)
  (cond
    ((and (null? x) (null? y)) #t)
    ((and (pair? x) (pair? y)) (and (structurally? (car x) (car y)) (structurally? (cdr x) (cdr y))))
    ((xor (pair? x) (pair? y)) #f)
    (else
     #t)))

(structurally? '(1 2 (3 a 5) (b 6 c "string" 7 (5)) 9)
 '(2 1 (3 "string" 5) (b 6 c a 7 (5)) 9))
(structurally? '(1 2 (3 a b 5) (b 6 c "string" 7 (5)) 9)
 '(2 1 (3 "string" 5) (b 6 c d a 7 (5)) 9))
(structurally? '(a b c d) '(b c d e f))
(structurally? 'a 1234)
(structurally? '(()) '())
