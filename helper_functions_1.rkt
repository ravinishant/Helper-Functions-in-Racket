#lang racket

;****************** list2Bag ************************

(define (list2bag lat)
  (counter_list (sort_val lat)))

(define (counter_list lst) 
        (if (null? lst)
         '()
        (cons (cons (car lst) (cons (count_list_help (car lst) lst) '()))
              (counter_list (delete_duplicates (car lst) lst)))))

(define (count_list_help x lst)
        (cond ((null? lst) 0)
        ((eq? x (car lst)) (+ 1 (count_list_help x (cdr lst))))
        (else (count_list_help x (cdr lst)))))

(define delete_duplicates
  (lambda (item lst)
    (cond
      ((null? lst) '())
     ((equal? item (car lst)) (delete_duplicates item (cdr lst)))
     (else (cons (car lst) (delete_duplicates item (cdr lst)))))))

(define sort_val
  (lambda (numList)
    (cond
      ((null? numList) '())
      ((= (car numList) (apply min numList))
       (cons (car numList) (sort_val (cdr numList))))
      (else (sort_val (append (list-tail numList (- (length numList) 1))
                             (reverse (list-tail (reverse numList) 1)))))
      )))


;*************** bag? *********************

(define (bag? lat)
  (bag_help? lat lat))

(define (bag_help? lat lat1)

  (cond
    ((null? lat) 
     (cond 
       ((equal? (evaluateOrder? lat1) #t) #t)
       (else #f)))
    ((pair? lat)
    (cond
      ((not (pair? (car lat))) #f)
      ((equal? (count-atoms (car lat)) 2) (bag_help? (cdr lat) lat1)) 
      (else #f)
      ))
    (else #f)))

(define (count-atoms x)
  (if (pair? x) 
      (+ (count-atoms (car x)) 
         (count-atoms (cdr x)))
      (if (null? x)
          0 
          1)))

(define (evaluateOrder? lat)
  (isOrdered? (extract_val lat)))

(define (extract_val lat)
  (cond
    ((null? lat) '())
    ((pair? (car lat)) (cons (car (car lat)) (extract_val (cdr lat))))
    ))

(define (isOrdered? lst)
      (cond ((null? lst) #t)
            ((eq? (length lst) 1) 
             (cond
               ((eq? (car lst) 0) #f)
               (else #t)))
      ((> (car (cdr lst)) (car lst))
        (isOrdered? (cdr lst)))
      (else #f))
     )

;***************** countBag ************************* 

(define (countBag lst)
  (cond
    ((null? lst) 0)
    ((not (bag? lst)) #f)
    (else (+ (car (cdr (car lst))) (countBag (cdr lst))))))


;**************** member? ***************************

(define (member? x mincard bag)
  (cond
    ((null? bag) #t)
    ((not (bag? bag)) #f)
    ((<= mincard (get_cardinality x bag)) #t)
    (else #f)))

(define (get_cardinality x bag)
  (cond
    ((null? bag) 0)
    ((eq? (car (car bag)) x) (car (cdr (car bag))))
    (else (get_cardinality x (cdr bag)))))

;**************** union ***************************

(define (union bag1 bag2)
(define (union_help bag1 bag2 bag)
(cond
((null? bag1)
(cond
((null? bag2) bag)
(else (union_help bag1 (cdr bag2) (append bag (cons (car bag2) '()))))))
(else
(cond
((null? bag2) (union_help (cdr bag1) bag2 (append bag (cons (car bag1) '()))))
(else
(cond
((= (car (car bag1)) (car (car bag2)))
(union_help (cdr bag1) (cdr bag2) (append bag (cons (list (car (car bag1)) (+ (car (cdr
(car bag1))) (car (cdr (car bag2))))) '()))))
((< (car (car bag1)) (car (car bag2))) (union_help (cdr bag1) bag2 (append bag (cons
(car bag1) '()))))
((> (car (car bag1)) (car (car bag2))) (union_help bag1 (cdr bag2) (append bag (cons
(car bag2) '()))))
)
)))
))
(union_help bag1 bag2 '()))

;*********************** intersect **********************************

(define (intersect bag1 bag2)
  (define (intersect_help bag1 bag2 bag)
    (cond
      ((and (null? bag1) (null? bag2)) bag)
      ((and (null? bag1) (not (null? bag2))) bag)
      ((and (not (null? bag1)) (null? bag2)) bag)
      (else 
       (cond
          ((= (car (car bag1)) (car (car bag2)))
           (cond
             ((<=(car (cdr (car bag1))) (car (cdr (car bag2)))) (intersect_help (cdr bag1) (cdr bag2) 
                           (append bag (cons (car bag1) '()))))
             ((>(car (cdr (car bag1))) (car (cdr (car bag2)))) (intersect_help (cdr bag1) (cdr bag2) 
                           (append bag (cons (car bag2) '()))))
             )
           )
          ((< (car (car bag1)) (car (car bag2))) (intersect_help (cdr bag1) bag2 bag))
          ((> (car (car bag1)) (car (car bag2))) (intersect_help bag1 (cdr bag2) bag))
          ))
      )
    )
(intersect_help bag1 bag2 '()))

;*************************** diff ************************************

(define (diff bag1 bag2)
  (define (diff_help bag1 bag2 bag)
    (cond
      ((and (null? bag1) (null? bag2)) bag)
      ((and (null? bag1) (not (null? bag2))) bag)
      ((and (not (null? bag1)) (null? bag2)) (diff_help (cdr bag1) bag2 (append bag (cons (car bag1) '()))))
      (else 
       (cond 
         ((= (car (car bag1)) (car (car bag2)))
          (cond
            ((<(car (cdr (car bag1))) (car (cdr (car bag2)))) (diff_help (cdr bag1) (cdr bag2) bag))
            ((>(car (cdr (car bag1))) (car (cdr (car bag2)))) (diff_help (cdr bag1) (cdr bag2) 
                                                                         (append bag (cons (list (car (car bag1)) (- (car (cdr (car bag1))) (car (cdr (car bag2))))) '()))))
            (else (diff_help (cdr bag1) (cdr bag2) bag))
            )
          )
         ((< (car (car bag1)) (car (car bag2))) (diff_help (cdr bag1) bag2 (append bag (cons (car bag1) '()))))
         ((> (car (car bag1)) (car (car bag2))) (diff_help bag1 (cdr bag2) bag))
         ))
      )
    )
  (diff_help bag1 bag2 '()))

;*************************** symdiff ************************************

(define (symdiff bag1 bag2)
  (define (symdiff_help bag1 bag2 bag)
    (cond
      ((and (null? bag1) (null? bag2)) bag)
      ((and (null? bag1) (not (null? bag2))) (symdiff_help  bag1 (cdr bag2) (append bag (cons (car bag2) '()))))
      ((and (not (null? bag1)) (null? bag2)) (symdiff_help (cdr bag1) bag2 (append bag (cons (car bag1) '()))))
      (else 
       (cond 
         ((= (car (car bag1)) (car (car bag2)))
          (cond
            ((<(car (cdr (car bag1))) (car (cdr (car bag2)))) 
             (symdiff_help (cdr bag1) (cdr bag2) 
                           (append bag (cons (list (car (car bag1)) (- (car (cdr (car bag2))) (car (cdr (car bag1))))) '()))))
            ((>(car (cdr (car bag1))) (car (cdr (car bag2)))) 
             (symdiff_help (cdr bag1) (cdr bag2) 
                           (append bag (cons (list (car (car bag1)) (- (car (cdr (car bag1))) (car (cdr (car bag2))))) '()))))
            (else (symdiff_help (cdr bag1) (cdr bag2) bag))
            )
          )
         ((< (car (car bag1)) (car (car bag2))) (symdiff_help (cdr bag1) bag2 (append bag (cons (car bag1) '()))))
         ((> (car (car bag1)) (car (car bag2))) (symdiff_help bag1 (cdr bag2) (append bag (cons (car bag2) '()))))
         ))
      )
    )
(symdiff_help bag1 bag2 '()))

;************************** execution *******************************************
  
;'list2bag
;(list2bag '(5))
;(list2bag '())
;(list2bag '(5 5))
;(list2bag '(1 4 3 2 3 2 1 5 1 5 4))
;
;'countBag
;(countBag (list2bag '(1 4 3 2 3 2 1 5 1 5 4)))
;(countBag '((1 3) (2 2) (3 2) (4 2) (5 2) (6 3) (7 3)))
;
;'bag?
;(bag? '((0 1)))
;(bag? '())
;(bag? '10)
;(bag? '((5 2) (4 3)))
;(bag? '((5 1 0)))
;(bag? '((5 5) (5 2)))
;(bag? (list2bag '(1 4 3 2 3 2 1 5 1 5 4)))
;
;'union
;(union (list2bag '(1 4 3 5 6 3 4 5 2 7 6)) (list2bag '(5 6 7 8 9 4 3 2 5 6 100)))
;(union '((1 5) (3 7) (6 5) (8 9) (99 4) (333 3)) '((1 3) (2 1) (3 3) (5 1) (6 7) (99 3) (11111 1)))
;
;'intersect
;(intersect (list2bag '(1 4 3 5 6 3 4 5 2 7 6)) (list2bag '(5 6 7 8 9 4 3 2 5 6 100)))
;(intersect '((1 5) (3 7) (6 5) (8 9) (99 4) (333 3)) '((1 3) (2 1) (3 3) (5 1) (6 7) (99 3) (11111 1)))
;
;'diff
;(diff '((1 5) (2 2) (3 4) (4 8) (5 4)) '((2 3) (4 3) (6 2)))
;(diff (list2bag '(1 4 3 5 6 3 4 5 2 7 6)) (list2bag '(5 6 7 8 9 4 3 2 5 6 100)))
;
;'symdiff
;(symdiff (list2bag '(1 4 3 5 6 3 4 5 2 7 6)) (list2bag '(5 6 7 8 9 4 3 2 5 6 100)))
;(symdiff '((1 5) (3 7) (6 5) (8 9) (99 4) (333 3)) '((1 3) (2 1) (3 3) (5 1) (6 7) (99 3) (11111 1)))
;(symdiff '((1 5) (2 2) (3 4) (4 8) (5 4)) '((2 3) (4 3) (6 2)))
;
;'member
;'true
;(member? 1 3 (list2bag '(1 4 3 2 3 2 1 5 1 5 4)))
;(member? 1 3 '((1 5) (2 2) (3 4) (4 8) (5 4)))
;(member? 1 5 '((1 5) (2 2) (3 4) (4 8) (5 4)))
;(member? 4 4 (diff '((1 5) (2 2) (3 4) (4 8) (5 4)) '((2 3) (4 3) (6 2))))
;(member? 6 2 (symdiff '((1 5) (2 2) (3 4) (4 8) (5 4)) '((2 3) (4 3) (6 2))))
;(member? 3 4 '((1 5) (3 7) (6 5) (8 9) (99 4) (333 3)))
;(member? 8 9 '((1 5) (3 7) (6 5) (8 9) (99 4) (333 3)))
;'false
;(member? 5 3 (list2bag '(1 4 3 2 3 2 1 5 1 5 4)))
;(member? 2 5 '((1 5) (2 2) (3 4) (4 8) (5 4)))
;(member? 6 2 (diff '((1 5) (2 2) (3 4) (4 8) (5 4)) '((2 3) (4 3) (6 2))))
;(member? 3 10 '((1 5) (3 7) (6 5) (8 9) (99 4) (333 3)))
;(member? 999 999 '((1 5) (3 7) (6 5) (8 9) (99 4) (333 3)))
