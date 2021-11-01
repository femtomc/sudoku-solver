#lang rosette

;;
;; Utilities.
;;

(define (row-sum-g x) (/ (* x (+ 1 x)) 2))
(define (row-sum-s r)
  (define l (lambda (x y) (+ x y)))
  (foldl l 0 r)
  )

(define (list-equal? lst y)
  (andmap (lambda (x) (equal? x y))
          lst)
  )

(define (transpose xss)
  (apply map list xss)
  )

;; A fresh stream of symbolic variables.
(define (S) 
  (define-symbolic* q integer?)
  q)

;;
;; Verify that the problem is valid.
;;

(define (verify-problem seq)
  (define dim (length seq))
  (define res (map length seq))
  (and (list-equal? res dim)
       (foldl && #t (map 
                      (lambda (l)
                        (foldl && #t (map (lambda (m)
                                            (cond
                                              [(term? m) #t]
                                              [else (or (<= m dim) (>= m 1))]
                                              )
                                            ) l)
                               )
                        ) seq)
              )
       )
  )


;;
;; Compile to Rosette constraints.
;;

(define (constrain seq upper)
  (define pred (lambda (x) (not (term? x))))
  (define l (lambda (r)
              (begin
                (define no-sym (filter pred r))
                (for-each (lambda (e)
                            (cond
                              [(term? e) (begin
                                           (assert (<= e upper))
                                           (assert (>= e 1))
                                           (for-each (lambda (m)
                                                       (assert (not (equal? m e))))
                                                     no-sym)
                                           )]
                              [else #f]
                              )
                            ) r)
                )
              )
    )
  (for-each l seq)
  )

(define (solution seq)
  (define dim (length seq))
  (define s (row-sum-g dim))
  (define l (lambda (r) 
              (assert (equal? (row-sum-s r) s))
              )
    )
  (constrain seq dim)
  ;; We double count interval constraints, 
  ;; but this is not a big deal.
  (constrain (transpose seq) dim)
  (solve (map l seq))
  )

(module+ test
  (require rackunit)

  ;; Test "ground truth" row sum.
  (define m1 (row-sum-g 3))
  (check-equal? m1 6)

  ;; Test row sum.
  (define m2 (row-sum-s (list 1 2 3)))
  (check-equal? m2 6)

  ;; Defines a sudoku problem.
  (define p (list (list (S) 4 (S) (S))
                  (list 3 (S) (S) (S))
                  (list 4 1 3 (S))
                  (list (S) 3 (S) (S))
                  )
    )
  (write p)

  ;; Check if the problem is valid.
  (define s (verify-problem p))
  (check-equal? s #t)
  (define sn (solution p))
  (write sn)
  )
