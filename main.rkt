#lang rosette

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
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

(define (verify-problem-length seq)
  (define dim (length seq))
  (define res (map length seq))
  (list-equal? res dim)
  )

(define (S) 
  (define-symbolic* q integer?)
  q)

(define (soln seq)
  (define dim (length seq))
  (define s (row-sum-g dim))
  (define l (lambda (r) 
              (assert (equal? (row-sum-s r) s))
              )
    )
  (map l seq)
  )

(module+ test
  (require rackunit))

(module+ test
  ;; Test "ground truth" row sum.
  (define m1 (row-sum-g 3))
  (check-equal? m1 6)

  ;; Test row sum.
  (define m2 (row-sum-s (list 1 2 3)))
  (check-equal? m2 6)

  ;; Defines a sudoku problem.
  (define p (list (list 1 (S) 3)
                  (list (S) 1 (S))
                  (list 2 (S) (S))
                  )
    )

  ;; Check if the problem is valid.
  (define s (verify-problem-length p))
  (check-equal? s #t)
  (define sn (solve (soln p)))
  (write sn)
  )
