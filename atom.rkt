#lang planet neil/sicp

(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))