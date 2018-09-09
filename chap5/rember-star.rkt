#lang planet neil/sicp

(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define rember*
  (lambda (a l)
    (cond ((null? l) '())
          ((atom? (car l))
            (cond ((eq? (car l) a) (rember* a (cdr l)))
                  (else (cons (car l) (rember* a (cdr l))))))
          (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond ((null? l) '())
          ((atom? (car l))
            (cond ((eq? (car l) old)
                    (cons old (cons new (insertR* new old (cdr l)))))
                  (else
                    (cons (car l) (insertR* new old (cdr l))))))
          (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
          ((atom? (car l))
            (cond ((eq? a (car l)) (+ 1 (occur* a (cdr l))))
                  (else (occur* a (cdr l)))))
          (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond ((null? l) '())
          ((atom? (car l))
            (cond ((eq? old (car l)) (cons new (subst* new old (cdr l))))
                  (else (cons (car l) (subst* new old (cdr l))))))
          (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond ((null? l) #f)
          ((atom? (car l))
            (cond ((eq? a (car l)) #t)
                  (else (member* a (cdr l)))))
          (else (or (member* a (car l))
                    (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond ((atom? (car l)) (car l))
          (else (leftmost (car l))))))