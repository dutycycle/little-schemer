(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((eq? a (car lat)) #t)
     (else (member? a (cdr lat))))))

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) ())
     ((eq? a (car lat)) (cdr lat))
     (else (cons (car lat) (rember a (cdr lat)))))))

;; return a list of all the first s-expressions in a list of lists _l_

(define firsts
  (lambda (l)
  (cond
   ((null? l) ())
   (else (cons (car (car l)) (firsts (cdr l)))))))

;; insert _new_ to the right of _old_ in list of atoms _lat_

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) ())
     ((eq? old (car lat)) (cons (cons (car lat) (cons new ()) (cdr lat)))
     (else (cons (car lat) (insertR new old (cdr lat))))))))
