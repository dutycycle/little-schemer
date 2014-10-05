;; returns true iff _x_ is a atom

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; returns iff _x_ is a null list or a list of atoms

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

;; returns true if atoms _a_ is in the list of atoms _lat_

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((eq? a (car lat)) #t)
     (else (member? a (cdr lat))))))

;; removes the first occurance of the atom _a_ from the list of atoms _lat_

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
     ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

;; insert _new_ to the left of _old_ in list of atoms _lat_

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) ())
     ((eq? old (car lat)) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

;;; substitute _new_ for _old_ in list of atoms _lat_

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) ())
     ((eq? old (car lat)) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

;;; substitute _new_ for either _o1_ or _o2_, whichever appears first, in list
;;; of atoms _lat_

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) ())
     ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

;;; remove all occurances of the atom _a_ from the list of atoms _lat_

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) ())
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

;;; insert atom _new_ to the right of all occurances of atom _old_ in list of atoms _lat_

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) ())
     ((eq? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

;;; insert atom _new_ to the left of all occurances of atom _old_ in list of atoms _lat_

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) ())
     ((eq? old (car lat)) (cons new (multiinsertL new old lat)))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))


;; CHAPTER 4

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (o+ (add1 n) (sub1 m))))))

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (o- (sub1 n) (sub1 m))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup) (addtup (cdr tup)))))))

(define *
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (* n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup 1)
     (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
     ((> n m) #f)
     ((< n m) #f)
     (else #t))))

(define pow
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (* n (pow n (sub1 m)))))))


(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((= n 1) (car lat))
     (else (pick (sub1 n) (cdr lat))))))
