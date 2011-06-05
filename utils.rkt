#lang plai

;;; EECS 322 Aggregate Type Utilities
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

; macro for function definitions
; enabling this value causes contracts to be enforced
(define-syntax (define-with-contract stx)
  (define enforce-contracts? #f)
  (syntax-case stx ()
    [(define-with-contract (fn args ...) fn-contract body-exprs ...)
     (if enforce-contracts?
         #'(define/contract (fn args ...) fn-contract body-exprs ...)
         #'(define (fn args ...) body-exprs ...))]))

; like set/c, but works as a predicate
(define (setof inside-pred?)
  (flat-contract
   (λ (x)
     (and (set? x)
          ((listof inside-pred?) (set->list x))))))

; like setof, but ensures that set is not empty
(define (non-empty-setof inside-pred?)
  (flat-contract
   (λ (x)
     (and ((setof inside-pred?) x)
          (> (set-count x) 0)))))

; like setof, but ensures exactly two elements
(define (pairof inside-pred?)
  (flat-contract
   (λ (x)
     (and ((setof inside-pred?) x)
          (= (set-count x) 2)))))

; given a list and an element, return boolean whether elem is in list
(define-with-contract (member? elem lst)
  (any/c list? . -> . boolean?)
  (and (not (empty? lst))
       (or (equal? elem (first lst))
           (member? elem (rest lst)))))

; given a set, return list with all elems
(define-with-contract (set->list s)
  (set? . -> . list?)
  (set-map s values))

; given a list, create set with all elems
(define-with-contract (list->set l)
  (list? . -> . set?)
  (apply set l))

; return an alphabetized version of lists, assoc lists, sets
(define-with-contract (alphabetize l)
  ((or/c (listof symbol?)
         (listof (cons/c symbol? any/c))
         (set/c symbol?))
   . -> .
   (or/c (listof symbol?)
         (listof (cons/c symbol? any/c))))
  (cond
    [(null? l) '()]
    [((listof symbol?) l) (sort l string<? #:key symbol->string)]
    [((listof (cons/c symbol? any/c)) l)
     (sort l string<? #:key (λ (l) (symbol->string (car l))))]
    [(set? l) (alphabetize (set->list l))]))

; fixes the weird behavior of the set utilities which require
; at least one argument
(define-with-contract (setlst-union setlst)
  ((listof set?) . -> . set?)
  (apply set-union (cons (set) setlst)))

; given a set and a predicate, return a filtered set
(define-with-contract (set-filter s pred)
  (set? procedure? . -> . set?)
  (list->set (filter pred (set->list s))))

; gets the set of every set combination of 2 items from a set
; outer set has size n-choose-2, inner sets have size 2
(define-with-contract (powerset pool)
  (set? . -> . (set/c set?))
  (let ([pred (cond [(set-eq? pool) eq?]
                    [(set-eqv? pool) eqv?]
                    [(set-equal? pool) equal?])]
        [set2 (cond [(set-eq? pool) (seteq)]
                    [(set-eqv? pool) (seteqv)]
                    [(set-equal? pool) (set)])])
    (set-for-each
     pool
     (λ (e1)
       (set-for-each
        pool
        (λ (e2)
          (unless (pred e1 e2)
            (set! set2 (set-add set2 (set e1 e2))))))))
    set2))

; gets every possible pair, given a first term and a set of second terms
(define-with-contract (pairs v pool)
  (any/c set? . -> . (set/c set?))
  (set-filter (powerset (set-add pool v))
              (λ (s) (set-member? s v))))

;; create a symbol by concatenating a prefix and a counter
(define-with-contract (temp-var prefix counter)
  (symbol? integer? . -> . symbol?)
  (string->symbol (string-append (symbol->string prefix)
                                 (number->string counter))))

;; create a symbol creation function
(define-with-contract (make-counter prefix)
  (symbol? . -> . (-> symbol?))
  (let ([p (symbol->string prefix)]
        [i 0])
    (λ ()
      (begin0 (string->symbol (string-append p (number->string i)))
              (set! i (+ i 1))))))

;; create a symbol creation function
(define-with-contract (make-int-counter)
  (-> (-> integer?))
  (let ([i 0])
    (λ ()
      (begin0 i (set! i (+ i 1))))))
