#lang plai

;;; EECS 322 Aggregate Type Utilities
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))

; macro for function definitions
; enabling this value causes contracts to be enforced
(define-syntax (define-with-contract stx)
  (define enforce-contracts? #t)
  (syntax-case stx ()
    [(define-with-contract (fn args ...) fn-contract body-exprs ...)
     (if enforce-contracts?
         #'(define/contract (fn args ...) fn-contract body-exprs ...)
         #'(define (fn args ...) body-exprs ...))]))

;; namemap type
;; immutable hash, maps variables to variables or labels to labels
;; defines constructor, contract, and label simplification
; TODO: move namemap into types.rkt encapsulating hash-ref and hash-set
(define namemap hash)
(define namemap?
  (flat-named-contract
   'namemap?
   (hash/c symbol? symbol? #:immutable #t #:flat? #t)))
(define (namemap-lbls-only nm)
  (make-immutable-hash
   (filter (not/c null?)
           (hash-map nm (λ (k v) (if (label? k) (cons k v) '()))))))


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

; giben a set and a predicate, return a filtered set
(define-with-contract (set-filter s pred)
  (set? procedure? . -> . set?)
  (list->set (filter pred (set->list s))))

; given a vectorof setof symbols, return a listof listof symbols 
; vector stays in order, but set is alphabetized
(define-with-contract (vs->ll vs)
  ((vectorof (set/c symbol?)) . -> . (listof (listof symbol?)))
  (map alphabetize
       (vector->list (vector-map (λ (s) (set->list s)) vs))))

; given a hashof symbols keyed by symbols, return a listof listof symbols
; outer list alphabetized by the key
; inner lists have 2 elements
(define-with-contract (hts->list ht)
  ((hash/c symbol? symbol?) . -> . (listof (listof symbol?)))
  (map (λ (entry) (list (car entry) (cdr entry)))
       (alphabetize (hash->list ht))))

; given a hashof setof symbols keyed by symbols, return a listof listof symbols
; outer list alphabetized by the key
; inner lists have key as first element, rest alphabetized
(define-with-contract (htss->list ht)
  ((hash/c symbol? (set/c symbol?)) . -> . (listof (listof symbol?)))
  (map (λ (entry) (cons (car entry) (alphabetize (set->list (cdr entry)))))
       (alphabetize (hash->list ht))))

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
