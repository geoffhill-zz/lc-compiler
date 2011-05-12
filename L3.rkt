#lang plai

;;; EECS 322 L3->L2 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))

; TODO: prefix all labels

;;;
;;; DATA TYPES
;;;

;; L3term types for terms
;; smallest compound types, non-recursive
(define-type L3term
  [l3t-biop (op L3-biop?) (v1 L3-v?) (v2 L3-v?)]
  [l3t-pred (pred L3-pred?) (v L3-v?)]
  [l3t-apply (fn L3-v?) (args (listof L3-v?))]
  [l3t-newarray (len L3-v?) (init L3-v?)]
  [l3t-newtuple (args (listof L3-v?))]
  [l3t-aref (arr L3-v?) (i L3-v?)]
  [l3t-aset (arr L3-v?) (i L3-v?) (v L3-v?)]
  [l3t-alen (arr L3-v?)]
  [l3t-print (a1 L3-v?)]
  [l3t-makeclj (lbl label?) (a2 L3-v?)]
  [l3t-cljproc (a1 L3-v?)]
  [l3t-cljvars (a1 L3-v?)]
  [l3t-v (v L3-v?)])

;; L3expr types for expressions
;; recurively defined
(define-type L3expr
  [l3e-let (id L3-v?)
           (binding L3term?)
           (body L3expr?)]
  [l3e-if (test L3-v?)
          (then L3expr?)
          (else L3expr?)]
  [l3e-t (t L3term?)])

;; L3prog types for functions
;; two variants, one for main function
;; and one for all other functions
(define-type L3fn
  [l3f-mainfn (e L3expr?)]
  [l3f-fn (lbl label?)
          (args (listof L3-v?))
          (e L3expr?)])

;; L3prog types for entire programs
;; composed of a main function
;; and zero or more other functions
(define-type L3prog
  [l3p (main l3f-mainfn?)
       (others (listof l3f-fn?))])

;;;
;;; TREE GENERATION
;;;

;; parse an s-expr to create an L3term
(define/contract (build-L3term src)
  (any/c . -> . L3term?)
  (match src
    [`(,(? L3-biop? op) ,(? L3-v? v1) ,(? L3-v? v2)) (l3t-biop op v1 v2)]
    [`(,(? L3-pred? pred) ,(? L3-v? v)) (l3t-pred pred v)]
    [`(,(? L3-v? fn) ,(? L3-v? args) ...) (l3t-apply fn args)]
    [`(new-array ,(? L3-v? len) ,(? L3-v? init)) (l3t-newarray len init)]
    [`(new-tuple ,(? L3-v? args) ...) (l3t-newtuple args)]
    [`(aref ,(? L3-v? arr) ,(? L3-v? i)) (l3t-aref arr i)]
    [`(aset ,(? L3-v? arr) ,(? L3-v? i) ,(? L3-v? v)) (l3t-aset arr i v)]
    [`(alen ,(? L3-v? arr)) (l3t-alen arr)]
    [`(print ,(? L3-v? v)) (l3t-print v)]
    [`(make-closure ,(? label? proc) ,(? L3-v? vars)) (l3t-makeclj proc vars)]
    [`(closure-proc ,(? L3-v? clj)) (l3t-cljproc clj)]
    [`(closure-vars ,(? L3-v? clj)) (l3t-cljvars clj)]
    [(? L3-v? v) (l3t-v v)]
    [_ (error 'L3 "no matching term for ~a" src)]))

;; parse an s-expr to create an L3expr
(define/contract (build-L3expr src)
  (any/c . -> . L3expr?)
  (match src
    [`(let ([,(? L3-x? id) ,t]) ,e) (l3e-let id (build-L3term t) (build-L3expr e))]
    [`(if ,(? L3-v? v) ,e1 ,e2) (l3e-if v (build-L3expr e1) (build-L3expr e2))]
    [_ (l3e-t (build-L3term src))]))

;; parse an s-expr to create an l3f-mainfn
(define/contract (build-l3f-mainfn src)
  (any/c . -> . l3f-mainfn?)
  (l3f-mainfn (build-L3expr src)))

;; parse an s-expr to create an l3f-fn
(define/contract (build-l3f-fn src)
  (any/c . -> . l3f-fn?)
  (match src
    [`(,(? label? lbl) (,(? L3-x? args) ...) ,e)
     (l3f-fn lbl args (build-L3expr e))]
    [_ (error 'L3 "not a well-formed function")]))

;; parse an s-expr to create an L3prog
(define/contract (build-L3prog src)
  (list? . -> . L3prog?)
  (l3p (build-l3f-mainfn (car src))
       (map build-l3f-fn (cdr src))))

;;;
;;; CODE GENERATION
;;;

;; compile an L3prog into sequence of L2 stmts
(define/contract (compile-L3prog prog)
  (L3prog? . -> . any/c)
  (type-case L3prog prog
    [l3p (main others)
         (append-map compile-L3fn (cons main others))]))

;; compile an L3fn into sequence of L2 stmts
(define/contract (compile-L3fn fn)
  (L3fn? . -> . list?)
  (type-case L3fn fn
    [l3f-mainfn (e) (compile-L3expr e)]
    [l3f-fn (lbl args e)
            (cons
             lbl
             (cons
              (for/list ([arg args] [reg '(ecx edx eax)])
                `(,arg <- ,reg))
              (compile-L3expr e)))]))

;; compile an L3expr into sequence of L2 stmts
(define/contract (compile-L3expr e)
  (L3expr? . -> . list?)
  (type-case L3expr e
    [l3e-let (id binding body)
             (append (compile-L3term binding id)
                     (compile-L3expr body))]
    [l3e-if (test then else)
             (append `(cjump ,test = 1 :then :else) ; TODO: generate labels
                     ':then
                     (compile-L3expr then)
                     ':else
                     (compile-L3expr else))]
    [l3e-t (t) `(,(compile-L3term t 'eax))]))

;; compile an L3 into sequence of L2 stmts
(define/contract (compile-L3term t dst)
  (L3term? L3-x? . -> . list?)
  (type-case L3term t
    [l3t-biop (op v1 v2)
              (case op
                [(+) '((,dst <- ,v1) (,dst += ,v2))]
                [(-) '((,dst <- ,v1) (,dst -= ,v2))]
                [(*) '((,dst <- ,v1) (,dst *= ,v2))]
                [(<) '((,dst <- ,v1 < ,v2))]
                [(<=) '((,dst <- ,v1 <= ,v2))]
                [(=) '((,dst <- ,v1 = ,v2))])]
    [l3t-pred (pred v)
              (case pred
                [(number?) '((,dst <- ,v) (,dst &= 1))]
                [(a?) '((,dst <- ,v) (,dst += 1) (,dst &= 1))])]
    [l3t-apply (fn args) '(...)]
    [l3t-newarray (len init) '(...)]
    [l3t-newtuple (args) '(...)]
    [l3t-aref (arr i) '(...)]
    [l3t-aset (arr i v) '(...)]
    [l3t-alen (arr) '(...)]
    [l3t-print (v) '(...)]
    [l3t-makeclj (proc vars) '(...)]
    [l3t-cljproc (clj) '(...)]
    [l3t-cljvars (clj) '(...)]
    [l3t-v (v) '(...)]))