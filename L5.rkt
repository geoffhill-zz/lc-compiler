#lang plai

;;; EECS 322 L5->L4 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))
(require (file "utils.rkt"))
(require (file "types.rkt"))
(require (file "input.rkt"))
(require (file "output.rkt"))

; TODO: fix discrepancy between lambdas and builtins
; TODO: fix 3-argument limit

;;;
;;; LIFTED CLOSURE AND CLOSURE MAP TYPES
;;;

(define-type LiftedClj
  [lft-clj (lbl label?)
           (args (listof L5-var?))
           (frees (listof L5-var?))
           (body L5expr?)])
(define cljmap hash)
(define cljmap?
  (flat-named-contract
   'cljmap?
   (hash/c integer? LiftedClj? #:immutable #t #:flat? #t)))

(define-with-contract (cljmap-get m i)
  (cljmap? integer? . -> . LiftedClj?)
  (or
   (hash-ref m i #f)
   (error 'cljmap "key doesn't exist")))

(define cljmap-getall hash-values)

(define-with-contract (cljmap-extend m i clj)
  (cljmap? integer? LiftedClj? . -> . cljmap?)
  (when (hash-ref m i #f)
    (error 'cljmap "key already exists"))
  (hash-set m i clj))

(define-with-contract (cljmap-lstunion lst)
  ((listof cljmap?) . -> . cljmap?)
  (foldl (λ (lhs rhs)
           (let loop ([newhash lhs]
                      [lstform (hash->list rhs)])
             (if (empty? lstform)
                 newhash
                 (let ([k (car (first lstform))]
                       [v (cdr (first lstform))])
                   (loop (cljmap-extend newhash k v) (rest lstform))))))
         (cljmap)
         lst))

;;;
;;; LETREC ELIMINATION
;;;

(define-with-contract (elim-letrec expr)
  (L5expr? . -> . L5expr?)
  (elim-letrec-traverse expr (make-counter 'recvar_)))

(define-with-contract (elim-letrec-traverse expr varfn)
  (L5expr? (-> symbol?) . -> . L5expr?)
  (type-case L5expr expr
    [l5e-lambda (args body)
                (l5e-lambda args (elim-letrec-traverse body varfn))]
    [l5e-let (id binding body)
             (l5e-let id
                      (elim-letrec-traverse binding varfn)
                      (elim-letrec-traverse body varfn))]
    [l5e-letrec (id binding body)
                (let* ([newid (varfn)]
                       [new-binding (elim-letrec-traverse binding varfn)]
                       [new-body (elim-letrec-traverse body varfn)]
                       [bound-binding (bind-var new-binding id newid)]
                       [bound-body (bind-var new-body id newid)])
                  (l5e-let
                   newid
                   (l5e-newtuple `(,(l5e-num 0)))
                   (l5e-begin (l5e-app (l5e-prim 'aset)
                                       `(,(l5e-var newid)
                                         ,(l5e-num 0)
                                         ,bound-binding))
                              bound-body)))]
    [l5e-if (test then else)
            (l5e-if (elim-letrec-traverse test varfn)
                    (elim-letrec-traverse then varfn)
                    (elim-letrec-traverse else varfn))]
    [l5e-newtuple (args)
                  (l5e-newtuple
                   (map (λ (arg) (elim-letrec-traverse arg varfn))
                        args))]
    [l5e-begin (fst snd)
               (l5e-begin
                (elim-letrec-traverse fst varfn)
                (elim-letrec-traverse snd varfn))]
    [l5e-app (fn args)
             (l5e-app
              (elim-letrec-traverse fn varfn)
              (map (λ (arg) (elim-letrec-traverse arg varfn))
                   args))]
    [l5e-prim (prim) expr]
    [l5e-var (var) expr]
    [l5e-num (num) expr]))

; replaces variables with array references
; preserves scope with lambda, let and letrec
(define-with-contract (bind-var expr oldvar arr)
  (L5expr? L5-var? L5-var? . -> . L5expr?)
  (type-case L5expr expr
    [l5e-lambda (args body)
                (l5e-lambda args
                            (if (member? oldvar args)
                                body
                                (bind-var body oldvar arr)))]
    [l5e-let (id binding body)
             (l5e-let id
                      (bind-var binding oldvar arr)
                      (if (equal? oldvar id)
                          body
                          (bind-var body oldvar arr)))]
    [l5e-letrec (id binding body)
                (l5e-letrec id
                      (bind-var binding oldvar arr)
                      (if (equal? oldvar id)
                          body
                          (bind-var body oldvar arr)))]
    [l5e-if (test then else)
            (l5e-if (bind-var test oldvar arr)
                    (bind-var then oldvar arr)
                    (bind-var else oldvar arr))]
    [l5e-newtuple (args)
                  (l5e-newtuple
                   (map (λ (arg) (bind-var arg oldvar arr))
                        args))]
    [l5e-begin (fst snd)
               (l5e-begin
                (bind-var fst oldvar arr)
                (bind-var snd oldvar arr))]
    [l5e-app (fn args)
             (l5e-app
              (bind-var fn oldvar arr)
              (map (λ (arg) (bind-var arg oldvar arr))
                   args))]
    [l5e-prim (prim) expr]
    [l5e-var (var) (if (equal? oldvar var)
                       (l5e-app (l5e-prim 'aref)
                                `(,(l5e-var arr)
                                  ,(l5e-num 0)))
                       expr)]
    [l5e-num (num) expr]))

;;;
;;; CLOSURE GENERATION
;;;

(define-with-contract (make-closures expr)
  (L5expr? . -> . cljmap?)
  (let ([counter (make-int-counter)]
        [lblfn (make-counter ':l5clj_)])
    (make-closures-traverse expr counter lblfn)))

(define-with-contract (make-closures-traverse expr counter lblfn)
  (L5expr? (-> integer?) (-> label?) . -> . cljmap?)
  (type-case L5expr expr
    [l5e-lambda (args body)
                (let* ([pos (counter)]
                       [lbl (lblfn)]
                       [frees (set-subtract (free-vars body) (list->set args))]
                       [freeslst (alphabetize frees)]
                       [newbody (let loop ([newbody body]
                                           [vs freeslst]
                                           [i 0])
                                  (if (empty? vs)
                                      newbody
                                      (loop (l5e-let
                                             (first vs)
                                             (l5e-app (l5e-prim 'aref)
                                                      `(,(l5e-var 'vars-tup)
                                                        ,(l5e-num i)))
                                             newbody)
                                            (rest vs)
                                            (+ i 1))))])
                  (cljmap-extend
                   (make-closures-traverse body counter lblfn)
                   pos
                   (lft-clj lbl
                            (cons 'vars-tup args)
                            freeslst
                            newbody)))]
    [l5e-let (id binding body)
             (begin
               (counter)
               (cljmap-lstunion
                `(,(make-closures-traverse binding counter lblfn)
                  ,(make-closures-traverse body counter lblfn))))]
    [l5e-letrec (id binding body)
                (begin
                  (counter)
                  (cljmap-lstunion
                   `(,(make-closures-traverse binding counter lblfn)
                     ,(make-closures-traverse body counter lblfn))))]
    [l5e-if (test then else)
            (begin
              (counter)
              (cljmap-lstunion
               `(,(make-closures-traverse test counter lblfn)
                 ,(make-closures-traverse then counter lblfn)
                 ,(make-closures-traverse else counter lblfn))))]
    [l5e-newtuple (args)
                  (begin
                    (counter)
                    (cljmap-lstunion
                     (map (λ (arg) (make-closures-traverse arg counter lblfn))
                          args)))]
    [l5e-begin (fst snd)
               (begin
                 (counter)
                 (cljmap-lstunion
                  `(,(make-closures-traverse fst counter lblfn)
                    ,(make-closures-traverse snd counter lblfn))))]
    [l5e-app (fn args)
             (begin
               (counter)
               (cljmap-lstunion
                (map (λ (arg) (make-closures-traverse arg counter lblfn))
                     (cons fn args))))]
    [l5e-prim (prim) (begin (counter) (cljmap))]
    [l5e-var (var) (begin (counter) (cljmap))]
    [l5e-num (num) (begin (counter) (cljmap))]))

;;;
;;; LAMBDA LIFTING
;;;

(define-with-contract (lambda-lift expr cljs)
  (L5expr? cljmap? . -> . L5expr?)
  (lambda-lift-traverse expr cljs (make-int-counter)))

(define-with-contract (lambda-lift-traverse expr cljs counter)
  (L5expr? cljmap? (-> integer?) . -> . L5expr?)
  (type-case L5expr expr
    [l5e-lambda (args body)
                (let* ([pos (counter)]
                       [clj (cljmap-get cljs pos)])
                  (type-case LiftedClj clj
                    [lft-clj (lbl args frees body)
                             (l5e-app (l5e-var 'make-closure)
                                      `(,(l5e-var lbl)
                                        ,(l5e-newtuple (map l5e-var frees))))]))]
    [l5e-let (id binding body)
             (begin
               (counter)
               (l5e-let id
                        (lambda-lift-traverse binding cljs counter)
                        (lambda-lift-traverse body cljs counter)))]
    [l5e-letrec (id binding body)
                (begin
                  (counter)
                  (l5e-letrec id
                              (lambda-lift-traverse binding cljs counter)
                              (lambda-lift-traverse body cljs counter)))]
    [l5e-if (test then else)
            (begin
              (counter)
              (l5e-if (lambda-lift-traverse test cljs counter)
                      (lambda-lift-traverse then cljs counter)
                      (lambda-lift-traverse else cljs counter)))]
    [l5e-newtuple (args)
                  (begin
                    (counter)
                    (l5e-newtuple
                     (map (λ (arg) (lambda-lift-traverse arg cljs counter))
                          args)))]
    [l5e-begin (fst snd)
               (begin
                 (counter)
                 (l5e-begin
                  (lambda-lift-traverse fst cljs counter)
                  (lambda-lift-traverse snd cljs counter)))]
    [l5e-app (fn args)
             (let ([pos (counter)]
                   [traversed-fn (lambda-lift-traverse fn cljs counter)]
                   [traversed-args
                    (map (λ (arg) (lambda-lift-traverse arg cljs counter))
                         args)])
               (if (l5e-prim? traversed-fn)
                   (l5e-app traversed-fn traversed-args)
                   (l5e-app (l5e-app (l5e-var 'closure-proc)
                                     `(,traversed-fn))
                            (cons
                             (l5e-app (l5e-var 'closure-vars)
                                      `(,traversed-fn))
                             traversed-args))))]
    [l5e-prim (prim) (begin (counter) expr)]
    [l5e-var (var) (begin (counter) expr)]
    [l5e-num (num) (begin (counter) expr)]))

;;;
;;; FREE VARIABLE SEARCHING
;;;

(define-with-contract (free-vars expr)
  (L5expr? . -> . (setof L5-var?))
  (free-vars-traverse expr (set)))

(define-with-contract (free-vars-traverse expr bound)
  (L5expr? (setof L5-var?) . -> . (setof L5-var?))
  (type-case L5expr expr
    [l5e-lambda (args body)
                (free-vars-traverse body (set-union bound (list->set args)))]
    [l5e-let (id binding body)
             (set-union (free-vars-traverse binding bound)
                        (free-vars-traverse body (set-add bound id)))]
    [l5e-letrec (id binding body)
                (set-union (free-vars-traverse binding bound)
                           (free-vars-traverse body (set-add bound id)))]
    [l5e-if (test then else)
            (set-union (free-vars-traverse test bound)
                       (free-vars-traverse then bound)
                       (free-vars-traverse else bound))]
    [l5e-newtuple (args)
                  (setlst-union
                   (map (λ (arg) (free-vars-traverse arg bound)) args))]
    [l5e-begin (fst snd)
               (set-union (free-vars-traverse fst bound)
                          (free-vars-traverse snd bound))]
    [l5e-app (fn args)
             (setlst-union
              (map (λ (arg) (free-vars-traverse arg bound))
                   (cons fn args)))]
    [l5e-prim (prim) (set)]
    [l5e-var (var)
             (if (set-member? bound var)
                 (set)
                 (set var))]
    [l5e-num (num) (set)]))

;;;
;;; L5 -> L4 COMPILATION
;;;

;; program runs through this transformation:
;;
;; (1) letrecs eliminated
;; (2) primitive closing
;; (3) cljmap built up:
;;    keys are every lambda in the function
;;    duplicate lambdas get duplicate closures TODO: this is bad
;;    vals are the corresponding LiftedCljs
;; (4) using cljmap, lambdas and apps all replaced

(define-with-contract (compile-L5expr expr)
  (L5expr? . -> . L4prog?)
  (let* ([expr-flat (elim-letrec expr)]
         [cljs (make-closures expr)]
         [others (cljmap-getall cljs)])
    (l4prog (l4mainfn (convert-L5expr (lambda-lift expr cljs)))
            (map (λ (clj)
                   (type-case LiftedClj clj
                     [lft-clj (lbl args frees body)
                              (l4fn lbl args (convert-L5expr (lambda-lift body cljs)))]))
                 others))))

(define-with-contract (convert-L5expr expr)
  (L5expr? . -> . L4expr?)
  (type-case L5expr expr
    [l5e-lambda (args body)
                (error 'L5 "should not see lambda after lambda-lifting")]
    [l5e-let (id binding body)
             (l4e-let id (convert-L5expr binding) (convert-L5expr body))]
    [l5e-letrec (id binding body)
                (error 'L5 "should not see letrec after lambda-lifting")]
    [l5e-if (test then else)
            (l4e-if (convert-L5expr test)
                    (convert-L5expr then)
                    (convert-L5expr else))]
    [l5e-newtuple (args)
                  (l4e-app (l4e-v 'new-tuple)
                           (map convert-L5expr args))]
    [l5e-begin (fst snd)
               (l4e-begin (convert-L5expr fst)
                          (convert-L5expr snd))]
    [l5e-app (fn args)
             (l4e-app (convert-L5expr fn)
                      (map convert-L5expr args))]
    [l5e-prim (prim) (l4e-v prim)]
    [l5e-var (var) (l4e-v var)]
    [l5e-num (num) (l4e-v num)]))

;;;
;;; EXTERNAL INTERFACE
;;;

(define-with-contract (main fname)
  (string? . -> . void?)
  (call-with-input-file fname main/compile-L5))

(define-with-contract (main/compile-L5 port)
  (input-port? . -> . void?)
  (pretty-write
   (format-L4prog
    (compile-L5expr
     (build-L5expr (read port))))))
