#lang plai

;;; EECS 322 L5->L4 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))
(require (file "utils.rkt"))
(require (file "types.rkt"))
(require (file "input.rkt"))
(require (file "output.rkt"))
(require (file "renaming.rkt"))

;;;
;;; OPTIMIZATION
;;;

(define-with-contract (optimize expr)
  (L5expr? . -> . L5expr?)
  (optimize-iter (clean expr) 2 6))

(define-with-contract (optimize-iter expr num-inline num-clean)
  (L5expr? integer? integer? . -> . L5expr?)
  (if (zero? num-inline)
      expr
      (optimize-iter (clean-iter (inline expr) num-clean)
                     (- num-inline 1)
                     num-clean)))

(define-with-contract (clean-iter expr num-clean)
  (L5expr? integer? . -> . L5expr?)
  (if (zero? num-clean)
      expr
      (clean-iter (clean expr) (- num-clean 1))))

;;;
;;; INLINING
;;;

(define-with-contract (inline expr)
  (L5expr? . -> . L5expr?)
  (inline-fns expr (funcmap)))

(define-type Func
  [func (args (listof L5-var?))
        (body L5expr?)])

(define funcmap hash)

(define funcmap?
  (flat-named-contract
   'funcmap?
   (hash/c L5-var? Func? #:immutable #t #:flat? #t)))

(define-with-contract (funcmap-get m v)
  (funcmap? L5-var? . -> . (or/c Func? false?))
  (hash-ref m v #f))

(define-with-contract (funcmap-extend m v expr)
  (funcmap? L5-var? Func? . -> . funcmap?)
  (hash-set m v expr))

(define-with-contract (funcmap-remove m v)
  (funcmap? L5-var? . -> . funcmap?)
  (hash-remove m v))

(define-with-contract (inline-fns expr fns)
  (L5expr? funcmap? . -> . L5expr?)
  (type-case L5expr expr
    [l5e-lambda (args body)
                (l5e-lambda args (inline-fns body fns))]
    [l5e-let (id binding body)
             ;; sometimes lets go away
             ;; id is always removed or overriden in funcmap
             (if (and (l5e-lambda? binding)
                      (set-empty? (free-vars binding)))
                 (inline-fns body (funcmap-extend fns id (func (l5e-lambda-args binding)
                                                               (l5e-lambda-body binding))))
                 (l5e-let id
                          (inline-fns binding fns)
                          (inline-fns body (funcmap-remove fns id))))]
    [l5e-letrec (id binding body)
             ;; letrecs never go away
             ;; id is always removed or overriden in funcmap
                (let ([new-fn (and (l5e-lambda? binding)
                                   (set-empty? (set-remove (free-vars binding) id))
                                   (func (l5e-lambda-args binding)
                                         (l5e-lambda-body binding)))])
                  (l5e-letrec id
                              (inline-fns binding (if new-fn (funcmap-extend fns id new-fn) fns))
                              (inline-fns body (if new-fn
                                                   (funcmap-extend fns id new-fn)
                                                   (funcmap-remove fns id)))))]
    [l5e-if (test then else)
            (l5e-if (inline-fns test fns)
                    (inline-fns then fns)
                    (inline-fns else fns))]
    [l5e-newtuple (args)
                  (l5e-newtuple (for/list ([arg args])
                                  (inline-fns arg fns)))]
    [l5e-begin (fst snd)
               (l5e-begin (inline-fns fst fns)
                          (inline-fns snd fns))]
    [l5e-app (fn args)
             (let ([new-args (for/list ([arg args])
                               (inline-fns arg fns))]
                   [func (and (l5e-var? fn)
                              (funcmap-get fns (l5e-var-var fn)))])
               (if func
                   (add-lets (func-body func)
                             (func-args func)
                             new-args)
                   (l5e-app (inline-fns fn fns) new-args)))]
    [l5e-prim (prim) expr]
    [l5e-var (var) 
             (let ([func (funcmap-get fns var)])
               (if func
                   (l5e-lambda (func-args func) (func-body func))
                   expr))]
    [l5e-num (num) expr]))

(define-with-contract (add-lets expr ids args)
  (L5expr? (listof L5-var?) (listof L5expr?) . -> . L5expr?)
  (cond
    [(and (null? ids) (null? args)) expr]
    [(or (null? ids) (null? args))
     (error 'L5 "add-lets given two different length lists")]
    [else
     (add-lets (l5e-let (first ids) (first args) expr)
               (rest ids)
               (rest args))]))

;;;
;;; CLEANING
;;;

(define-with-contract (clean expr)
  (L5expr? . -> . L5expr?)
  (clean-lets expr (substmap)))

(define substmap hash)

(define substmap?
  (flat-named-contract
   'substmap?
   (hash/c L5-var? L5expr? #:immutable #t #:flat? #t)))

(define-with-contract (substmap-get m k)
  (substmap? L5-var? . -> . (or/c L5expr? false?))
  (hash-ref m k #f))

(define-with-contract (substmap-getrec m k)
  (substmap? L5-var? . -> . (or/c L5expr? false?))
  (let ([v (substmap-get m k)])
    (cond
      [(false? v) (l5e-var k)]
      [(l5e-num? v) v]
      [(l5e-var? v)
       (if (equal? k (l5e-var-var v))
           (l5e-var k)
           (substmap-getrec m (l5e-var-var v)))]
      [else #f])))

(define-with-contract (substmap-extend m v expr)
  (substmap? L5-var? L5expr? . -> . substmap?)
  (hash-set m v expr))

(define-with-contract (substmap-remove m v)
  (substmap? L5-var? . -> . substmap?)
  (hash-remove m v))

(define-with-contract (clean-lets expr smap)
  (L5expr? substmap? . -> . L5expr?)
  (type-case L5expr expr
    [l5e-lambda (args body)
                (l5e-lambda args (clean-lets body smap))]
    [l5e-let (id binding body)
             (if (or (l5e-var? binding) (l5e-num? binding))
                 (clean-lets body (substmap-extend smap id binding))
                 (l5e-let id
                          (clean-lets binding smap)
                          (clean-lets body (substmap-remove smap id))))]
    [l5e-letrec (id binding body)
                (l5e-letrec id
                            (clean-lets binding smap)
                            (clean-lets body smap))]
    [l5e-if (test then else)
            (let ([new-test (clean-lets test smap)]
                  [new-then (clean-lets then smap)]
                  [new-else (clean-lets else smap)])
              (if (l5e-num? new-test)
                  (if (= (l5e-num-num new-test) 0)
                      new-else
                      new-then)
                  (l5e-if new-test new-then new-else)))]
    [l5e-newtuple (args)
                  (l5e-newtuple (for/list ([arg args])
                                  (clean-lets arg smap)))]
    [l5e-begin (fst snd)
               (l5e-begin (clean-lets fst smap)
                          (clean-lets snd smap))]
    [l5e-app (fn args)
             (let ([new-fn (clean-lets fn smap)]
                   [new-args (for/list ([arg args])
                               (clean-lets arg smap))])
               (if (and (l5e-prim? fn)
                        (= (length args) 2)
                        (andmap l5e-num? args))
                   (let ([fst (l5e-num-num (first args))]
                         [snd (l5e-num-num (second args))])
                     (case (l5e-prim-prim fn)
                       [(+) (l5e-num (+ fst snd))]
                       [(-) (l5e-num (- fst snd))]
                       [(*) (l5e-num (* fst snd))]
                       [else (l5e-app new-fn new-args)]))
                   (l5e-app new-fn new-args)))]
    [l5e-prim (prim) expr]
    [l5e-var (var) (or (substmap-getrec smap var) expr)]
    [l5e-num (num) expr]))

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
                   (l5e-newtuple (list (l5e-num 0)))
                   (l5e-begin (l5e-app (l5e-prim 'aset)
                                       (list (l5e-var newid)
                                             (l5e-num 0)
                                             bound-binding))
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
;;; CLOSURE GENERATION AND LAMBDA LIFTING
;;;

(define freevar-tuple-name 'frees)
(define multarg-tuple-name 'extra)
(define empty-freevar-tuple-subst (l5e-num 0))

(define-type Closure
  [closure (lbl label?)
           (args (listof (and/c L5-var? L4-v?)))
           (body L5expr?)])

(define cljmap make-hash)

(define cljmap?
  (flat-named-contract
   'cljmap?
   (hash/c label? Closure? #:immutable #f #:flat? #t)))

(define-with-contract (cljmap-get m i)
  (cljmap? label? . -> . Closure?)
  (or
   (hash-ref m i #f)
   (error 'cljmap "key doesn't exist")))

(define-with-contract (cljmap-getall m)
  (cljmap? . -> . (listof Closure?))
  (map cdr (alphabetize (hash->list m))))

(define-with-contract (cljmap-set! m i clj)
  (cljmap? label? Closure? . -> . void?)
  (hash-set! m i clj)
  (void))

(define-with-contract (cljmap-lstunion lst)
  ((listof cljmap?) . -> . cljmap?)
  (let ([new-map (cljmap)])
    (for ([old-map lst])
      (hash-for-each old-map (λ (k v) (cljmap-set! new-map k v))))))

(define-with-contract (bif-lbl bif)
  (L5-builtin? . -> . label?)
  (case bif
    [(+) ':l5bif_plus]
    [(-) ':l5bif_minus]
    [(*) ':l5bif_times]
    [(<) ':l5bif_less]
    [(<=) ':l5bif_leq]
    [(=) ':l5bif_eq]
    [(number?) ':l5bif_number]
    [(a?) ':l5bif_a]
    [(print) ':l5bif_print]
    [(new-array) ':l5bif_newarray]
    [(aref) ':l5bif_aref]
    [(aset) ':l5bif_aset]
    [(alen) ':l5bif_alen]))

(define-with-contract (bif-arity bif)
  (L5-builtin? . -> . integer?)
  (case bif
    [(number? a? print alen) 1]
    [(+ - * < <= = new-array aref) 2]
    [(aset) 3]))

(define-with-contract (bif-clj bif)
  (L5-builtin? . -> . Closure?)
  (let* ([lbl (bif-lbl bif)]
         [arity (bif-arity bif)]
         [varfn (make-counter 'arg)]
         [args (for/list ([i (in-range arity)]) (varfn))]
         [body (l5e-app (l5e-prim bif) (map l5e-var args))])
    (if (arity . <= . 2)
        (closure lbl `(,freevar-tuple-name ,@args) body)
        (closure lbl `(,freevar-tuple-name ,multarg-tuple-name)
                 (extract-tuple body args multarg-tuple-name)))))

(define-with-contract (lambda-lift expr)
  (L5expr? . -> . (values L5expr? (listof Closure?)))
  (let ([cljs (cljmap)]
        [lblfn (make-counter ':l5clj_)])
    (values (lambda-lift-traverse expr cljs lblfn)
            (cljmap-getall cljs))))

(define-with-contract (extract-tuple expr ids tuple-name)
  (L5expr? (listof symbol?) symbol? . -> . L5expr?)
  (let loop ([expr expr]
             [ids ids]
             [i 0])
    (if (empty? ids)
        expr
        (let ([new-expr (l5e-let
                         (first ids)
                         (l5e-app (l5e-prim 'aref)
                                  `(,(l5e-var tuple-name)
                                    ,(l5e-num i)))
                         expr)]
              [new-ids (rest ids)]
              [new-i (+ i 1)])
          (loop new-expr new-ids new-i)))))

(define-with-contract (lambda-lift-traverse expr cljs lblfn)
  (L5expr? cljmap? (-> label?) . -> . L5expr?)
  (type-case L5expr expr
    [l5e-lambda (args body)
                (let* ([new-body (lambda-lift-traverse body cljs lblfn)]
                       [lbl (lblfn)]
                       [free-set (set-subtract (free-vars body) (list->set args))]
                       [frees (alphabetize free-set)]
                       [body-with-frees
                        (extract-tuple new-body frees freevar-tuple-name)]
                       [body-with-multarg
                        (if ((length args) . <= . 2)
                            body-with-frees
                            (extract-tuple body-with-frees args multarg-tuple-name))])
                  (cljmap-set! cljs
                               lbl
                               (closure lbl
                                        (if ((length args) . <= . 2)
                                            `(,freevar-tuple-name ,@args)
                                            `(,freevar-tuple-name ,multarg-tuple-name))
                                        body-with-multarg))
                  (l5e-app (l5e-var 'make-closure)
                           `(,(l5e-var lbl)
                             ,(if (zero? (length frees))
                                  empty-freevar-tuple-subst
                                  (l5e-newtuple (map l5e-var frees))))))]
    [l5e-let (id binding body)
             (l5e-let id
                      (lambda-lift-traverse binding cljs lblfn)
                      (lambda-lift-traverse body cljs lblfn))]
    [l5e-letrec (id binding body)
                (error 'L5 "letrec not eliminated")]
    [l5e-if (test then else)
            (l5e-if (lambda-lift-traverse test cljs lblfn)
                    (lambda-lift-traverse then cljs lblfn)
                    (lambda-lift-traverse else cljs lblfn))]
    [l5e-newtuple (args)
                  (l5e-newtuple (map (λ (arg) (lambda-lift-traverse arg cljs lblfn)) args))]
    [l5e-begin (fst snd)
               (l5e-begin (lambda-lift-traverse fst cljs lblfn)
                          (lambda-lift-traverse snd cljs lblfn))]
    [l5e-app (fn args)
             (if (l5e-prim? fn)
                 (l5e-app fn (map (λ (arg) (lambda-lift-traverse arg cljs lblfn)) args))
                 (let ([new-fn (lambda-lift-traverse fn cljs lblfn)]
                       [new-args (map (λ (arg) (lambda-lift-traverse arg cljs lblfn)) args)])
                   (l5e-app (l5e-app (l5e-var 'closure-proc) (list new-fn))
                            `(,(l5e-app (l5e-var 'closure-vars) (list new-fn))
                              ,@(if ((length new-args) . <= . 2)
                                    new-args
                                    (list (l5e-newtuple new-args)))))))]
    [l5e-prim (prim)
              (begin
                (cljmap-set! cljs
                             (bif-lbl prim)
                             (bif-clj prim))
                (l5e-app (l5e-var 'make-closure)
                         `(,(l5e-var (bif-lbl prim))
                           ,empty-freevar-tuple-subst)))]
    [l5e-var (var) expr]
    [l5e-num (num) expr]))

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

(define-with-contract (compile-L5expr expr)
  (L5expr? . -> . L4prog?)
  (compile-L5expr-unoptim
   (optimize (rename-L5expr expr))))

(define-with-contract (compile-L5expr-unoptim expr)
  (L5expr? . -> . L5expr?)
  (let-values ([(main cljs) (lambda-lift (elim-letrec (rename-L5expr expr)))])
    (l4prog (l4mainfn (convert-L5expr main))
            (map (λ (clj)
                   (type-case Closure clj
                     [closure (lbl args body)
                              (l4fn lbl args (convert-L5expr body))]))
                 cljs))))


(define-with-contract (convert-L5expr expr)
  (L5expr? . -> . L4expr?)
  (type-case L5expr expr
    [l5e-lambda (args body)
                (error 'L5 "lambda not lifted")]
    [l5e-let (id binding body)
             (l4e-let id (convert-L5expr binding) (convert-L5expr body))]
    [l5e-letrec (id binding body)
                (error 'L5 "letrec not eliminated")]
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
