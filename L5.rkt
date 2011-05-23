#lang plai

;;; EECS 322 L5->L4 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))
(require (file "utils.rkt"))
(require (file "types.rkt"))
(require (file "input.rkt"))
(require (file "output.rkt"))

;;;
;;; L5 -> L4 COMPILATION
;;;

;; program runs through this transformation:
;;
;; (1) cljmap built up:
;;    keys are every lambda in the function
;;    duplicate lambdas get duplicate closures <- THIS IS BAD
;;    vals are the corresponding LiftedCljs
;; (2) using cljmap, lambdas and apps all replaced

(define gen-new-l5-cljlbl (make-counter ':l5_clj_))

(define-type LiftedClj
  [lft-clj (lbl label?)
           (args (listof L5-var?))
           (body L5expr?)])
(define cljmap hash)
(define cljmap?
  (flat-named-contract
   'cljmap?
   (hash/c L5expr? LiftedClj? #:immutable #t #:flat? #t)))

(define-with-contract (compile-L5expr expr)
  (L5expr? . -> . L4prog?)
  (let* ([cljs (make-closures expr)]
         [main (lambda-lift expr cljs)]
         [others (hash-values cljs)])
    (l4prog (l4mainfn (convert-L5expr main))
            (map (λ (clj)
                   (type-case LiftedClj clj
                     [lft-clj (lbl args body)
                              (l4fn lbl args (convert-L5expr body))]))
                 others))))

(define-with-contract (convert-L5expr expr)
  (L5expr? . -> . L4expr?)
  ; TODO: stricten contract to only allow lambda-lifted L5exprs
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

(define-with-contract (make-closures expr)
  (L5expr? . -> . cljmap?)
  (type-case L5expr expr
    [l5e-lambda (args body) (cljmap)]
    [l5e-let (id binding body) (cljmap)]
    [l5e-letrec (id binding body) (cljmap)]
    [l5e-if (test then else) (cljmap)]
    [l5e-newtuple (args) (cljmap)]
    [l5e-begin (fst snd) (cljmap)]
    [l5e-app (fn args) (cljmap)]
    [l5e-prim (prim) (cljmap)]
    [l5e-var (var) (cljmap)]
    [l5e-num (num) (cljmap)]))

(define-with-contract (lambda-lift expr cljs)
  (L5expr? cljmap? . -> . L5expr?)
  (type-case L5expr expr
    [l5e-lambda (args body)
                (let ([newlbl (gen-new-l5-cljlbl)]
                      [vs (free-vars body)])
                  expr)]
    [l5e-let (id binding body) expr]
    [l5e-letrec (id binding body) expr]
    [l5e-if (test then else) expr]
    [l5e-newtuple (args) expr]
    [l5e-begin (fst snd) expr]
    [l5e-app (fn args) expr]
    [l5e-prim (prim) expr]
    [l5e-var (var) expr]
    [l5e-num (num) expr]))

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
;;; EXTERNAL INTERFACE
;;;

#|
(define-with-contract (main fname)
  (string? . -> . void?)
  (call-with-input-file fname main/compile-L5))

(define-with-contract (main/compile-L5 port)
  (input-port? . -> . void?)
  (pretty-write
   (format-L4prog
    (compile-L5expr
     (build-L5expr (read port))))))
|#
