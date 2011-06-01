#lang plai

;;; EECS 322 L Renaming
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))
(require (file "utils.rkt"))
(require (file "types.rkt"))

;;; where possible, the renaming mechanism clears up any
;;; instances of variable shadowing due to scope

;; TODO: process function labels first in L2 like L3-L4

;; dictionary abstract data type
(define dict make-hash)
(define dict?
  (flat-named-contract
   'dict?
   (hash/c symbol? symbol? #:immutable #f #:flat? #t)))

;; dictionary extension, for same-scope renaming
(define-with-contract (dict-extend! m k v-thunk)
  (dict? symbol? (-> symbol?) . -> . symbol?)
  (or (hash-ref m k #f)
      (let ([v (v-thunk)])
        (hash-set! m k v)
        v)))

;; dictionary overriding, for inner-scope renaming
(define-with-contract (dict-override! m k v-thunk)
  (dict? symbol? (-> symbol?) . -> . symbol?)
  (let ([v (v-thunk)])
    (hash-set! m k v)
    v))

;;;
;;; L2 RENAMING
;;;

(define L2-lbl-prefix ':L)
(define L2-var-prefix 'v)

(define-with-contract (rename-L2prog prog)
  (L2prog? . -> . L2prog?)
  (type-case L2prog prog
    [l2prog (main others)
            (let ([lblfn (make-counter L2-lbl-prefix)]
                  [lblmap (dict)])
              (l2prog (rename-L2fn main lblfn lblmap)
                      (for/list ([fn others])
                        (rename-L2fn fn lblfn lblmap))))]))

(define-with-contract (rename-L2fn fn lblfn lblmap)
  (L2fn? (-> symbol?) dict? . -> . L2fn?)
  (type-case L2fn fn
    [l2fn (stmts)
          (l2fn (let ([varfn (make-counter L2-var-prefix)]
                      [varmap (dict)])
                  (for/list ([stmt stmts])
                    (rename-L2stmt stmt lblfn varfn lblmap varmap))))]))

(define-with-contract (rename-L2stmt stmt lblfn varfn lblmap varmap)
  (L2stmt? (-> symbol?) (-> symbol?) dict? dict? . -> . L2stmt?)
  (define (replace x)
    (cond
      [(label? x) (dict-extend! lblmap x lblfn)]
      [(or (num? x) (reg? x)) x]
      [else (dict-extend! varmap x varfn)]))
  (type-case L2stmt stmt
    [l2s-assign (lhs rhs)
                (l2s-assign (replace lhs) (replace rhs))]
    [l2s-memget (lhs base offset)
                (l2s-memget (replace lhs) (replace base) offset)]
    [l2s-memset (base offset rhs)
                (l2s-memset (replace base) offset (replace rhs))]
    [l2s-aop (lhs op rhs)
             (l2s-aop (replace lhs) op (replace rhs))]
    [l2s-sop (lhs op rhs)
             (l2s-sop (replace lhs) op (replace rhs))]
    [l2s-cmp (lhs c1 op c2)
             (l2s-cmp (replace lhs) (replace c1) op (replace c2))]
    [l2s-label (lbl)
               (l2s-label (replace lbl))]
    [l2s-goto (lbl)
              (l2s-goto (replace lbl))]
    [l2s-cjump (c1 op c2 lbl1 lbl2)
               (l2s-cjump (replace c1) op (replace c2) (replace lbl1) (replace lbl2))]
    [l2s-call (dst) (l2s-call (replace dst))]
    [l2s-tcall (dst) (l2s-tcall (replace dst))]
    [l2s-return () (l2s-return)]
    [l2s-print (lhs arg1)
               (l2s-print (replace lhs) (replace arg1))]
    [l2s-alloc (lhs arg1 arg2)
               (l2s-alloc (replace lhs) (replace arg1) (replace arg2))]
    [l2s-arrayerr (lhs arg1 arg2)
                  (l2s-arrayerr (replace lhs) (replace arg1) (replace arg2))]))


;;;
;;; L3 RENAMING
;;;

(define L3-lbl-prefix ':L)
(define L3-var-prefix 'v)

(define-with-contract (rename-L3prog prog)
  (L3prog? . -> . L3prog?)
  (type-case L3prog prog
    [l3prog (main others)
            (let ([lblfn (make-counter L3-lbl-prefix)]
                  [lblmap (dict)])
              (for/list ([fn others])
                (dict-extend! lblmap (l3fn-lbl fn) lblfn))
              (l3prog (rename-L3fn main lblfn lblmap)
                      (for/list ([fn others])
                        (rename-L3fn fn lblfn lblmap))))]))

(define-with-contract (rename-L3fn fn lblfn lblmap)
  (L3fn? (-> symbol?) dict? . -> . L3fn?)
  (let ([varfn (make-counter L3-var-prefix)]
        [varmap (dict)])
    (type-case L3fn fn
      [l3mainfn (body)
                (l3mainfn (rename-L3expr body lblfn varfn lblmap varmap))]
      [l3fn (lbl args body)
            (l3fn (dict-extend! lblmap lbl lblfn)
                  (for/list ([arg args])
                    (dict-extend! varmap arg varfn))
                  (rename-L3expr body lblfn varfn lblmap varmap))])))

(define-with-contract (rename-L3expr expr lblfn varfn lblmap varmap)
  (L3expr? (-> symbol?) (-> symbol?) dict? dict? . -> . L3expr?)
  (define (replace v)
    (rename-L3v v lblfn varfn lblmap varmap))
  (type-case L3expr expr
    [l3e-let (id binding body)
             (let ([new-binding (rename-L3term binding lblfn varfn lblmap varmap)]
                   [new-id (dict-override! varmap id varfn)]
                   [new-body (rename-L3expr body lblfn varfn lblmap varmap)])
               (l3e-let new-id new-binding new-body))]
    [l3e-if (test then else)
            (let ([new-test (replace test)]
                  [new-then (rename-L3expr then lblfn varfn lblmap varmap)]
                  [new-else (rename-L3expr else lblfn varfn lblmap varmap)])
              (l3e-if new-test new-then new-else))]
    [l3e-t (t)
           (l3e-t (rename-L3term t lblfn varfn lblmap varmap))]))

(define-with-contract (rename-L3term term lblfn varfn lblmap varmap)
  (L3term? (-> symbol?) (-> symbol?) dict? dict? . -> . L3term?)
  (define (replace v)
    (rename-L3v v lblfn varfn lblmap varmap))
  (type-case L3term term
    [l3t-biop (op v1 v2) (l3t-biop op (replace v1) (replace v2))]
    [l3t-pred (pred v) (l3t-pred pred (replace v))]
    [l3t-apply (fn args) (l3t-apply (replace fn) (for/list ([arg args])
                                                   (replace arg)))]
    [l3t-newarray (len init) (l3t-newarray (replace len) (replace init))]
    [l3t-newtuple (args) (l3t-newtuple (for/list ([arg args])
                                         (replace arg)))]
    [l3t-aref (arr i) (l3t-aref (replace arr) (replace i))]
    [l3t-aset (arr i v) (l3t-aset (replace arr) (replace i) (replace v))]
    [l3t-alen (arr) (l3t-alen (replace arr))]
    [l3t-print (v) (l3t-print (replace v))]
    [l3t-makeclj (proc vars) (l3t-makeclj (replace proc) (replace vars))]
    [l3t-cljproc (clj) (l3t-cljproc (replace clj))]
    [l3t-cljvars (clj) (l3t-cljvars (replace clj))]
    [l3t-v (v) (l3t-v (replace v))]))

(define-with-contract (rename-L3v v lblfn varfn lblmap varmap)
  (L3-v? (-> symbol?) (-> symbol?) dict? dict? . -> . L3-v?)
  (cond
    [(label? v) (dict-extend! lblmap v lblfn)]
    [(num? v) v]
    [else (dict-extend! varmap v varfn)]))

;;;
;;; L4 RENAMING
;;;



(define L4-lbl-prefix ':L)
(define L4-var-prefix 'v)

(define-with-contract (rename-L4prog prog)
  (L4prog? . -> . L4prog?)
  (type-case L4prog prog
    [l4prog (main others)
            (let ([lblfn (make-counter L4-lbl-prefix)]
                  [lblmap (dict)])
              (for/list ([fn others])
                (dict-extend! lblmap (l4fn-lbl fn) lblfn))
              (l4prog (rename-L4fn main lblfn lblmap)
                      (for/list ([fn others])
                        (rename-L4fn fn lblfn lblmap))))]))

(define-with-contract (rename-L4fn fn lblfn lblmap)
  (L4fn? (-> symbol?) dict? . -> . L4fn?)
  (let ([varfn (make-counter L4-var-prefix)]
        [varmap (dict)])
    (type-case L4fn fn
      [l4mainfn (body)
                (l4mainfn (rename-L4expr body lblfn varfn lblmap varmap))]
      [l4fn (lbl args body)
            (l4fn (dict-extend! lblmap lbl lblfn)
                  (for/list ([arg args])
                    (dict-extend! varmap arg varfn))
                  (rename-L4expr body lblfn varfn lblmap varmap))])))

(define-with-contract (rename-L4expr expr lblfn varfn lblmap varmap)
  (L4expr? (-> symbol?) (-> symbol?) dict? dict? . -> . L4expr?)
  (type-case L4expr expr
    [l4e-let (id binding body)
             (let ([new-binding (rename-L4expr binding lblfn varfn lblmap varmap)]
                   [new-id (dict-override! varmap id varfn)]
                   [new-body (rename-L4expr body lblfn varfn lblmap varmap)])
               (l4e-let new-id new-binding new-body))]
    [l4e-if (test then else)
            (let ([new-test (rename-L4expr test lblfn varfn lblmap varmap)]
                  [new-then (rename-L4expr then lblfn varfn lblmap varmap)]
                  [new-else (rename-L4expr else lblfn varfn lblmap varmap)])
              (l4e-if new-test new-then new-else))]
    [l4e-begin (fst snd)
            (let ([new-fst (rename-L4expr fst lblfn varfn lblmap varmap)]
                  [new-snd (rename-L4expr snd lblfn varfn lblmap varmap)])
              (l4e-begin new-fst new-snd))]
    [l4e-app (fn args)
            (let ([new-fn (rename-L4expr fn lblfn varfn lblmap varmap)]
                  [new-args (for/list ([arg args])
                              (rename-L4expr arg lblfn varfn lblmap varmap))])
              (l4e-app new-fn new-args))]
    [l4e-v (v)
           (l4e-v (cond
                    [(label? v) (dict-extend! lblmap v lblfn)]
                    [(or (num? v) (L4-builtin? v)) v]
                    [else (dict-extend! varmap v varfn)]))]))
