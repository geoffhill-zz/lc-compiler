#lang plai

;;; EECS 322 L Renaming
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))
(require (file "utils.rkt"))
(require (file "types.rkt"))

;; symbol-map abstract data type
(define symbol-map make-hash)

(define symbol-map?
  (flat-named-contract
   'symbol-map?
   (hash/c symbol? symbol? #:immutable #f #:flat? #t)))

(define-with-contract (symbol-map-extend! m k v-thunk)
  (symbol-map? symbol? (-> symbol?) . -> . symbol?)
  (or (hash-ref m k #f)
      (let ([v (v-thunk)])
        (hash-set! m k v)
        v)))

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
                  [lblmap (symbol-map)])
              (l2prog (rename-L2fn main lblfn lblmap)
                      (for/list ([fn others])
                        (rename-L2fn fn lblfn lblmap))))]))

(define-with-contract (rename-L2fn fn lblfn lblmap)
  (L2fn? (-> symbol?) symbol-map? . -> . L2fn?)
  (type-case L2fn fn
    [l2fn (stmts)
          (l2fn (let ([varfn (make-counter L2-var-prefix)]
                      [varmap (symbol-map)])
                  (for/list ([stmt stmts])
                    (rename-L2stmt stmt lblfn varfn lblmap varmap))))]))

(define-with-contract (rename-L2stmt stmt lblfn varfn lblmap varmap)
  (L2stmt? (-> symbol?) (-> symbol?) symbol-map? symbol-map? . -> . L2stmt?)
  (define (replace x)
    (cond
      [(label? x) (symbol-map-extend! lblmap x lblfn)]
      [(or (num? x) (reg? x)) x]
      [else (symbol-map-extend! varmap x varfn)]))
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
