#lang plai

;;; EECS 322 L3->L2 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))
(require (file "types.rkt"))

; TODO: prefix all labels


;;;
;;; L3 -> L2 COMPILATION
;;;

(define arg-regs '(ecx edx eax))

;; compile an L3prog into sequence of L2 stmts
(define/contract (compile-L3prog prog)
  (L3prog? . -> . L2prog?)
  (type-case L3prog prog
    [l3prog (main others)
            (l2prog (l2fn (compile-L3expr main))
                    (map compile-L3fn others))]))

;; compile an L3fn into sequence of L2 stmts
(define/contract (compile-L3fn fn)
  (L3fn? . -> . L2fn?)
  (type-case L3fn fn
    [l3fn (lbl args e)
          (l2fn (append `(,(l2s-label lbl))
                        (for/list ([arg args] [reg arg-regs])
                          (l2s-assign arg reg))
                        (compile-L3expr e)))]))

;; compile an L3expr into sequence of L2 stmts
(define/contract (compile-L3expr e)
  (L3expr? . -> . (listof L2stmt?))
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
    [l3e-t (t) (compile-L3term t 'eax)]))

;; compile an L3 into sequence of L2 stmts
(define/contract (compile-L3term t dst)
  (L3term? L3-x? . -> . (listof L2stmt?))
  (type-case L3term t
    [l3t-biop (op v1 v2)
              (if (and (num? v1) (num? v2))
                  (case op
                    [(+) `(,(l2s-assign dst (+ v1 v2)))]
                    [(-) `(,(l2s-assign dst (- v1 v2)))]
                    [(*) `(,(l2s-assign dst (* v1 v2)))]
                    [(<) `(,(l2s-assign dst (if (< v1 v2) 1 0)))]
                    [(<=) `(,(l2s-assign dst (if (<= v1 v2) 1 0)))]
                    [(=) `(,(l2s-assign dst (if (= v1 v2) 1 0)))])
                  (case op
                    [(+) `(,(l2s-assign dst v1)
                           ,(l2s-aop dst '+= v2))]
                    [(-) `(,(l2s-assign dst v1)
                           ,(l2s-aop dst '-= v2))]
                    [(*) `(,(l2s-assign dst v1)
                           ,(l2s-aop dst '*= v2))]
                    [(<) `(,(l2s-cmp v1 '< v2))]
                    [(<=) `(,(l2s-cmp v1 '<= v2))]
                    [(=) `(,(l2s-cmp v1 '= v2))]))]
    [l3t-pred (pred v)
              (if (num? v)
                  (case pred
                    [(number?) `(,(l2s-assign dst 1))]
                    [(number?) `(,(l2s-assign dst 0))])
                  (case pred
                    [(number?) `(,(l2s-assign dst v)
                                 ,(l2s-aop '&= 1))]
                    [(a?) `(,(l2s-assign dst v)
                            ,(l2s-aop '+= 1)
                            ,(l2s-aop '&= 1))]))]
    [l3t-apply (fn args)
               (append
                (for/list ([arg args] [reg arg-regs])
                  (l2s-assign reg arg))
                `(,(l2s-call fn))
                (if (equal? dst 'eax)
                    '()
                    `(,(l2s-assign dst 'eax))))]
    [l3t-newarray (len init)
                  (append
                   `(,(l2s-alloc 'eax len init))
                   (if (equal? dst 'eax)
                       '()
                       `(,(l2s-assign dst 'eax))))]
    [l3t-newtuple (args)
                  (append
                   `(,(l2s-alloc 'eax (length args) 0))
                   (for/list ([arg args] [i (in-range 0 (length args))])
                     (l2s-memset 'eax (* 4 (+ i 1)) arg))
                   (if (equal? dst 'eax)
                       '()
                       `(,(l2s-assign dst 'eax))))]
    [l3t-aref (arr i) '(...)]
    [l3t-aset (arr i v) '(...)]
    [l3t-alen (arr) '(...)]
    [l3t-print (v) '(...)]
    [l3t-makeclj (proc vars) '(...)]
    [l3t-cljproc (clj) '(...)]
    [l3t-cljvars (clj) '(...)]
    [l3t-v (v) `(,(l2s-assign dst v))]))