#lang plai

;;; EECS 322 L3->L2 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))
(require (file "utils.rkt"))
(require (file "types.rkt"))
(require (file "input.rkt"))
(require (file "output.rkt"))
(require (file "renaming.rkt"))

;; TODO: move counter closures out of global for testing

;;;
;;; L3 -> L2 COMPILATION
;;;

(define arg-regs '(ecx edx eax))
(define L3-gvar-prefix 'g)
(define L3-glbl-prefix ':h)
(define L3-endlbl ':endofmain)

(define-type Context
  [ctxt-mainfn]
  [ctxt-fn])

;; compile an L3prog into an L2prog
(define-with-contract (compile-L3prog prog)
  (L3prog? . -> . L2prog?)
  (let ([renamed-prog (rename-L3prog prog)]
        [varfn (make-counter L3-gvar-prefix)]
        [lblfn (make-counter L3-glbl-prefix)])
    (type-case L3prog renamed-prog
      [l3prog (main others)
              (l2prog (compile-L3fn main varfn lblfn)
                      (map (λ (fn) (compile-L3fn fn varfn lblfn)) others))])))

;; compile an L3fn into an L2fn
(define-with-contract (compile-L3fn fn varfn lblfn)
  (L3fn? (-> symbol?) (-> symbol?) . -> . L2fn?)
  (type-case L3fn fn
    [l3mainfn (body)
              (l2mainfn (optimize-endjmp
                         `(,@(compile-L3expr body (ctxt-mainfn) varfn lblfn)
                           ,(l2s-label L3-endlbl))))]
    [l3fn (lbl args body)
          (l2fn lbl
                `(,@(for/list ([arg args] [reg arg-regs])
                      (l2s-assign arg reg))
                  ,@(compile-L3expr body (ctxt-fn) varfn lblfn)))]))

;; compile an L3expr into a list of L2stmts
;; main? is #t when currently processing main
;; end? is #t when main must jump to end to return
(define-with-contract (compile-L3expr e ctxt varfn lblfn)
  (L3expr? Context? (-> symbol?) (-> symbol?) . -> . (listof L2stmt?))
  (type-case L3expr e
    [l3e-let (id binding body)
             `(,@(compile-L3term binding id varfn lblfn)
               ,@(compile-L3expr body ctxt varfn lblfn))]
    [l3e-if (test then else)
            (let ([thenlbl (lblfn)]
                  [elselbl (lblfn)])
              `(,(l2s-cjump (encode test) '= (encode 0) elselbl thenlbl)
                ,(l2s-label thenlbl)
                ,@(compile-L3expr then ctxt varfn lblfn)
                ,(l2s-label elselbl)
                ,@(compile-L3expr else ctxt varfn lblfn)))]
    [l3e-t (t)
           (type-case Context ctxt
             [ctxt-mainfn () `(,@(compile-L3term t 'eax varfn lblfn)
                               ,(l2s-goto L3-endlbl))]
             [ctxt-fn () (type-case L3term t
                           [l3t-apply (fn args)
                                      `(,@(for/list ([arg args] [reg arg-regs])
                                            (l2s-assign reg (encode arg)))
                                        ,(l2s-tcall fn))]
                           [else `(,@(compile-L3term t 'eax varfn lblfn)
                                   ,(l2s-return))])])]))

;; compile an L3term into a list of L2stmts
(define-with-contract (compile-L3term t dst varfn lblfn)
  (L3term? L3-x? (-> symbol?) (-> symbol?) . -> . (listof L2stmt?))
  (type-case L3term t
    [l3t-biop (op v1 v2)
              (let ([tmp (varfn)])
                (if (and (num? v1) (num? v2))
                    (case op
                      [(+) (list (l2s-assign dst (encode (+ v1 v2))))]
                      [(-) (list (l2s-assign dst (encode (- v1 v2))))]
                      [(*) (list (l2s-assign dst (encode (* v1 v2))))]
                      [(<) (list (l2s-assign dst (encode (if (< v1 v2) 1 0))))]
                      [(<=) (list (l2s-assign dst (encode (if (<= v1 v2) 1 0))))]
                      [(=) (list (l2s-assign dst (encode (if (= v1 v2) 1 0))))])
                    (case op
                      [(+) (list (l2s-assign dst (encode v1))
                                 (l2s-aop dst '+= (encode v2))
                                 (l2s-aop dst '-= 1))]
                      [(-) (list (l2s-assign dst (encode v1))
                                 (l2s-aop dst '+= (encode v2))
                                 (l2s-aop dst '+= 1))]
                      [(*) (list (l2s-assign tmp (encode v1))
                                 (l2s-sop tmp '>>= 1)
                                 (l2s-assign dst (encode v2))
                                 (l2s-sop dst '>>= 1)
                                 (l2s-aop dst '*= tmp)
                                 (l2s-sop dst '<<= 1)
                                 (l2s-aop dst '+= 1))]
                      [(<) (list (l2s-cmp dst (encode v1) '< (encode v2))
                                 (l2s-sop dst '<<= 1)
                                 (l2s-aop dst '+= 1))]
                      [(<=) (list (l2s-cmp dst (encode v1) '<= (encode v2))
                                  (l2s-sop dst '<<= 1)
                                  (l2s-aop dst '+= 1))]
                      [(=) (list (l2s-cmp dst (encode v1) '= (encode v2))
                                 (l2s-sop dst '<<= 1)
                                 (l2s-aop dst '+= 1))])))]
    [l3t-pred (pred v)
              (if (num? v)
                  (case pred
                    [(number?) (list (l2s-assign dst 1))]
                    [(a?) (list (l2s-assign dst 0))])
                  (case pred
                    [(number?) (list (l2s-assign dst v)
                                     (l2s-aop dst '&= 1)
                                     (l2s-sop dst '<<= 1)
                                     (l2s-aop dst '+= 1))]
                    [(a?) (list (l2s-assign dst v)
                                (l2s-aop dst '+= 1)
                                (l2s-aop dst '&= 1)
                                (l2s-sop dst '<<= 1)
                                (l2s-aop dst '+= 1))]))]
    [l3t-apply (fn args)
               `(,@(for/list ([arg args] [reg arg-regs])
                     (l2s-assign reg (encode arg)))
                 ,(l2s-call fn)
                 ,@(if (equal? dst 'eax)
                       '()
                       (list (l2s-assign dst 'eax))))]
    [l3t-newarray (len init)
                  `(,(l2s-alloc 'eax (encode len) (encode init))
                    ,@(if (equal? dst 'eax)
                          '()
                          (list (l2s-assign dst 'eax))))]
    [l3t-newtuple (args)
                  `(,(l2s-alloc 'eax (encode (length args)) (encode 0))
                    ,@(for/list ([arg args] [i (in-range 0 (length args))])
                        (l2s-memset 'eax (* 4 (+ i 1)) (encode arg)))
                    ,@(if (equal? dst 'eax)
                          '()
                          (list (l2s-assign dst 'eax))))]
    [l3t-aref (arr i)
              (let ([tmp (varfn)]
                    [passlbl (lblfn)]
                    [faillbl (lblfn)])
                (list (l2s-assign dst (encode i))
                      (l2s-sop dst '>>= 1)
                      (l2s-memget tmp (encode arr) 0)
                      (l2s-cjump dst '< tmp passlbl faillbl)
                      (l2s-label faillbl)
                      (l2s-arrayerr 'eax (encode arr) (encode i))
                      (l2s-label passlbl)
                      (l2s-sop dst '<<= 2)
                      (l2s-aop dst '+= arr)
                      (l2s-memget dst dst 4)))]
    [l3t-aset (arr i v)
              (let ([tmp (varfn)]
                    [passlbl (lblfn)]
                    [faillbl (lblfn)])
                (list (l2s-assign dst (encode i))
                      (l2s-sop dst '>>= 1)
                      (l2s-memget tmp (encode arr) 0)
                      (l2s-cjump dst '< tmp passlbl faillbl)
                      (l2s-label faillbl)
                      (l2s-arrayerr 'eax (encode arr) (encode i))
                      (l2s-label passlbl)
                      (l2s-sop dst '<<= 2)
                      (l2s-aop dst '+= arr)
                      (l2s-memset dst 4 (encode v))
                      (l2s-assign dst (encode 0))))]
    [l3t-alen (arr)
              (list (l2s-memget dst arr 0)
                    (l2s-sop dst '<<= 1)
                    (l2s-aop dst '+= 1))]
    [l3t-print (v)
               `(,(l2s-print 'eax (encode v))
                 ,@(if (equal? dst 'eax)
                       '()
                       (list (l2s-assign dst 'eax))))]
    [l3t-makeclj (proc vars) (compile-L3term (l3t-newtuple (list proc vars)) dst varfn lblfn)]
    [l3t-cljproc (clj) (compile-L3term (l3t-aref clj 0) dst varfn lblfn)]
    [l3t-cljvars (clj) (compile-L3term (l3t-aref clj 1) dst varfn lblfn)]
    [l3t-v (v) (list (l2s-assign dst (encode v)))]))

(define-with-contract (encode s)
  (L3-v? . -> . L2-s?)
  (if (num? s)
      (+ 1 (* s 2))
      s))

(define-with-contract (optimize-endjmp stmts)
  ((listof L2stmt?) . -> . (listof L2stmt?))
  (cond
    [(null? stmts) stmts]        ; 0 stmts
    [(null? (cdr stmts)) stmts]  ; 1 stmt
    [(null? (cddr stmts))        ; 2 stmts
     (let ([fst (first stmts)]
           [snd (second stmts)])
       (if (and (l2s-goto? fst)
                (l2s-label? snd)
                (equal? (l2s-goto-lbl fst) (l2s-label-lbl snd)))
           '()
           stmts))]
    [else (cons (first stmts) (optimize-endjmp (rest stmts)))]))