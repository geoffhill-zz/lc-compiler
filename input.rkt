#lang plai

;;; EECS 322 L Compiler PLAI Input (Code Parsing)
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))
(require (file "utils.rkt"))
(require (file "types.rkt"))

;;;
;;; S-EXPR -> L1
;;;

(define-with-contract (build-L1prog src)
  (any/c . -> . L1prog?)
  (l1prog (build-L1fn (car src))
          (map build-L1fn (cdr src))))

(define-with-contract (build-L1fn src)
  (any/c . -> . L1fn?)
  (l1fn (map build-L1stmt src)))

(define-with-contract (build-L1stmt src)
  (any/c . -> . L1stmt?)
  (match src
    [`(,(? L1-x? lhs) <- ,(? L1-s? rhs)) (l1s-assign lhs rhs)]
    [`(,(? L1-x? lhs) <- (mem ,(? L1-x? base) ,(? n4? offset))) (l1s-memget lhs base offset)]
    [`((mem ,(? L1-x? base) ,(? n4? offset)) <- ,(? L1-s? rhs)) (l1s-memset base offset rhs)]
    [`(,(? L1-x? lhs) ,(? aop? op) ,(? L1-s? rhs)) (l1s-aop lhs op rhs)]
    [`(,(? L1-x? lhs) ,(? sop? op) ,(? L1-s? rhs)) (l1s-sop lhs op rhs)]
    [`(,(? L1-x? lhs) <- ,(? L1-s? c1) ,(? cmp? op) ,(? L1-s? c2)) (l1s-cmp lhs c1 op c2)]
    [(? label? lbl) (l1s-label lbl)]
    [`(goto ,(? label? lbl)) (l1s-goto lbl)]
    [`(cjump ,(? L1-s? c1) ,(? cmp? op) ,(? L1-s? c2) ,(? label? lbl1) ,(? label? lbl2))
     (l1s-cjump c1 op c2 lbl1 lbl2)]
    [`(call ,(? L1-s? dst)) (l1s-call dst)]
    [`(tail-call ,(? L1-s? dst)) (l1s-tcall dst)]
    [`(return) (l1s-return)]
    [`(,(? L1-x? lhs) <- (print ,(? L1-s? arg1))) (l1s-print lhs arg1)]
    [`(,(? L1-x? lhs) <- (allocate ,(? L1-s? arg1) ,(? L1-s? arg2))) (l1s-alloc lhs arg1 arg2)]
    [`(,(? L1-x? lhs) <- (array-error ,(? L1-s? arg1) ,(? L1-s? arg2))) (l1s-arrayerr lhs arg1 arg2)]
    [_ (error 'L1 "no matching clause for ~a" src)]))

;;;
;;; S-EXPR -> L2
;;;

(define-with-contract (build-L2prog src)
  (any/c . -> . L2prog?)
  (l2prog (build-L2fn (car src))
          (map build-L2fn (cdr src))))

(define-with-contract (build-L2fn src)
  (any/c . -> . L2fn?)
  (l2fn (map build-L2stmt src)))

(define-with-contract (build-L2stmt src)
  (any/c . -> . L2stmt?)
  (match src
    [`(,(? L2-x? lhs) <- ,(? L2-s? rhs)) (l2s-assign lhs rhs)]
    [`(,(? L2-x? lhs) <- (mem ,(? L2-x? base) ,(? n4? offset))) (l2s-memget lhs base offset)]
    [`((mem ,(? L2-x? base) ,(? n4? offset)) <- ,(? L2-s? rhs)) (l2s-memset base offset rhs)]
    [`(,(? L2-x? lhs) ,(? aop? op) ,(? L2-s? rhs)) (l2s-aop lhs op rhs)]
    [`(,(? L2-x? lhs) ,(? sop? op) ,(? L2-s? rhs)) (l2s-sop lhs op rhs)]
    [`(,(? L2-x? lhs) <- ,(? L2-s? c1) ,(? cmp? op) ,(? L2-s? c2)) (l2s-cmp lhs c1 op c2)]
    [(? label? lbl) (l2s-label lbl)]
    [`(goto ,(? label? lbl)) (l2s-goto lbl)]
    [`(cjump ,(? L2-s? c1) ,(? cmp? op) ,(? L2-s? c2) ,(? label? lbl1) ,(? label? lbl2))
     (l2s-cjump c1 op c2 lbl1 lbl2)]
    [`(call ,(? L2-s? dst)) (l2s-call dst)]
    [`(tail-call ,(? L2-s? dst)) (l2s-tcall dst)]
    [`(return) (l2s-return)]
    [`(,(? L2-x? lhs) <- (print ,(? L2-s? arg1))) (l2s-print lhs arg1)]
    [`(,(? L2-x? lhs) <- (allocate ,(? L2-s? arg1) ,(? L2-s? arg2))) (l2s-alloc lhs arg1 arg2)]
    [`(,(? L2-x? lhs) <- (array-error ,(? L2-s? arg1) ,(? L2-s? arg2))) (l2s-arrayerr lhs arg1 arg2)]
    [_ (error 'L2 "no matching clause for ~a" src)]))

;;;
;;; S-EXPR -> L3
;;;

(define-with-contract (build-L3prog src)
  (list? . -> . L3prog?)
  (l3prog (build-l3mainfn (car src))
          (map build-l3fn (cdr src))))

(define-with-contract (build-l3mainfn src)
  (any/c . -> . l3mainfn?)
  (l3mainfn (build-L3expr src)))

(define-with-contract (build-l3fn src)
  (any/c . -> . l3fn?)
  (match src
    [`(,(? label? lbl) (,(? L3-x? args) ...) ,e)
     (l3fn lbl args (build-L3expr e))]
    [_ (error 'L3 "not a well-formed function")]))

(define-with-contract (build-L3expr src)
  (any/c . -> . L3expr?)
  (match src
    [`(let ([,(? L3-x? id) ,t]) ,e) (l3e-let id (build-L3term t) (build-L3expr e))]
    [`(if ,(? L3-v? v) ,e1 ,e2) (l3e-if v (build-L3expr e1) (build-L3expr e2))]
    [_ (l3e-t (build-L3term src))]))

(define-with-contract (build-L3term src)
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

;;;
;;; S-EXPR -> L4
;;;

(define-with-contract (build-L4prog src)
  (list? . -> . L4prog?)
  (l4prog (build-l4mainfn (car src))
          (map build-l4fn (cdr src))))

(define-with-contract (build-l4mainfn src)
  (any/c . -> . l4mainfn?)
  (l4mainfn (build-L4expr src)))

(define-with-contract (build-l4fn src)
  (any/c . -> . l4fn?)
  (match src
    [`(,(? label? lbl) (,(? L4-x? args) ...) ,e)
     (l4fn lbl args (build-L4expr e))]
    [_ (error 'L4 "not a well-formed function")]))

(define-with-contract (build-L4expr src)
  (any/c . -> . L4expr?)
  (match src
    [`(let ([,(? L4-x? id) ,e1]) ,e2) (l4e-let id (build-L4expr e1) (build-L4expr e2))]
    [`(if ,e1 ,e2 ,e3) (l4e-if (build-L4expr e1) (build-L4expr e2) (build-L4expr e3))]
    [`(begin ,e1 ,e2) (l4e-begin (build-L4expr e1) (build-L4expr e2))]
    [`(,fn ,args ...) (l4e-app (build-L4expr fn) (map build-L4expr args))]
    [(? L4-v?) (l4e-v src)]
    [_ (error 'L4 "not a well-formed expression")]))

;;;
;;; S-EXPR -> L5
;;;

(define-with-contract (build-L5expr src)
  (any/c . -> . L5expr?)
  (match src
    [`(lambda (,args ...) ,e) (l5e-lambda args (build-L5expr e))]
    [`(let ([,(? L5-var? id) ,e1]) ,e2) (l5e-let id (build-L5expr e1) (build-L5expr e2))]
    [`(letrec ([,(? L5-var? id) ,e1]) ,e2) (l5e-letrec id (build-L5expr e1) (build-L5expr e2))]
    [`(if ,e1 ,e2 ,e3) (l5e-if (build-L5expr e1) (build-L5expr e2) (build-L5expr e3))]
    [`(new-tuple ,args ...) (l5e-newtuple (map build-L5expr args))]
    [`(begin ,e1 ,e2) (l5e-begin (build-L5expr e1) (build-L5expr e2))]
    [`(,fn ,args ...) (l5e-app (build-L5expr fn) (map build-L5expr args))]
    [(? L5-builtin? prim) (l5e-prim prim)]
    [(? L5-var? var) (l5e-var var)]
    [(? num? num) (l5e-num num)]
    [_ (error 'L5 "not a well-formed expression")]))
