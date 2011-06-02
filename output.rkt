#lang plai

;;; EECS 322 L Compiler PLAI Output (Code Generation)
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "utils.rkt"))
(require (file "types.rkt"))

;;;
;;; L1 -> S-EXPR
;;;

(define-with-contract (format-L1prog prog)
  (L1prog? . -> . list?)
  (type-case L1prog prog
    [l1prog (main others)
            `(,(format-L1fn main) ,@(map format-L1fn others))]))

(define-with-contract (format-L1fn fn)
  (L1fn? . -> . any/c)
  (type-case L1fn fn
    [l1mainfn (stmts) (map format-L1stmt stmts)]
    [l1fn (lbl stmts) `(,lbl ,@(map format-L1stmt stmts))]))

(define-with-contract (format-L1stmt stmt)
  (L1stmt? . -> . any/c)
  (type-case L1stmt stmt
    [l1s-assign (lhs rhs) `(,lhs <- ,rhs)]
    [l1s-memget (lhs base offset) `(,lhs <- (mem ,base ,offset))]
    [l1s-memset (base offset rhs) `((mem ,base ,offset) <- ,rhs)]
    [l1s-aop (lhs op rhs) `(,lhs ,op ,rhs)]
    [l1s-sop (lhs op rhs) `(,lhs ,op ,rhs)]
    [l1s-cmp (lhs c1 op c2) `(,lhs <- ,c1 ,op ,c2)]
    [l1s-label (lbl) lbl]
    [l1s-goto (lbl) `(goto ,lbl)]
    [l1s-cjump (c1 op c2 lbl1 lbl2) `(cjump ,c1 ,op ,c2 ,lbl1 ,lbl2)]
    [l1s-call (dst) `(call ,dst)]
    [l1s-tcall (dst) `(tail-call ,dst)]
    [l1s-return () `(return)]
    [l1s-print (lhs arg1) `(,lhs <- (print ,arg1))]
    [l1s-alloc (lhs arg1 arg2) `(,lhs <- (allocate ,arg1 ,arg2))]
    [l1s-arrayerr (lhs arg1 arg2) `(,lhs <- (array-error ,arg1 ,arg2))]))

;;;
;;; L2 -> S-EXPR
;;;

(define-with-contract (format-L2prog prog)
  (L2prog? . -> . list?)
  (type-case L2prog prog
    [l2prog (main others)
            `(,(format-L2fn main) ,@(map format-L2fn others))]))

(define-with-contract (format-L2fn fn)
  (L2fn? . -> . any/c)
  (type-case L2fn fn
    [l2mainfn (stmts) (map format-L2stmt stmts)]
    [l2fn (lbl stmts) `(,lbl ,@(map format-L2stmt stmts))]))

(define-with-contract (format-L2stmt stmt)
  (L2stmt? . -> . any/c)
  (type-case L2stmt stmt
    [l2s-assign (lhs rhs) `(,lhs <- ,rhs)]
    [l2s-memget (lhs base offset) `(,lhs <- (mem ,base ,offset))]
    [l2s-memset (base offset rhs) `((mem ,base ,offset) <- ,rhs)]
    [l2s-aop (lhs op rhs) `(,lhs ,op ,rhs)]
    [l2s-sop (lhs op rhs) `(,lhs ,op ,rhs)]
    [l2s-cmp (lhs c1 op c2) `(,lhs <- ,c1 ,op ,c2)]
    [l2s-label (lbl) lbl]
    [l2s-goto (lbl) `(goto ,lbl)]
    [l2s-cjump (c1 op c2 lbl1 lbl2) `(cjump ,c1 ,op ,c2 ,lbl1 ,lbl2)]
    [l2s-call (dst) `(call ,dst)]
    [l2s-tcall (dst) `(tail-call ,dst)]
    [l2s-return () `(return)]
    [l2s-print (lhs arg1) `(,lhs <- (print ,arg1))]
    [l2s-alloc (lhs arg1 arg2) `(,lhs <- (allocate ,arg1 ,arg2))]
    [l2s-arrayerr (lhs arg1 arg2) `(,lhs <- (array-error ,arg1 ,arg2))]))

;;;
;;; L3 -> S-EXPR
;;;

(define-with-contract (format-L3prog prog)
  (L3prog? . -> . list?)
  (type-case L3prog prog
    [l3prog (main others)
            `(,(format-L3fn main) ,@(map format-L3fn others))]))

(define-with-contract (format-L3fn fn)
  (L3fn? . -> . any/c)
  (type-case L3fn fn
    [l3mainfn (body) (format-L3expr body)]
    [l3fn (lbl args body)
          `(,lbl ,args ,(format-L3expr body))]))

(define-with-contract (format-L3expr expr)
  (L3expr? . -> . any/c)
  (type-case L3expr expr
    [l3e-let (id binding body) `(let ([,id ,(format-L3term binding)]) ,(format-L3expr body))]
    [l3e-if (test then else) `(if ,test ,(format-L3expr then) ,(format-L3expr else))]
    [l3e-t (t) (format-L3term t)]))

(define-with-contract (format-L3term term)
  (L3term? . -> . any/c)
  (type-case L3term term
    [l3t-biop (op v1 v2) `(,op ,v1 ,v2)]
    [l3t-pred (pred v) `(,pred ,v)]
    [l3t-apply (fn args) (cons fn args)]
    [l3t-newarray (len init) `(new-array ,len ,init)]
    [l3t-newtuple (args) (cons 'new-tuple args)]
    [l3t-aref (arr i) `(aref ,arr ,i)]
    [l3t-aset (arr i v) `(aset ,arr ,i ,v)]
    [l3t-alen (arr) `(alen ,arr)]
    [l3t-print (a1) `(print ,a1)]
    [l3t-makeclj (lbl a2) `(make-closure ,lbl ,a2)]
    [l3t-cljproc (a1) `(closure-proc ,a1)]
    [l3t-cljvars (a1) `(closure-vars ,a1)]
    [l3t-v (v) v]))

;;;
;;; L4 -> S-EXPR
;;;

(define-with-contract (format-L4prog prog)
  (L4prog? . -> . any/c)
  (type-case L4prog prog
    [l4prog (main others)
            (cons (format-L4fn main)
                  (map format-L4fn others))]))

(define-with-contract (format-L4fn fn)
  (L4fn? . -> . any/c)
  (type-case L4fn fn
    [l4mainfn (body) (format-L4expr body)]
    [l4fn (lbl args body)
          `(,lbl ,args ,(format-L4expr body))]))

(define-with-contract (format-L4expr expr)
  (L4expr? . -> . any/c)
  (type-case L4expr expr
    [l4e-let (id binding body) `(let ([,id ,(format-L4expr binding)]) ,(format-L4expr body))]
    [l4e-if (test then else) `(if ,(format-L4expr test) ,(format-L4expr then) ,(format-L4expr else))]
    [l4e-begin (fst snd) `(begin ,(format-L4expr fst) ,(format-L4expr snd))]
    [l4e-app (fn args) (map format-L4expr (cons fn args))]
    [l4e-v (v) v]))

;;;
;;; L5 -> S-EXPR
;;;

(define-with-contract (format-L5expr expr)
  (L5expr? . -> . any/c)
  (type-case L5expr expr
    [l5e-lambda (args body) `(lambda ,args ,(format-L5expr body))]
    [l5e-let (id binding body) `(let ([,id ,(format-L5expr binding)]) ,(format-L5expr body))]
    [l5e-letrec (id binding body) `(letrec ([,id ,(format-L5expr binding)]) ,(format-L5expr body))]
    [l5e-if (test then else) `(if ,(format-L5expr test) ,(format-L5expr then) ,(format-L5expr else))]
    [l5e-newtuple (args) (cons 'new-tuple (map format-L5expr args))]
    [l5e-begin (fst snd) `(begin ,(format-L5expr fst) ,(format-L5expr snd))]
    [l5e-app (fn args) (map format-L5expr (cons fn args))]
    [l5e-prim (prim) prim]
    [l5e-var (var) var]
    [l5e-num (num) num]))
