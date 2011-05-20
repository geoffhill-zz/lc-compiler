#lang plai

;;; EECS 322 L Compiler PLAI Types
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))
(require (file "utils.rkt"))

;;;
;;; L1 PLAI TYPES
;;;

(define-type L1prog
  [l1prog (main L1fn?)
          (others (listof L1fn?))])

(define-type L1fn
  [l1fn (stmts (listof L1stmt?))])

(define-type L1stmt
  [l1s-assign (lhs L1-x?) (rhs L1-s?)]
  [l1s-memget (lhs L1-x?) (base L1-x?) (offset n4?)]
  [l1s-memset (base L1-x?) (offset n4?) (rhs L1-s?)]
  [l1s-aop (lhs L1-x?) (op aop?) (rhs L1-s?)]
  [l1s-sop (lhs L1-x?) (op sop?) (rhs L1-s?)]
  [l1s-cmp (lhs L1-x?) (c1 L1-s?) (op cmp?) (c2 L1-s?)]
  [l1s-label (lbl label?)]
  [l1s-goto (lbl label?)]
  [l1s-cjump (c1 L1-s?) (op cmp?) (c2 L1-s?) (lbl1 label?) (lbl2 label?)]
  [l1s-call (dst L1-s?)]
  [l1s-tcall (dst L1-s?)]
  [l1s-return]
  [l1s-print (lhs L1-x?) (arg1 L1-s?)]
  [l1s-alloc (lhs L1-x?) (arg1 L1-s?) (arg2 L1-s?)]
  [l1s-arrayerr (lhs L1-x?) (arg1 L1-s?) (arg2 L1-s?)])

;;;
;;; S-EXPR -> L1
;;;

(define/contract (build-L1prog src)
  (any/c . -> . L1prog?)
  (l1prog (build-L1fn (car src))
          (map build-L1fn (cdr src))))

(define/contract (build-L1fn src)
  (any/c . -> . L1fn?)
  (l1fn (map build-L1stmt src)))

(define/contract (build-L1stmt src)
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
;;; L1 -> S-EXPR
;;;

(define/contract (format-L1prog prog)
  (L1prog? . -> . any/c)
  (type-case L1prog prog
    [l1prog (main others)
            (cons (format-L1fn main)
                  (map format-L1fn others))]))

(define/contract (format-L1fn fn)
  (L1fn? . -> . any/c)
  (type-case L1fn fn
    [l1fn (stmts)
          (map format-L1stmt stmts)]))

(define/contract (format-L1stmt stmt)
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
;;; L2 PLAI TYPES
;;;

(define-type L2prog
  [l2prog (main L2fn?)
          (others (listof L2fn?))])

(define-type L2fn
  [l2fn (stmts (listof L2stmt?))])

(define-type L2stmt
  [l2s-assign (lhs L2-x?) (rhs L2-s?)]
  [l2s-memget (lhs L2-x?) (base L2-x?) (offset n4?)]
  [l2s-memset (base L2-x?) (offset n4?) (rhs L2-s?)]
  [l2s-aop (lhs L2-x?) (op aop?) (rhs L2-s?)]
  [l2s-sop (lhs L2-x?) (op sop?) (rhs L2-s?)]
  [l2s-cmp (lhs L2-x?) (c1 L2-s?) (op cmp?) (c2 L2-s?)]
  [l2s-label (lbl label?)]
  [l2s-goto (lbl label?)]
  [l2s-cjump (c1 L2-s?) (op cmp?) (c2 L2-s?) (lbl1 label?) (lbl2 label?)]
  [l2s-call (dst L2-s?)]
  [l2s-tcall (dst L2-s?)]
  [l2s-return]
  [l2s-print (lhs L2-x?) (arg1 L2-s?)]
  [l2s-alloc (lhs L2-x?) (arg1 L2-s?) (arg2 L2-s?)]
  [l2s-arrayerr (lhs L2-x?) (arg1 L2-s?) (arg2 L2-s?)])

;;;
;;; S-EXPR -> L2
;;;

(define/contract (build-L2prog src)
  (any/c . -> . L2prog?)
  (l2prog (build-L2fn (car src))
          (map build-L2fn (cdr src))))

(define/contract (build-L2fn src)
  (any/c . -> . L2fn?)
  (l2fn (map build-L2stmt src)))

(define/contract (build-L2stmt src)
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
;;; L2 -> S-EXPR
;;;

(define/contract (format-L2prog prog)
  (L2prog? . -> . any/c)
  (type-case L2prog prog
    [l2prog (main others)
            (cons (format-L2fn main)
                  (map format-L2fn others))]))

(define/contract (format-L2fn fn)
  (L2fn? . -> . any/c)
  (type-case L2fn fn
    [l2fn (stmts)
          (map format-L2stmt stmts)]))

(define/contract (format-L2stmt stmt)
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
;;; L3 PLAI TYPES
;;;

(define-type L3prog
  [l3prog (main l3mainfn?)
          (others (listof l3fn?))])

(define-type L3fn
  [l3mainfn (body L3expr?)]
  [l3fn (lbl label?)
        (args (listof L3-v?))
        (body L3expr?)])

(define-type L3expr
  [l3e-let (id L3-v?)
           (binding L3term?)
           (body L3expr?)]
  [l3e-if (test L3-v?)
          (then L3expr?)
          (else L3expr?)]
  [l3e-t (t L3term?)])

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

;;;
;;; S-EXPR -> L3
;;;

(define/contract (build-L3prog src)
  (list? . -> . L3prog?)
  (l3prog (build-l3mainfn (car src))
          (map build-l3fn (cdr src))))

(define/contract (build-l3mainfn src)
  (any/c . -> . l3mainfn?)
  (l3mainfn (build-L3expr src)))

(define/contract (build-l3fn src)
  (any/c . -> . l3fn?)
  (match src
    [`(,(? label? lbl) (,(? L3-x? args) ...) ,e)
     (l3fn lbl args (build-L3expr e))]
    [_ (error 'L3 "not a well-formed function")]))

(define/contract (build-L3expr src)
  (any/c . -> . L3expr?)
  (match src
    [`(let ([,(? L3-x? id) ,t]) ,e) (l3e-let id (build-L3term t) (build-L3expr e))]
    [`(if ,(? L3-v? v) ,e1 ,e2) (l3e-if v (build-L3expr e1) (build-L3expr e2))]
    [_ (l3e-t (build-L3term src))]))

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

;;;
;;; L3 -> S-EXPR
;;;

(define/contract (format-L3prog prog)
  (L3prog? . -> . any/c)
  (type-case L3prog prog
    [l3prog (main others)
            `(,(format-L3expr main)
              ,(map format-L3fn others))]))

(define/contract (format-L3fn fn)
  (L3fn? . -> . any/c)
  (type-case L3fn fn
    [l3mainfn (body) (format-L3expr body)]
    [l3fn (lbl args body)
          `(,lbl ,args ,(format-L3expr body))]))

(define/contract (format-L3expr expr)
  (L3expr? . -> . any/c)
  (type-case L3expr expr
    [l3e-let (id binding body) `(let ([,id ,(format-L3term binding)]) ,(format-L3expr body))]
    [l3e-if (test then else) `(if ,test ,(format-L3expr then) ,(format-L3expr else))]
    [l3e-t (t) (format-L3term t)]))

(define/contract (format-L3term term)
  (L3term? . -> . any/c)
  (type-case L3term term
    [l3t-biop (op v1 v2) `(,op v1 v2)]
    [l3t-pred (pred v) `(,pred v)]
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
;;; L4 PLAI TYPES
;;;

(define-type L4prog
  [l4prog (main l4mainfn?)
          (others (listof l4fn?))])

(define-type L4fn
  [l4mainfn (body L4expr?)]
  [l4fn (lbl label?)
        (args (listof L4-v?))
        (body L4expr?)])

(define-type L4expr
  [l4e-let (id L4-v?)
           (binding L4expr?)
           (body L4expr?)]
  [l4e-if (test L4expr?)
          (then L4expr?)
          (else L4expr?)]
  [l4e-app (fn L4expr?)
           (args (listof L4expr?))]
  [l4e-v (v L4-v?)])

;;;
;;; S-EXPR -> L4
;;;

(define/contract (build-L4prog src)
  (list? . -> . L4prog?)
  (l4prog (build-l4mainfn (car src))
          (map build-l4fn (cdr src))))

(define/contract (build-l4mainfn src)
  (any/c . -> . l4mainfn?)
  (l4mainfn (build-L4expr src)))

(define/contract (build-l4fn src)
  (any/c . -> . l4fn?)
  (match src
    [`(,(? label? lbl) (,(? L4-x? args) ...) ,e)
     (l4fn lbl args (build-L4expr e))]
    [_ (error 'L4 "not a well-formed function")]))

(define/contract (build-L4expr src)
  (any/c . -> . L4expr?)
  (match src
    [`(let ([,(? L4-x? id) ,e1]) ,e2) (l4e-let id (build-L4expr e1) (build-L4expr e2))]
    [`(if ,e1 ,e2 ,e3) (l4e-if (build-L4expr e1) (build-L4expr e2) (build-L4expr e3))]
    [`(,fn ,args ...) (l4e-app (build-L4expr fn) (map build-L4expr args))]
    [_ (error 'L4 "not a well-formed expression")]))

;;;
;;; L4 -> S-EXPR
;;;

(define/contract (format-L4prog prog)
  (L4prog? . -> . any/c)
  (type-case L4prog prog
    [l4prog (main others)
            `(,(format-L4expr main)
              ,(map format-L4fn others))]))

(define/contract (format-L4fn fn)
  (L4fn? . -> . any/c)
  (type-case L4fn fn
    [l4mainfn (body) (format-L4expr body)]
    [l4fn (lbl args body)
          `(,lbl ,args ,(format-L4expr body))]))

(define/contract (format-L4expr expr)
  (L4expr? . -> . any/c)
  (type-case L4expr expr
    [l4e-let (id binding body) `(let ([,id ,(format-L4expr binding)]) ,(format-L4expr body))]
    [l4e-if (test then else) `(if ,(format-L4expr test) ,(format-L4expr then) ,(format-L4expr else))]
    [l4e-app (fn args) (append `(,(format-L4expr fn)) (map format-L4expr args))]
    [l4e-v (v) v]))