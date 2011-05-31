#lang plai

;;; EECS 322 L Compiler PLAI Types
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))

;;;
;;; L1 PLAI TYPES
;;;

(define-type L1prog
  [l1prog (main L1fn?)
          (others (listof L1fn?))])

#|
(type-case L1prog prog
  [l1prog (main others) prog])
|#

(define-type L1fn
  [l1fn (stmts (listof L1stmt?))])

#|
(type-case L1fn fn
  [l1fn (stmts) fn])
|#

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

#|
(type-case L1stmt stmt
  [l1s-assign (lhs rhs) stmt]
  [l1s-memget (lhs base offset) stmt]
  [l1s-memset (base offset rhs) stmt]
  [l1s-aop (lhs op rhs) stmt]
  [l1s-sop (lhs op rhs) stmt]
  [l1s-cmp (lhs c1 op c2) stmt]
  [l1s-label (lbl) stmt]
  [l1s-goto (lbl) stmt]
  [l1s-cjump (c1 op c2 lbl1 lbl2) stmt]
  [l1s-call (dst) stmt]
  [l1s-tcall (dst) stmt]
  [l1s-return () stmt]
  [l1s-print (lhs arg1) stmt]
  [l1s-alloc (lhs arg1 arg2) stmt]
  [l1s-arrayerr (lhs arg1 arg2) stmt])
|#

;;;
;;; L2 PLAI TYPES
;;;

(define-type L2prog
  [l2prog (main L2fn?)
          (others (listof L2fn?))])
#|
(type-case L2prog prog
  [l2prog (main others) prog])
|#

(define-type L2fn
  [l2fn (stmts (listof L2stmt?))])

#|
(type-case L2fn fn
  [l2fn (stmts) fn])
|#

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

#|
(type-case L2stmt stmt
  [l2s-assign (lhs rhs) stmt]
  [l2s-memget (lhs base offset) stmt]
  [l2s-memset (base offset rhs) stmt]
  [l2s-aop (lhs op rhs) stmt]
  [l2s-sop (lhs op rhs) stmt]
  [l2s-cmp (lhs c1 op c2) stmt]
  [l2s-label (lbl) stmt]
  [l2s-goto (lbl) stmt]
  [l2s-cjump (c1 op c2 lbl1 lbl2) stmt]
  [l2s-call (dst) stmt]
  [l2s-tcall (dst) stmt]
  [l2s-return () stmt]
  [l2s-print (lhs arg1) stmt]
  [l2s-alloc (lhs arg1 arg2) stmt]
  [l2s-arrayerr (lhs arg1 arg2) stmt])
|#

;;;
;;; L3 PLAI TYPES
;;;

(define-type L3prog
  [l3prog (main l3mainfn?)
          (others (listof l3fn?))])

#|
(type-case L3prog prog
  [l3prog (main others) prog])
|#

(define-type L3fn
  [l3mainfn (body L3expr?)]
  [l3fn (lbl label?)
        (args (listof L3-v?))
        (body L3expr?)])

#|
(type-case L3fn fn
  [l3mainfn (body) fn]
  [l3fn (lbl args body) fn])
|#

(define-type L3expr
  [l3e-let (id L3-v?)
           (binding L3term?)
           (body L3expr?)]
  [l3e-if (test L3-v?)
          (then L3expr?)
          (else L3expr?)]
  [l3e-t (t L3term?)])

#|
(type-case L3expr expr
  [l3e-let (id binding body) expr]
  [l3e-if (test then else) expr]
  [l3e-t (t) expr])
|#

(define-type L3term
  [l3t-biop (op L3-biop?) (v1 L3-v?) (v2 L3-v?)]
  [l3t-pred (pred L3-pred?) (v L3-v?)]
  [l3t-apply (fn L3-v?) (args (listof L3-v?))]
  [l3t-newarray (len L3-v?) (init L3-v?)]
  [l3t-newtuple (args (listof L3-v?))]
  [l3t-aref (arr L3-v?) (i L3-v?)]
  [l3t-aset (arr L3-v?) (i L3-v?) (v L3-v?)]
  [l3t-alen (arr L3-v?)]
  [l3t-print (v L3-v?)]
  [l3t-makeclj (proc label?) (vars L3-v?)]
  [l3t-cljproc (clj L3-v?)]
  [l3t-cljvars (clj L3-v?)]
  [l3t-v (v L3-v?)])

#|
(type-case L3term term
  [l3t-biop (op v1 v2) term]
  [l3t-pred (pred v) term]
  [l3t-apply (fn args) term]
  [l3t-newarray (len init) term]
  [l3t-newtuple (args) term]
  [l3t-aref (arr i) term]
  [l3t-aset (arr i v) term]
  [l3t-alen (arr) term]
  [l3t-print (v) term]
  [l3t-makeclj (proc vars) term]
  [l3t-cljproc (clj) term]
  [l3t-cljvars (clj) term]
  [l3t-v (v) term])
|#

;;;
;;; L4 PLAI TYPES
;;;

(define-type L4prog
  [l4prog (main l4mainfn?)
          (others (listof l4fn?))])

#|
(type-case L4prog prog
  [l4prog (main others) prog])
|#

(define-type L4fn
  [l4mainfn (body L4expr?)]
  [l4fn (lbl label?)
        (args (listof L4-x?))
        (body L4expr?)])
#|
(type-case L4fn fn
  [l4mainfn (body) fn]
  [l4fn (lbl args body) fn])
|#

(define-type L4expr
  [l4e-let (id L4-x?)
           (binding L4expr?)
           (body L4expr?)]
  [l4e-if (test L4expr?)
          (then L4expr?)
          (else L4expr?)]
  [l4e-begin (fst L4expr?)
             (snd L4expr?)]
  [l4e-app (fn L4expr?)
           (args (listof L4expr?))]
  [l4e-v (v L4-v?)])

#|
(type-case L4expr expr
  [l4e-let (id binding body) expr]
  [l4e-if (test then else) expr]
  [l4e-begin (fst snd) expr]
  [l4e-app (fn args) expr]
  [l4e-v (v) expr])
|#

;;;
;;; L5 PLAI TYPES
;;;

(define-type L5expr
  [l5e-lambda (args (listof L5-var?))
              (body L5expr?)]
  [l5e-let (id L5-var?)
           (binding L5expr?)
           (body L5expr?)]
  [l5e-letrec (id L5-var?)
              (binding L5expr?)
              (body L5expr?)]
  [l5e-if (test L5expr?)
          (then L5expr?)
          (else L5expr?)]
  [l5e-newtuple (args (listof L5expr?))]
  [l5e-begin (fst L5expr?)
             (snd L5expr?)]
  [l5e-app (fn L5expr?)
           (args (listof L5expr?))]
  [l5e-prim (prim L5-builtin?)]
  [l5e-var (var L5-var?)]
  [l5e-num (num num?)])

#|
(type-case L5expr expr
  [l5e-lambda (args body) expr]
  [l5e-let (id binding body) expr]
  [l5e-letrec (id binding body) expr]
  [l5e-if (test then else) expr]
  [l5e-newtuple (args) expr]
  [l5e-begin (fst snd) expr]
  [l5e-app (fn args) expr]
  [l5e-prim (prim) expr]
  [l5e-var (var) expr]
  [l5e-num (num) expr])
|#
