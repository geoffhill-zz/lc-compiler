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
  [l4e-begin (fst L4expr?)
             (snd L4expr?)]
  [l4e-app (fn L4expr?)
           (args (listof L4expr?))]
  [l4e-v (v L4-v?)])
