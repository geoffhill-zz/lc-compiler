#lang plai

;;; EECS 322 L3->L2 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))

;; L3prog types
(define-type L3prog
  [l3p-mt]
  [l3p-mainfn (e L3expr?)]
  [l3p-fn (lbl label?)
          (args (listof L3-v?))
          (e L3expr?)])

;; L3expr types
(define-type L3expr
  [l3s-let (bound L3-v?)
           (binding L3term?)
           (body L3expr?)]
  [l3s-if (test L3-v?)
          (then L3expr?)
          (else L3expr?)]
  [l3s-d (d L3term?)])

;; L3term types
(define-type L3term
  [l3t-biop (op L3-biop?) (a1 L3-v?) (a2 L3-v?)]
  [l3t-pred (pred L3-pred?) (a1 L3-v?)]
  [l3t-apply (fn L3-v?) (args (listof L3-v?))]
  [l3t-newarray (a1 L3-v?) (a2 L3-v?)]
  [l3t-newtuple (args (listof L3-v?))]
  [l3t-aref (a1 L3-v?) (a2 L3-v?)]
  [l3t-aset (a1 L3-v?) (a2 L3-v?) (a3 L3-v?)]
  [l3t-alen (a1 L3-v?)]
  [l3t-print (a1 L3-v?)]
  [l3t-makeclosure (lbl label?) (a2 L3-v?)]
  [l3t-closureproc (a1 L3-v?)]
  [l3t-closurevars (a1 L3-v?)]
  [l3t-v (val L3-v?)])

(define/contract (build-prog src)
  (any/c . -> . L3prog?)
  (l3p-mt))

(define/contract (build-expr src)
  (any/c . -> . L3expr?)
  (l3s-d (l3t-v 5)))

(define/contract (build-term src)
  (any/c . -> . L3term?)
  (match src
    [`(,(? L3-biop? op) ,(? L3-v? a1) ,(? L3-v? a2)) (l3t-biop op a1 a2)]
    [`(,(? L3-pred? pred) ,(? L3-v? a1)) (l3t-pred pred a1)]))
