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
  [l3t-biop (op L3-biop?) (v1 L3-v?) (v2 L3-v?)]
  [l3t-pred (pred L3-pred?) (v L3-v?)]
  [l3t-apply (fn L3-v?) (args (listof L3-v?))]
  [l3t-newarray (a1 L3-v?) (a2 L3-v?)]
  [l3t-newtuple (args (listof L3-v?))]
  [l3t-aref (arr L3-v?) (a2 L3-v?)]
  [l3t-aset (arr L3-v?) (a2 L3-v?) (a3 L3-v?)]
  [l3t-alen (arr L3-v?)]
  [l3t-print (a1 L3-v?)]
  [l3t-makeclj (lbl label?) (a2 L3-v?)]
  [l3t-cljproc (a1 L3-v?)]
  [l3t-cljvars (a1 L3-v?)]
  [l3t-v (v L3-v?)])

(define/contract (build-L3prog src)
  (any/c . -> . L3prog?)
  (l3p-mt))

(define/contract (build-L3expr src)
  (any/c . -> . L3expr?)
  (l3s-d (l3t-v 5)))

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
    [(? L3-v? v) (l3t-v v)]))
