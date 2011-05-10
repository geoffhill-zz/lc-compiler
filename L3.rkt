#lang plai

;;; EECS 322 L3->L2 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))

;; L3stmt types
(define-type L3prog
  [l3p-mt]
  [l3p-mainfn]
  [l3p-fn])

(define-type L3expr
  [l3s-mt]
  [l3s-let]
  [l3s-if]
  [l3s-d])

(define-type L3term
  [l3t-biop]
  [l3t-pred]
  [l3t-apply]
  [l3t-newarray]
  [l3t-newtuple]
  [l3t-aref]
  [l3t-aset]
  [l3t-alen]
  [l3t-print]
  [l3t-makeclosure]
  [l3t-closureproc]
  [l3t-closurevars]
  [l3t-v])