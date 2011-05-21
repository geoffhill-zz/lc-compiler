#lang plai

;;; EECS 322 L4->L3 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "types.rkt"))
(require (file "preds.rkt"))
(require (file "utils.rkt"))

(define-type L4ctxt
  [mt-ctxt]
  [let-ctxt (id L4-x?)
            (binding L4expr?)
            (body L4expr?)
            (ctxt L4ctxt?)]
  [if-ctxt (test L4expr?)
           (then L4expr?)
           (else L4expr?)
           (ctxt L4ctxt?)]
  [fn-ctxt (op L4expr?)
           (args (listof L4expr?))
           (ctxt L4ctxt?)])

;;;
;;; L4 -> L3 COMPILATION
;;;

(define-with-contract (compile-L4prog prog)
  (L4prog? . -> . L3prog?)
  (type-case L4prog prog
    [l4prog (main others)
            (l3prog (compile-L4fn main)
                    (map compile-L4fn others))]))

(define-with-contract (compile-L4fn fn)
  (L4fn? . -> . L3fn?)
  (type-case L4fn fn
    [l4mainfn (body) (l3mainfn (compile-L4expr body))]
    [l4fn (lbl args body) (l3fn lbl args (compile-L4expr body))]))

(define-with-contract (compile-L4expr expr)
  (L4expr? . -> . L3expr?)
  expr)
  
;;;
;;; EXTERNAL INTERFACE
;;;

(define-with-contract (main fname)
  (string? . -> . void?)
  (call-with-input-file fname main/compile-L4))

(define-with-contract (main/compile-L4 port)
  (input-port? . -> . void?)
  (pretty-write
   (format-L3prog
    (compile-L4prog
     (build-L4prog (read port))))))
