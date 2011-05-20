#lang plai

;;; EECS 322 L4->L3 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "types.rkt"))
(require (file "preds.rkt"))
(require (file "utils.rkt"))

;;;
;;; L4 -> L3 COMPILATION
;;;

#|
(define/contract (compile-L4prog prog)
  (L4prog? . -> . L3prog?)
  ...)

(define/contract (compile-L4fn fn)
  (L4fn? . -> . L3fn?)
  ...)

(define/contract (compile-L4expr expr)
  (L4expr? . -> . L3expr?)
  ...)
|#
  
;;;
;;; EXTERNAL INTERFACE
;;;

#|
(define/contract (main fname)
  (string? . -> . void?)
  (call-with-input-file fname main/compile-L4))

(define/contract (main/compile-L4 port)
  (input-port? . -> . void?)
  (pretty-write
   (format-L3prog
    (compile-L4prog
     (build-L4prog (read port))))))
|#
