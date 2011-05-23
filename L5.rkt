#lang plai

;;; EECS 322 L5->L4 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))
(require (file "utils.rkt"))
(require (file "types.rkt"))
(require (file "input.rkt"))
(require (file "output.rkt"))

;;;
;;; L5 -> L4 COMPILATION
;;;

#|
(define-with-contract (compile-L5prog prog)
  (L5prog? . -> . L4prog?)
  ...)

(define-with-contract (compile-L5fn fn)
  (L5fn? . -> . L4fn?)
  ...)

(define-with-contract (compile-L5expr expr)
  (L5expr? . -> . L4expr?)
  ...)
|#
  
;;;
;;; EXTERNAL INTERFACE
;;;

#|
(define-with-contract (main fname)
  (string? . -> . void?)
  (call-with-input-file fname main/compile-L5))

(define-with-contract (main/compile-L5 port)
  (input-port? . -> . void?)
  (pretty-write
   (format-L4prog
    (compile-L5prog
     (build-L5prog (read port))))))
|#
