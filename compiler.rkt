#lang plai

;;; EECS 322 L Compiler Suite
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "types.rkt"))
(require (file "preds.rkt"))
(require (file "utils.rkt"))

(require (file "L5.rkt"))
(require (file "L4.rkt"))
(require (file "L3.rkt"))
(require (file "L2.rkt"))
(require (file "L1.rkt"))
  
;;;
;;; EXTERNAL INTERFACE
;;;

#|
(define-with-contract (main fname)
  (string? . -> . void?)
  (call-with-input-file fname main/compile-L))

(define-with-contract (main/compile-L port)
  (input-port? . -> . void?)
  (compile-L1prog
   (compile-L2prog
    (compile-L3prog
     (compile-L4prog
      (compile-L5prog
       (build-L5prog (read port))))))))
|#
