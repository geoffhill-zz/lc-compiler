#lang plai
(print-only-errors #t)

;;; EECS 322 L4->L3 Compiler -- Test
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "types.rkt"))
(require (file "L4.rkt"))

(test (rename-L4prog (build-L4prog '((+ 1 2))))
      (build-L4prog '((+ 1 2))))
(test (rename-L4prog (build-L4prog '((a 1 2))))
      (build-L4prog '((var_0 1 2))))
(test (rename-L4prog (build-L4prog '((:a 5) (:a (a) a))))
      (build-L4prog '((:lbl_0 5) (:lbl_0 (var_0) var_0))))
(test (rename-L4prog (build-L4prog '((+ (let ([x 1]) x) (let ([y 2]) y))
                                     (:a () (+ (let ([x 1]) x) (let ([y 2]) y))))))
      (build-L4prog '((+ (let ([var_0 1]) var_0) (let ([var_1 2]) var_1))
                      (:lbl_0 () (+ (let ([var_2 1]) var_2) (let ([var_3 2]) var_3))))))
(test (rename-L4prog (build-L4prog '((* (:a 4) (:a 5)) (:a (x) (- x 2)))))
      (build-L4prog '((* (:lbl_0 4) (:lbl_0 5)) (:lbl_0 (var_0) (- var_0 2)))))

(printf "tests completed~n")
