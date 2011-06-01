#lang plai
(print-only-errors #t)

;;; EECS 322 L4->L3 Compiler -- Test
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "types.rkt"))
(require (file "input.rkt"))
(require (file "L4.rkt"))

(test (compile-L4prog (build-L4prog '(5)))
      (build-L3prog '(5)))
(test (compile-L4prog (build-L4prog '(h)))
      (build-L3prog '(v0)))
(test (compile-L4prog (build-L4prog '((+ 2 3))))
      (build-L3prog '((+ 2 3))))
(test (compile-L4prog (build-L4prog '((+ ttt uuu))))
      (build-L3prog '((+ v0 v1))))
(test (compile-L4prog (build-L4prog '((print (let ([f 3])
                                               (+ (* f f)
                                                  (let ([f 5])
                                                    (- f 7))))))))
      (build-L3prog '((let ([v0 3])
                        (let ([r0 (* v0 v0)])
                          (let ([v1 5])
                            (let ([r1 (- v1 7)])
                              (let ([r2 (+ r0 r1)])
                                (print r2)))))))))
(test (compile-L4prog (build-L4prog '((if 1 2 3))))
      (build-L3prog '((if 1 2 3))))
(test (compile-L4prog (build-L4prog '((if (+ 0 1) 2 3))))
      (build-L3prog '((let ([r0 (+ 0 1)])
                        (if r0 2 3)))))
(test (compile-L4prog (build-L4prog '((if (if 1 2 3) 4 5))))
      (build-L3prog '((if 1 (if 2 4 5) (if 3 4 5)))))
(test (compile-L4prog (build-L4prog '((if (if a b c) d e))))
      (build-L3prog '((if v0 (if v1 v3 v4) (if v2 v3 v4)))))

(printf "tests completed~n")
