#lang plai
(print-only-errors #t)

;;; EECS 322 L3->L2 Compiler -- Test
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "types.rkt"))
(require (file "input.rkt"))
(require (file "L3.rkt"))

(test (build-L3term '(+ 0 0)) (l3t-biop '+ 0 0))
(test (build-L3term '(- 5 6)) (l3t-biop '- 5 6))
(test (build-L3term '(* a 9)) (l3t-biop '* 'a 9))
(test (build-L3term '(< -12 b)) (l3t-biop '< -12 'b))
(test (build-L3term '(<= d e)) (l3t-biop '<= 'd 'e))
(test (build-L3term '(= f f)) (l3t-biop '= 'f 'f))

(test (build-L3term '(a? 0)) (l3t-pred 'a? 0))
(test (build-L3term '(a? a)) (l3t-pred 'a? 'a))
(test (build-L3term '(a? f1)) (l3t-pred 'a? 'f1))
(test (build-L3term '(number? 0)) (l3t-pred 'number? 0))
(test (build-L3term '(number? b)) (l3t-pred 'number? 'b))
(test (build-L3term '(number? l1)) (l3t-pred 'number? 'l1))

(test (build-L3term '(rk)) (l3t-apply 'rk '()))
(test (build-L3term '(gg -6)) (l3t-apply 'gg '(-6)))
(test (build-L3term '(tt 1 a 2 b)) (l3t-apply 'tt '(1 a 2 b)))

(test (build-L3term '(new-array 5 0)) (l3t-newarray 5 0))
(test (build-L3term '(new-array x 0)) (l3t-newarray 'x 0))
(test (build-L3term '(new-array 1 u)) (l3t-newarray 1 'u))
(test (build-L3term '(new-array y z)) (l3t-newarray 'y 'z))
(test (build-L3term '(new-array h :l1)) (l3t-newarray 'h ':l1))

(test (build-L3term '(new-tuple 3)) (l3t-newtuple '(3)))
(test (build-L3term '(new-tuple x 0)) (l3t-newtuple '(x 0)))
(test (build-L3term '(new-tuple 1 x y z)) (l3t-newtuple '(1 x y z)))

(test (build-L3term '(aref a1 0)) (l3t-aref 'a1 0))
(test (build-L3term '(aref a2 4)) (l3t-aref 'a2 4))
(test (build-L3term '(aref a3 -12)) (l3t-aref 'a3 -12))

(test (build-L3term '(aset a1 0 1)) (l3t-aset 'a1 0 1))
(test (build-L3term '(aset a2 4 x)) (l3t-aset 'a2 4 'x))
(test (build-L3term '(aset a3 -12 r1)) (l3t-aset 'a3 -12 'r1))

(test (build-L3term '(alen a1)) (l3t-alen 'a1))
(test (build-L3term '(alen a2)) (l3t-alen 'a2))

(test (build-L3term '(print 0)) (l3t-print 0))
(test (build-L3term '(print -3)) (l3t-print -3))
(test (build-L3term '(print a)) (l3t-print 'a))
(test (build-L3term '(print xyz)) (l3t-print 'xyz))

(test (build-L3term '(make-closure :_lbl1 a)) (l3t-makeclj ':_lbl1 'a))
(test (build-L3term '(make-closure :l2 v12)) (l3t-makeclj ':l2 'v12))

(test (build-L3term '(closure-proc x)) (l3t-cljproc 'x))
(test (build-L3term '(closure-proc abc)) (l3t-cljproc 'abc))

(test (build-L3term '(closure-vars x)) (l3t-cljvars 'x))
(test (build-L3term '(closure-vars abc)) (l3t-cljvars 'abc))

(test (build-L3term '(closure-vars x)) (l3t-cljvars 'x))
(test (build-L3term '(closure-vars abc)) (l3t-cljvars 'abc))

(test (build-L3term 'x) (l3t-v 'x))
(test (build-L3term 'a) (l3t-v 'a))
(test (build-L3term 5) (l3t-v 5))
(test (build-L3term -12) (l3t-v -12))
(test (build-L3term ':lbl) (l3t-v ':lbl))
(test (build-L3term ':_lbl) (l3t-v ':_lbl))
(test (build-L3term ':_fn2__) (l3t-v ':_fn2__))

(test (build-L3expr '(let ([f 5]) f))
      (l3e-let 'f (l3t-v 5) (l3e-t (l3t-v 'f))))
(test (build-L3expr '(let ([g (+ 4 5)]) g))
      (l3e-let 'g (l3t-biop '+ 4 5) (l3e-t (l3t-v 'g))))

(test (build-L3expr '(if 0 1 2))
      (l3e-if 0 (l3e-t (l3t-v 1)) (l3e-t (l3t-v 2))))
(test (build-L3expr '(if a 3 z))
      (l3e-if 'a (l3e-t (l3t-v 3)) (l3e-t (l3t-v 'z))))
(test (build-L3expr '(if g (if b 0 y) (if c z 1)))
      (l3e-if 'g
              (l3e-if 'b (l3e-t (l3t-v 0)) (l3e-t (l3t-v 'y)))
              (l3e-if 'c (l3e-t (l3t-v 'z)) (l3e-t (l3t-v 1)))))

(test (build-L3expr 0) (l3e-t (l3t-v 0)))
(test (build-L3expr 4) (l3e-t (l3t-v 4)))
(test (build-L3expr -121) (l3e-t (l3t-v -121)))
(test (build-L3expr 'a) (l3e-t (l3t-v 'a)))
(test (build-L3expr 'z) (l3e-t (l3t-v 'z)))
(test (build-L3expr ':lbl1) (l3e-t (l3t-v ':lbl1)))

(printf "tests completed~n")
