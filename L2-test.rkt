#lang plai
(print-only-errors #t)

;;; EECS 322 L2->L1 Compiler -- Test
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "L2.rkt"))

;; parse assignment statements
(test (build-stmt '(eax <- 5)) (stmt-assign 'eax 5))
(test (build-stmt '(v11 <- -12)) (stmt-assign 'v11 -12))
(test (build-stmt '(esi <- :fn1)) (stmt-assign 'esi ':fn1))
(test (build-stmt '(tt8 <- eax)) (stmt-assign 'tt8 'eax))

;; parse memory get operations
(test (build-stmt '(y6 <- (mem ebp -8))) (stmt-memget 'y6 'ebp -8))
(test (build-stmt '(eax <- (mem v12 20))) (stmt-memget 'eax 'v12 20))
(test (build-stmt '(cre <- (mem cre 0))) (stmt-memget 'cre 'cre 0))

;; parse memory set operations
(test (build-stmt '((mem ebp -12) <- 31)) (stmt-memset 'ebp -12 31))
(test (build-stmt '((mem t8 4) <- v1)) (stmt-memset 't8 4 'v1))
(test (build-stmt '((mem ds0 0) <- ds0)) (stmt-memset 'ds0 0 'ds0))

;; parse arithmetic operations
(test (build-stmt '(eax += ebx)) (stmt-aop 'eax '+= 'ebx))
(test (build-stmt '(esi -= eax)) (stmt-aop 'esi '-= 'eax))
(test (build-stmt '(edi *= edx)) (stmt-aop 'edi '*= 'edx))
(test (build-stmt '(eax &= eax)) (stmt-aop 'eax '&= 'eax))
(test (build-stmt '(t14 += -5)) (stmt-aop 't14 '+= -5))
(test (build-stmt '(v01 *= 12)) (stmt-aop 'v01 '*= 12))
(test (build-stmt '(e_1 -= 7)) (stmt-aop 'e_1 '-= 7))
(test (build-stmt '(v_2 &= 11)) (stmt-aop 'v_2 '&= 11))

;; parse shift operations
(test (build-stmt '(eax <<= ebx)) (stmt-sop 'eax '<<= 'ebx))
(test (build-stmt '(esi <<= eax)) (stmt-sop 'esi '<<= 'eax))
(test (build-stmt '(edi >>= edx)) (stmt-sop 'edi '>>= 'edx))
(test (build-stmt '(eax >>= eax)) (stmt-sop 'eax '>>= 'eax))
(test (build-stmt '(t14 <<= 2)) (stmt-sop 't14 '<<= 2))
(test (build-stmt '(v01 <<= 3)) (stmt-sop 'v01 '<<= 3))
(test (build-stmt '(e_1 >>= 5)) (stmt-sop 'e_1 '>>= 5))
(test (build-stmt '(v_2 >>= 0)) (stmt-sop 'v_2 '>>= 0))

;; parse stored comparison statements
(test (build-stmt '(t07 <- ecx < ebx)) (stmt-cmp 't07 'ecx '< 'ebx))
(test (build-stmt '(var <- r12 <= tt5)) (stmt-cmp 'var 'r12 '<= 'tt5))
(test (build-stmt '(e12 <- eax = edi)) (stmt-cmp 'e12 'eax '= 'edi))
(test (build-stmt '(yy6 <- 6 < ebx)) (stmt-cmp 'yy6 6 '< 'ebx))
(test (build-stmt '(zz1 <- t87 <= 4)) (stmt-cmp 'zz1 't87 '<= 4))
(test (build-stmt '(ebx <- 2 = w_1)) (stmt-cmp 'ebx 2 '= 'w_1))
(test (build-stmt '(edi <- 9 < -8)) (stmt-cmp 'edi 9 '< -8))
(test (build-stmt '(eax <- -2 <= 1)) (stmt-cmp 'eax -2 '<= 1))
(test (build-stmt '(k09 <- 4 = 7)) (stmt-cmp 'k09 4 '= 7))
(test (build-stmt '(e45 <- 5 = 5)) (stmt-cmp 'e45 5 '= 5))

;; parse label statements
(test (build-stmt ':some_fn1) (stmt-label ':some_fn1))
(test (build-stmt ':__3fn_) (stmt-label ':__3fn_))
(test (build-stmt ':eax) (stmt-label ':eax))

;; parse goto statements
(test (build-stmt '(goto :some_fn1)) (stmt-goto ':some_fn1))
(test (build-stmt '(goto :__3fn_)) (stmt-goto ':__3fn_))
(test (build-stmt '(goto :eax)) (stmt-goto ':eax))

;; parse conditional jumps
(test (build-stmt '(cjump ecx < ebx :start1 :end1)) (stmt-cjump 'ecx '< 'ebx ':start1 ':end1))
(test (build-stmt '(cjump r12 <= tt5 :a1 :b1)) (stmt-cjump 'r12 '<= 'tt5 ':a1 ':b1))
(test (build-stmt '(cjump eax = edi :cc01 :cc02)) (stmt-cjump 'eax '= 'edi ':cc01 ':cc02))
(test (build-stmt '(cjump 6 < ebx :zy :zz)) (stmt-cjump 6 '< 'ebx ':zy ':zz))
(test (build-stmt '(cjump t87 <= 4 :_tmp1 :_tmp2)) (stmt-cjump 't87 '<= 4 ':_tmp1 ':_tmp2))
(test (build-stmt '(cjump 2 = w_1 :t6 :t7)) (stmt-cjump 2 '= 'w_1 ':t6 ':t7))
(test (build-stmt '(cjump 9 < -8 :tr1 :tr2)) (stmt-cjump 9 '< -8 ':tr1 ':tr2))
(test (build-stmt '(cjump -2 <= 1 :y0 :y1)) (stmt-cjump -2 '<= 1 ':y0 ':y1))
(test (build-stmt '(cjump 4 = 7 :s3 :s4)) (stmt-cjump 4 '= 7 ':s3 ':s4))
(test (build-stmt '(cjump 5 = 5 :ap12 :ap13)) (stmt-cjump 5 '= 5 ':ap12 ':ap13))

;; parse function calls
(test (build-stmt '(call :some_fn5)) (stmt-call ':some_fn5))
(test (build-stmt '(call eax)) (stmt-call 'eax))
(test (build-stmt '(call t12)) (stmt-call 't12))

;; parse tail calls
(test (build-stmt '(tail-call :some_fn5)) (stmt-tcall ':some_fn5))
(test (build-stmt '(tail-call eax)) (stmt-tcall 'eax))
(test (build-stmt '(tail-call t12)) (stmt-tcall 't12))

;; parse return statements
(test (build-stmt '(return)) (stmt-return))

;; parse print statements
(test (build-stmt '(eax <- (print 11))) (stmt-print 'eax 11))
(test (build-stmt '(eax <- (print esi))) (stmt-print 'eax 'esi))
(test (build-stmt '(eax <- (print t_9))) (stmt-print 'eax 't_9))

;; parse allocate statements
(test (build-stmt '(eax <- (allocate eax t43))) (stmt-alloc 'eax 'eax 't43))
(test (build-stmt '(eax <- (allocate esi 8))) (stmt-alloc 'eax 'esi 8))
(test (build-stmt '(eax <- (allocate t89 12))) (stmt-alloc 'eax 't89 12))
(test (build-stmt '(eax <- (allocate 10 edi))) (stmt-alloc 'eax 10 'edi))
(test (build-stmt '(eax <- (allocate 7 r4))) (stmt-alloc 'eax 7 'r4))
(test (build-stmt '(eax <- (allocate 8 8))) (stmt-alloc 'eax 8 8))

;; parse array error statements
(test (build-stmt '(eax <- (array-error eax t43))) (stmt-arrayerr 'eax 'eax 't43))
(test (build-stmt '(eax <- (array-error esi 8))) (stmt-arrayerr 'eax 'esi 8))
(test (build-stmt '(eax <- (array-error t89 12))) (stmt-arrayerr 'eax 't89 12))
(test (build-stmt '(eax <- (array-error 10 edi))) (stmt-arrayerr 'eax 10 'edi))
(test (build-stmt '(eax <- (array-error 7 r4))) (stmt-arrayerr 'eax 7 'r4))
(test (build-stmt '(eax <- (array-error 8 8))) (stmt-arrayerr 'eax 8 8))

;; parse bad statement
(test/exn (build-stmt '(eax <-)) "no matching clause")
(test/exn (build-stmt '((mem ebp -9) <- t12)) "no matching clause")
(test/exn (build-stmt '((mem ebp -12) <- (mem ebp -8))) "no matching clause")
(test/exn (build-stmt '(eax <- (print 5 5))) "no matching clause")

;; assignment gen/kill/var sets
(test (gen (build-stmt '(a <- 5))) (set))
(test (gen (build-stmt '(b <- :v1))) (set))
(test (gen (build-stmt '(c <- v1))) (set 'v1))
(test (kill (build-stmt '(a <- 5))) (set 'a))
(test (kill (build-stmt '(b <- :v1))) (set 'b))
(test (kill (build-stmt '(c <- v1))) (set 'c))
(test (var (build-stmt '(a <- 5))) (set 'a))
(test (var (build-stmt '(b <- :v1))) (set 'b))
(test (var (build-stmt '(c <- v1))) (set 'c 'v1))

;; memory reference gen/kill sets
(test (gen (build-stmt '(a <- (mem ebp -4)))) (set 'ebp))
(test (gen (build-stmt '((mem ebp -8) <- b))) (set 'ebp 'b))
(test (kill (build-stmt '(a <- (mem ebp -4)))) (set 'a))
(test (kill (build-stmt '((mem ebp -8) <- b))) (set))
(test (var (build-stmt '(a <- (mem ebp -4)))) (set 'a 'ebp))
(test (var (build-stmt '((mem ebp -8) <- b))) (set 'ebp 'b))

;; arithmetic ops gen/kill sets
(test (gen (build-stmt '(st += 5))) (set 'st))
(test (gen (build-stmt '(yz += aaa))) (set 'aaa 'yz))
(test (gen (build-stmt '(pc += pc))) (set 'pc))
(test (gen (build-stmt '(st -= 10))) (set 'st))
(test (gen (build-stmt '(yz -= zzz))) (set 'yz 'zzz))
(test (gen (build-stmt '(gz -= gz))) (set 'gz))
(test (gen (build-stmt '(cc *= 2))) (set 'cc))
(test (gen (build-stmt '(dd *= y6))) (set 'dd 'y6))
(test (gen (build-stmt '(l1 *= l1))) (set 'l1))
(test (gen (build-stmt '(ee &= 1))) (set 'ee))
(test (gen (build-stmt '(ff &= t3))) (set 'ff 't3))
(test (gen (build-stmt '(gg &= gg))) (set 'gg))
(test (kill (build-stmt '(st += 5))) (set 'st))
(test (kill (build-stmt '(yz += aaa))) (set 'yz))
(test (kill (build-stmt '(pc += pc))) (set 'pc))
(test (kill (build-stmt '(st -= 10))) (set 'st))
(test (kill (build-stmt '(yz -= zzz))) (set 'yz))
(test (kill (build-stmt '(gz -= gz))) (set 'gz))
(test (kill (build-stmt '(cc *= 2))) (set 'cc))
(test (kill (build-stmt '(dd *= y6))) (set 'dd))
(test (kill (build-stmt '(l1 *= l1))) (set 'l1))
(test (kill (build-stmt '(ee &= 1))) (set 'ee))
(test (kill (build-stmt '(ff &= t3))) (set 'ff))
(test (kill (build-stmt '(gg &= gg))) (set 'gg))
(test (var (build-stmt '(st += 5))) (set 'st))
(test (var (build-stmt '(yz += aaa))) (set 'yz 'aaa))
(test (var (build-stmt '(pc += pc))) (set 'pc))
(test (var (build-stmt '(st -= 10))) (set 'st))
(test (var (build-stmt '(yz -= zzz))) (set 'yz 'zzz))
(test (var (build-stmt '(gz -= gz))) (set 'gz))
(test (var (build-stmt '(cc *= 2))) (set 'cc))
(test (var (build-stmt '(dd *= y6))) (set 'dd 'y6))
(test (var (build-stmt '(l1 *= l1))) (set 'l1))
(test (var (build-stmt '(ee &= 1))) (set 'ee))
(test (var (build-stmt '(ff &= t3))) (set 'ff 't3))
(test (var (build-stmt '(gg &= gg))) (set 'gg))

;; shift ops gen/kill sets
(test (gen (build-stmt '(w <<= 1))) (set 'w))
(test (gen (build-stmt '(y <<= z))) (set 'y 'z))
(test (gen (build-stmt '(a >>= 3))) (set 'a))
(test (gen (build-stmt '(c >>= b))) (set 'b 'c))
(test (kill (build-stmt '(w <<= 1))) (set 'w))
(test (kill (build-stmt '(y <<= z))) (set 'y))
(test (kill (build-stmt '(a >>= 3))) (set 'a))
(test (kill (build-stmt '(c >>= b))) (set 'c))
(test (var (build-stmt '(w <<= 1))) (set 'w))
(test (var (build-stmt '(y <<= z))) (set 'y 'z))
(test (var (build-stmt '(a >>= 3))) (set 'a))
(test (var (build-stmt '(c >>= b))) (set 'c 'b))

;; stored comparsion gen/kill sets
(test (gen (build-stmt '(ecx <- 5 < 3))) (set))
(test (gen (build-stmt '(ecx <- x <= 3))) (set 'x))
(test (gen (build-stmt '(ecx <- 5 = y))) (set 'y))
(test (gen (build-stmt '(ecx <- v < u))) (set 'u 'v))
(test (kill (build-stmt '(ecx <- 5 < 3))) (set 'ecx))
(test (kill (build-stmt '(ecx <- x <= 3))) (set 'ecx))
(test (kill (build-stmt '(ecx <- 5 = y))) (set 'ecx))
(test (kill (build-stmt '(ecx <- u < v))) (set 'ecx))
(test (var (build-stmt '(ecx <- 5 < 3))) (set 'ecx))
(test (var (build-stmt '(ecx <- x <= 3))) (set 'ecx 'x))
(test (var (build-stmt '(ecx <- 5 = y))) (set 'ecx 'y))
(test (var (build-stmt '(ecx <- u < v))) (set 'ecx 'u 'v))

;; labels gen/kill sets
(test (gen (build-stmt ':some_fn)) (set))
(test (gen (build-stmt ':_2)) (set))
(test (kill (build-stmt ':some_fn)) (set))
(test (kill (build-stmt ':_2)) (set))
(test (var (build-stmt ':some_fn)) (set))
(test (var (build-stmt ':_2)) (set))

;; gotos gen/kill sets
(test (gen (build-stmt '(goto :lb_1))) (set))
(test (kill (build-stmt '(goto :lb_1))) (set))
(test (var (build-stmt '(goto :lb_1))) (set))

;; conditional jumps gen/kill sets
(test (gen (build-stmt '(cjump 4 <= 5 :fn_a :fn_b))) (set))
(test (gen (build-stmt '(cjump x < 8 :fn_a :fn_b))) (set 'x))
(test (gen (build-stmt '(cjump 9 = y :fn_a :fn_b))) (set 'y))
(test (gen (build-stmt '(cjump i <= a :fn_a :fn_b))) (set 'i 'a))
(test (kill (build-stmt '(cjump 4 <= 5 :fn_a :fn_b))) (set))
(test (kill (build-stmt '(cjump x < 8 :fn_a :fn_b))) (set))
(test (kill (build-stmt '(cjump 9 = y :fn_a :fn_b))) (set))
(test (kill (build-stmt '(cjump i <= a :fn_a :fn_b))) (set))
(test (var (build-stmt '(cjump 4 <= 5 :fn_a :fn_b))) (set))
(test (var (build-stmt '(cjump x < 8 :fn_a :fn_b))) (set 'x))
(test (var (build-stmt '(cjump 9 = y :fn_a :fn_b))) (set 'y))
(test (var (build-stmt '(cjump i <= a :fn_a :fn_b))) (set 'i 'a))

;; function calls gen/kill sets
(test (gen (build-stmt '(call t1))) (set 't1 'eax 'ecx 'edx))
(test (gen (build-stmt '(call :l1))) (set 'eax 'ecx 'edx))
(test (kill (build-stmt '(call t1))) (set 'eax 'ebx 'ecx 'edx))
(test (kill (build-stmt '(call :l1))) (set 'eax 'ebx 'ecx 'edx))
(test (var (build-stmt '(call t1))) (set 't1 'eax 'ebx 'ecx 'edx))
(test (var (build-stmt '(call :l1))) (set 'eax 'ebx 'ecx 'edx))


;; tail calls gen/kill sets
(test (gen (build-stmt '(tail-call t1))) (set 'eax 'ecx 'edi 'edx 'esi 't1))
(test (gen (build-stmt '(tail-call :l1))) (set 'eax 'ecx 'edi 'edx 'esi))
(test (kill (build-stmt '(tail-call t1))) (set))
(test (kill (build-stmt '(tail-call :l1))) (set))
(test (var (build-stmt '(tail-call t1))) (set 'eax 'ecx 'edi 'edx 'esi 't1))
(test (var (build-stmt '(tail-call :l1))) (set 'eax 'ecx 'edi 'edx 'esi))

;; return statement gen/kill sets
(test (gen (build-stmt '(return))) (set 'eax 'edi 'esi))
(test (kill (build-stmt '(return))) (set 'eax 'ecx 'edx))
(test (var (build-stmt '(return))) (set 'eax 'ecx 'edx 'edi 'esi))

;; c runtime calls gen/kill sets
(test (gen (build-stmt '(eax <- (print 5)))) (set))
(test (gen (build-stmt '(eax <- (print g54)))) (set 'g54))
(test (gen (build-stmt '(eax <- (allocate 5 10)))) (set))
(test (gen (build-stmt '(eax <- (allocate x 8)))) (set 'x))
(test (gen (build-stmt '(eax <- (allocate 4 y)))) (set 'y))
(test (gen (build-stmt '(eax <- (allocate r5 r6)))) (set 'r5 'r6))
(test (gen (build-stmt '(eax <- (array-error 12 11)))) (set))
(test (gen (build-stmt '(eax <- (array-error b 7)))) (set 'b))
(test (gen (build-stmt '(eax <- (array-error 1 c)))) (set 'c))
(test (gen (build-stmt '(eax <- (array-error q p)))) (set 'p 'q))
(test (kill (build-stmt '(eax <- (print 5)))) (set 'eax 'ecx 'edx))
(test (kill (build-stmt '(eax <- (print g54)))) (set 'eax 'ecx 'edx))
(test (kill (build-stmt '(eax <- (allocate 5 10)))) (set 'eax 'ecx 'edx))
(test (kill (build-stmt '(eax <- (allocate x 8)))) (set 'eax 'ecx 'edx))
(test (kill (build-stmt '(eax <- (allocate 4 y)))) (set 'eax 'ecx 'edx))
(test (kill (build-stmt '(eax <- (allocate r5 r6)))) (set 'eax 'ecx 'edx))
(test (kill (build-stmt '(eax <- (array-error 12 11)))) (set 'eax 'ecx 'edx))
(test (kill (build-stmt '(eax <- (array-error b 7)))) (set 'eax 'ecx 'edx))
(test (kill (build-stmt '(eax <- (array-error 1 c)))) (set 'eax 'ecx 'edx))
(test (kill (build-stmt '(eax <- (array-error q p)))) (set 'eax 'ecx 'edx))
(test (var (build-stmt '(eax <- (print 5)))) (set 'eax 'ecx 'edx))
(test (var (build-stmt '(eax <- (print g54)))) (set 'eax 'g54 'ecx 'edx))
(test (var (build-stmt '(eax <- (allocate 5 10)))) (set 'eax 'ecx 'edx))
(test (var (build-stmt '(eax <- (allocate x 8)))) (set 'eax 'x 'ecx 'edx))
(test (var (build-stmt '(eax <- (allocate 4 y)))) (set 'eax 'y 'ecx 'edx))
(test (var (build-stmt '(eax <- (allocate r5 r6)))) (set 'eax 'r5 'r6 'ecx 'edx))
(test (var (build-stmt '(eax <- (array-error 12 11)))) (set 'eax 'ecx 'edx))
(test (var (build-stmt '(eax <- (array-error b 7)))) (set 'eax 'b 'ecx 'edx))
(test (var (build-stmt '(eax <- (array-error 1 c)))) (set 'eax 'c 'ecx 'edx))
(test (var (build-stmt '(eax <- (array-error q p)))) (set 'eax 'q 'p 'ecx 'edx))

;; example gen and kill from slides
;; modified for correct return behavior
(test (gen (build-stmt ':f)) (set))
(test (gen (build-stmt '(x2 <- eax))) (set 'eax))
(test (gen (build-stmt '(x2 *= x2))) (set 'x2))
(test (gen (build-stmt '(2x2 <- x2))) (set 'x2))
(test (gen (build-stmt '(2x2 *= 2))) (set '2x2))
(test (gen (build-stmt '(3x <- eax))) (set 'eax))
(test (gen (build-stmt '(3x *= 3))) (set '3x))
(test (gen (build-stmt '(eax <- 2x2))) (set '2x2))
(test (gen (build-stmt '(eax += 3x))) (set '3x 'eax))
(test (gen (build-stmt '(eax += 4))) (set 'eax))
(test (gen (build-stmt '(return))) (set 'eax 'edi 'esi))
(test (kill (build-stmt ':f)) (set))
(test (kill (build-stmt '(x2 <- eax))) (set 'x2))
(test (kill (build-stmt '(x2 *= x2))) (set 'x2))
(test (kill (build-stmt '(2x2 <- x2))) (set '2x2))
(test (kill (build-stmt '(2x2 *= 2))) (set '2x2))
(test (kill (build-stmt '(3x <- eax))) (set '3x))
(test (kill (build-stmt '(3x *= 3))) (set '3x))
(test (kill (build-stmt '(eax <- 2x2))) (set 'eax))
(test (kill (build-stmt '(eax += 3x))) (set 'eax))
(test (kill (build-stmt '(eax += 4))) (set 'eax))
(test (kill (build-stmt '(return))) (set 'eax 'ecx 'edx))

(test (build-l2fn-base '())
              (l2fn-base
               `#()
               '#hash()))
(test (build-l2fn-base '((eax <- 5)))
              (l2fn-base
               `#(,(stmt-assign 'eax 5))
               '#hash()))
(test (build-l2fn-base '(:some_lbl))
              (l2fn-base
               `#(,(stmt-label ':some_lbl))
               '#hash((:some_lbl . 0))))
(test (build-l2fn-base '(:tr :gr :cr (eax <- 5) (edx <- 1) :hg (edx += eax)))
              (l2fn-base
               `#(,(stmt-label ':tr)
                  ,(stmt-label ':gr)
                  ,(stmt-label ':cr)
                  ,(stmt-assign 'eax 5)
                  ,(stmt-assign 'edx 1)
                  ,(stmt-label ':hg)
                  ,(stmt-aop 'edx '+= 'eax))
               '#hash((:hg . 5) (:cr . 2) (:gr . 1) (:tr . 0))))

(test (build-l2fn-succ
               (build-l2fn-base '()))
              (l2fn-succ
               `#()
               `#()))
(test (build-l2fn-succ
               (build-l2fn-base '((eax <- 5))))
              (l2fn-succ
               `#(,(stmt-assign 'eax 5))
               `#(,(set))))
(test (build-l2fn-succ
               (build-l2fn-base '(:some_lbl)))
              (l2fn-succ
               `#(,(stmt-label ':some_lbl))
               `#(,(set))))
(test (build-l2fn-succ
               (build-l2fn-base '(:tr :gr :cr (eax <- 5) (edx <- 1) :hg (edx += eax))))
              (l2fn-succ
               `#(,(stmt-label ':tr)
                  ,(stmt-label ':gr)
                  ,(stmt-label ':cr)
                  ,(stmt-assign 'eax 5)
                  ,(stmt-assign 'edx 1)
                  ,(stmt-label ':hg)
                  ,(stmt-aop 'edx '+= 'eax))
               `#(,(set 1) ,(set 2) ,(set 3) ,(set 4) ,(set 5) ,(set 6) ,(set))))

(test (reg-from-edge (set 'eax 'v1)) 'eax)
(test (reg-from-edge (set 'ebx 'v1)) 'ebx)
(test (reg-from-edge (set 'ecx 'v1)) 'ecx)
(test (reg-from-edge (set 'edx 'v1)) 'edx)
(test (reg-from-edge (set 'edi 'v1)) 'edi)
(test (reg-from-edge (set 'esi 'v1)) 'esi)
(test (reg-from-edge (set 'a5 'eax)) 'eax)
(test (reg-from-edge (set 'a5 'ebx)) 'ebx)
(test (reg-from-edge (set 'a5 'ecx)) 'ecx)
(test (reg-from-edge (set 'a5 'edx)) 'edx)
(test (reg-from-edge (set 'a5 'edi)) 'edi)
(test (reg-from-edge (set 'a5 'esi)) 'esi)
(test (reg-from-edge (set 'eax 'g6)) 'eax)
(test (reg-from-edge (set 'ebx 'f7)) 'ebx)
(test (reg-from-edge (set 'mter 'ecx)) 'ecx)
(test (reg-from-edge (set 'y93 'edx)) 'edx)
(test (reg-from-edge (set 'et1 'edi)) 'edi)
(test (reg-from-edge (set 'edi 'g91i)) 'edi)

(printf "tests completed~n")
