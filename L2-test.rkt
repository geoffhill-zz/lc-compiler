#lang plai
(print-only-errors #t)

;;; EECS 322 L2->L1 Compiler -- Test
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "types.rkt"))
(require (file "L2.rkt"))

;; parse assignment statements
(test (build-L2stmt '(eax <- 5)) (l2s-assign 'eax 5))
(test (build-L2stmt '(v11 <- -12)) (l2s-assign 'v11 -12))
(test (build-L2stmt '(esi <- :fn1)) (l2s-assign 'esi ':fn1))
(test (build-L2stmt '(tt8 <- eax)) (l2s-assign 'tt8 'eax))

;; parse memory get operations
(test (build-L2stmt '(y6 <- (mem ebp -8))) (l2s-memget 'y6 'ebp -8))
(test (build-L2stmt '(eax <- (mem v12 20))) (l2s-memget 'eax 'v12 20))
(test (build-L2stmt '(cre <- (mem cre 0))) (l2s-memget 'cre 'cre 0))

;; parse memory set operations
(test (build-L2stmt '((mem ebp -12) <- 31)) (l2s-memset 'ebp -12 31))
(test (build-L2stmt '((mem t8 4) <- v1)) (l2s-memset 't8 4 'v1))
(test (build-L2stmt '((mem ds0 0) <- ds0)) (l2s-memset 'ds0 0 'ds0))

;; parse arithmetic operations
(test (build-L2stmt '(eax += ebx)) (l2s-aop 'eax '+= 'ebx))
(test (build-L2stmt '(esi -= eax)) (l2s-aop 'esi '-= 'eax))
(test (build-L2stmt '(edi *= edx)) (l2s-aop 'edi '*= 'edx))
(test (build-L2stmt '(eax &= eax)) (l2s-aop 'eax '&= 'eax))
(test (build-L2stmt '(t14 += -5)) (l2s-aop 't14 '+= -5))
(test (build-L2stmt '(v01 *= 12)) (l2s-aop 'v01 '*= 12))
(test (build-L2stmt '(e_1 -= 7)) (l2s-aop 'e_1 '-= 7))
(test (build-L2stmt '(v_2 &= 11)) (l2s-aop 'v_2 '&= 11))

;; parse shift operations
(test (build-L2stmt '(eax <<= ebx)) (l2s-sop 'eax '<<= 'ebx))
(test (build-L2stmt '(esi <<= eax)) (l2s-sop 'esi '<<= 'eax))
(test (build-L2stmt '(edi >>= edx)) (l2s-sop 'edi '>>= 'edx))
(test (build-L2stmt '(eax >>= eax)) (l2s-sop 'eax '>>= 'eax))
(test (build-L2stmt '(t14 <<= 2)) (l2s-sop 't14 '<<= 2))
(test (build-L2stmt '(v01 <<= 3)) (l2s-sop 'v01 '<<= 3))
(test (build-L2stmt '(e_1 >>= 5)) (l2s-sop 'e_1 '>>= 5))
(test (build-L2stmt '(v_2 >>= 0)) (l2s-sop 'v_2 '>>= 0))

;; parse stored comparison statements
(test (build-L2stmt '(t07 <- ecx < ebx)) (l2s-cmp 't07 'ecx '< 'ebx))
(test (build-L2stmt '(var <- r12 <= tt5)) (l2s-cmp 'var 'r12 '<= 'tt5))
(test (build-L2stmt '(e12 <- eax = edi)) (l2s-cmp 'e12 'eax '= 'edi))
(test (build-L2stmt '(yy6 <- 6 < ebx)) (l2s-cmp 'yy6 6 '< 'ebx))
(test (build-L2stmt '(zz1 <- t87 <= 4)) (l2s-cmp 'zz1 't87 '<= 4))
(test (build-L2stmt '(ebx <- 2 = w_1)) (l2s-cmp 'ebx 2 '= 'w_1))
(test (build-L2stmt '(edi <- 9 < -8)) (l2s-cmp 'edi 9 '< -8))
(test (build-L2stmt '(eax <- -2 <= 1)) (l2s-cmp 'eax -2 '<= 1))
(test (build-L2stmt '(k09 <- 4 = 7)) (l2s-cmp 'k09 4 '= 7))
(test (build-L2stmt '(e45 <- 5 = 5)) (l2s-cmp 'e45 5 '= 5))

;; parse label statements
(test (build-L2stmt ':some_fn1) (l2s-label ':some_fn1))
(test (build-L2stmt ':__3fn_) (l2s-label ':__3fn_))
(test (build-L2stmt ':eax) (l2s-label ':eax))

;; parse goto statements
(test (build-L2stmt '(goto :some_fn1)) (l2s-goto ':some_fn1))
(test (build-L2stmt '(goto :__3fn_)) (l2s-goto ':__3fn_))
(test (build-L2stmt '(goto :eax)) (l2s-goto ':eax))

;; parse conditional jumps
(test (build-L2stmt '(cjump ecx < ebx :start1 :end1)) (l2s-cjump 'ecx '< 'ebx ':start1 ':end1))
(test (build-L2stmt '(cjump r12 <= tt5 :a1 :b1)) (l2s-cjump 'r12 '<= 'tt5 ':a1 ':b1))
(test (build-L2stmt '(cjump eax = edi :cc01 :cc02)) (l2s-cjump 'eax '= 'edi ':cc01 ':cc02))
(test (build-L2stmt '(cjump 6 < ebx :zy :zz)) (l2s-cjump 6 '< 'ebx ':zy ':zz))
(test (build-L2stmt '(cjump t87 <= 4 :_tmp1 :_tmp2)) (l2s-cjump 't87 '<= 4 ':_tmp1 ':_tmp2))
(test (build-L2stmt '(cjump 2 = w_1 :t6 :t7)) (l2s-cjump 2 '= 'w_1 ':t6 ':t7))
(test (build-L2stmt '(cjump 9 < -8 :tr1 :tr2)) (l2s-cjump 9 '< -8 ':tr1 ':tr2))
(test (build-L2stmt '(cjump -2 <= 1 :y0 :y1)) (l2s-cjump -2 '<= 1 ':y0 ':y1))
(test (build-L2stmt '(cjump 4 = 7 :s3 :s4)) (l2s-cjump 4 '= 7 ':s3 ':s4))
(test (build-L2stmt '(cjump 5 = 5 :ap12 :ap13)) (l2s-cjump 5 '= 5 ':ap12 ':ap13))

;; parse function calls
(test (build-L2stmt '(call :some_fn5)) (l2s-call ':some_fn5))
(test (build-L2stmt '(call eax)) (l2s-call 'eax))
(test (build-L2stmt '(call t12)) (l2s-call 't12))

;; parse tail calls
(test (build-L2stmt '(tail-call :some_fn5)) (l2s-tcall ':some_fn5))
(test (build-L2stmt '(tail-call eax)) (l2s-tcall 'eax))
(test (build-L2stmt '(tail-call t12)) (l2s-tcall 't12))

;; parse return statements
(test (build-L2stmt '(return)) (l2s-return))

;; parse print statements
(test (build-L2stmt '(eax <- (print 11))) (l2s-print 'eax 11))
(test (build-L2stmt '(eax <- (print esi))) (l2s-print 'eax 'esi))
(test (build-L2stmt '(eax <- (print t_9))) (l2s-print 'eax 't_9))

;; parse allocate statements
(test (build-L2stmt '(eax <- (allocate eax t43))) (l2s-alloc 'eax 'eax 't43))
(test (build-L2stmt '(eax <- (allocate esi 8))) (l2s-alloc 'eax 'esi 8))
(test (build-L2stmt '(eax <- (allocate t89 12))) (l2s-alloc 'eax 't89 12))
(test (build-L2stmt '(eax <- (allocate 10 edi))) (l2s-alloc 'eax 10 'edi))
(test (build-L2stmt '(eax <- (allocate 7 r4))) (l2s-alloc 'eax 7 'r4))
(test (build-L2stmt '(eax <- (allocate 8 8))) (l2s-alloc 'eax 8 8))

;; parse array error statements
(test (build-L2stmt '(eax <- (array-error eax t43))) (l2s-arrayerr 'eax 'eax 't43))
(test (build-L2stmt '(eax <- (array-error esi 8))) (l2s-arrayerr 'eax 'esi 8))
(test (build-L2stmt '(eax <- (array-error t89 12))) (l2s-arrayerr 'eax 't89 12))
(test (build-L2stmt '(eax <- (array-error 10 edi))) (l2s-arrayerr 'eax 10 'edi))
(test (build-L2stmt '(eax <- (array-error 7 r4))) (l2s-arrayerr 'eax 7 'r4))
(test (build-L2stmt '(eax <- (array-error 8 8))) (l2s-arrayerr 'eax 8 8))

;; parse bad statement
(test/exn (build-L2stmt '(eax <-)) "no matching clause")
(test/exn (build-L2stmt '((mem ebp -9) <- t12)) "no matching clause")
(test/exn (build-L2stmt '((mem ebp -12) <- (mem ebp -8))) "no matching clause")
(test/exn (build-L2stmt '(eax <- (print 5 5))) "no matching clause")

;; assignment gen/kill/var sets
(test (gen (build-L2stmt '(a <- 5))) (set))
(test (gen (build-L2stmt '(b <- :v1))) (set))
(test (gen (build-L2stmt '(c <- v1))) (set 'v1))
(test (kill (build-L2stmt '(a <- 5))) (set 'a))
(test (kill (build-L2stmt '(b <- :v1))) (set 'b))
(test (kill (build-L2stmt '(c <- v1))) (set 'c))
(test (var (build-L2stmt '(a <- 5))) (set 'a))
(test (var (build-L2stmt '(b <- :v1))) (set 'b))
(test (var (build-L2stmt '(c <- v1))) (set 'c 'v1))

;; memory reference gen/kill sets
(test (gen (build-L2stmt '(a <- (mem ebp -4)))) (set 'ebp))
(test (gen (build-L2stmt '((mem ebp -8) <- b))) (set 'ebp 'b))
(test (kill (build-L2stmt '(a <- (mem ebp -4)))) (set 'a))
(test (kill (build-L2stmt '((mem ebp -8) <- b))) (set))
(test (var (build-L2stmt '(a <- (mem ebp -4)))) (set 'a 'ebp))
(test (var (build-L2stmt '((mem ebp -8) <- b))) (set 'ebp 'b))

;; arithmetic ops gen/kill sets
(test (gen (build-L2stmt '(st += 5))) (set 'st))
(test (gen (build-L2stmt '(yz += aaa))) (set 'aaa 'yz))
(test (gen (build-L2stmt '(pc += pc))) (set 'pc))
(test (gen (build-L2stmt '(st -= 10))) (set 'st))
(test (gen (build-L2stmt '(yz -= zzz))) (set 'yz 'zzz))
(test (gen (build-L2stmt '(gz -= gz))) (set 'gz))
(test (gen (build-L2stmt '(cc *= 2))) (set 'cc))
(test (gen (build-L2stmt '(dd *= y6))) (set 'dd 'y6))
(test (gen (build-L2stmt '(l1 *= l1))) (set 'l1))
(test (gen (build-L2stmt '(ee &= 1))) (set 'ee))
(test (gen (build-L2stmt '(ff &= t3))) (set 'ff 't3))
(test (gen (build-L2stmt '(gg &= gg))) (set 'gg))
(test (kill (build-L2stmt '(st += 5))) (set 'st))
(test (kill (build-L2stmt '(yz += aaa))) (set 'yz))
(test (kill (build-L2stmt '(pc += pc))) (set 'pc))
(test (kill (build-L2stmt '(st -= 10))) (set 'st))
(test (kill (build-L2stmt '(yz -= zzz))) (set 'yz))
(test (kill (build-L2stmt '(gz -= gz))) (set 'gz))
(test (kill (build-L2stmt '(cc *= 2))) (set 'cc))
(test (kill (build-L2stmt '(dd *= y6))) (set 'dd))
(test (kill (build-L2stmt '(l1 *= l1))) (set 'l1))
(test (kill (build-L2stmt '(ee &= 1))) (set 'ee))
(test (kill (build-L2stmt '(ff &= t3))) (set 'ff))
(test (kill (build-L2stmt '(gg &= gg))) (set 'gg))
(test (var (build-L2stmt '(st += 5))) (set 'st))
(test (var (build-L2stmt '(yz += aaa))) (set 'yz 'aaa))
(test (var (build-L2stmt '(pc += pc))) (set 'pc))
(test (var (build-L2stmt '(st -= 10))) (set 'st))
(test (var (build-L2stmt '(yz -= zzz))) (set 'yz 'zzz))
(test (var (build-L2stmt '(gz -= gz))) (set 'gz))
(test (var (build-L2stmt '(cc *= 2))) (set 'cc))
(test (var (build-L2stmt '(dd *= y6))) (set 'dd 'y6))
(test (var (build-L2stmt '(l1 *= l1))) (set 'l1))
(test (var (build-L2stmt '(ee &= 1))) (set 'ee))
(test (var (build-L2stmt '(ff &= t3))) (set 'ff 't3))
(test (var (build-L2stmt '(gg &= gg))) (set 'gg))

;; shift ops gen/kill sets
(test (gen (build-L2stmt '(w <<= 1))) (set 'w))
(test (gen (build-L2stmt '(y <<= z))) (set 'y 'z))
(test (gen (build-L2stmt '(a >>= 3))) (set 'a))
(test (gen (build-L2stmt '(c >>= b))) (set 'b 'c))
(test (kill (build-L2stmt '(w <<= 1))) (set 'w))
(test (kill (build-L2stmt '(y <<= z))) (set 'y))
(test (kill (build-L2stmt '(a >>= 3))) (set 'a))
(test (kill (build-L2stmt '(c >>= b))) (set 'c))
(test (var (build-L2stmt '(w <<= 1))) (set 'w))
(test (var (build-L2stmt '(y <<= z))) (set 'y 'z))
(test (var (build-L2stmt '(a >>= 3))) (set 'a))
(test (var (build-L2stmt '(c >>= b))) (set 'c 'b))

;; stored comparsion gen/kill sets
(test (gen (build-L2stmt '(ecx <- 5 < 3))) (set))
(test (gen (build-L2stmt '(ecx <- x <= 3))) (set 'x))
(test (gen (build-L2stmt '(ecx <- 5 = y))) (set 'y))
(test (gen (build-L2stmt '(ecx <- v < u))) (set 'u 'v))
(test (kill (build-L2stmt '(ecx <- 5 < 3))) (set 'ecx))
(test (kill (build-L2stmt '(ecx <- x <= 3))) (set 'ecx))
(test (kill (build-L2stmt '(ecx <- 5 = y))) (set 'ecx))
(test (kill (build-L2stmt '(ecx <- u < v))) (set 'ecx))
(test (var (build-L2stmt '(ecx <- 5 < 3))) (set 'ecx))
(test (var (build-L2stmt '(ecx <- x <= 3))) (set 'ecx 'x))
(test (var (build-L2stmt '(ecx <- 5 = y))) (set 'ecx 'y))
(test (var (build-L2stmt '(ecx <- u < v))) (set 'ecx 'u 'v))

;; labels gen/kill sets
(test (gen (build-L2stmt ':some_fn)) (set))
(test (gen (build-L2stmt ':_2)) (set))
(test (kill (build-L2stmt ':some_fn)) (set))
(test (kill (build-L2stmt ':_2)) (set))
(test (var (build-L2stmt ':some_fn)) (set))
(test (var (build-L2stmt ':_2)) (set))

;; gotos gen/kill sets
(test (gen (build-L2stmt '(goto :lb_1))) (set))
(test (kill (build-L2stmt '(goto :lb_1))) (set))
(test (var (build-L2stmt '(goto :lb_1))) (set))

;; conditional jumps gen/kill sets
(test (gen (build-L2stmt '(cjump 4 <= 5 :fn_a :fn_b))) (set))
(test (gen (build-L2stmt '(cjump x < 8 :fn_a :fn_b))) (set 'x))
(test (gen (build-L2stmt '(cjump 9 = y :fn_a :fn_b))) (set 'y))
(test (gen (build-L2stmt '(cjump i <= a :fn_a :fn_b))) (set 'i 'a))
(test (kill (build-L2stmt '(cjump 4 <= 5 :fn_a :fn_b))) (set))
(test (kill (build-L2stmt '(cjump x < 8 :fn_a :fn_b))) (set))
(test (kill (build-L2stmt '(cjump 9 = y :fn_a :fn_b))) (set))
(test (kill (build-L2stmt '(cjump i <= a :fn_a :fn_b))) (set))
(test (var (build-L2stmt '(cjump 4 <= 5 :fn_a :fn_b))) (set))
(test (var (build-L2stmt '(cjump x < 8 :fn_a :fn_b))) (set 'x))
(test (var (build-L2stmt '(cjump 9 = y :fn_a :fn_b))) (set 'y))
(test (var (build-L2stmt '(cjump i <= a :fn_a :fn_b))) (set 'i 'a))

;; function calls gen/kill sets
(test (gen (build-L2stmt '(call t1))) (set 't1 'eax 'ecx 'edx))
(test (gen (build-L2stmt '(call :l1))) (set 'eax 'ecx 'edx))
(test (kill (build-L2stmt '(call t1))) (set 'eax 'ebx 'ecx 'edx))
(test (kill (build-L2stmt '(call :l1))) (set 'eax 'ebx 'ecx 'edx))
(test (var (build-L2stmt '(call t1))) (set 't1 'eax 'ebx 'ecx 'edx))
(test (var (build-L2stmt '(call :l1))) (set 'eax 'ebx 'ecx 'edx))


;; tail calls gen/kill sets
(test (gen (build-L2stmt '(tail-call t1))) (set 'eax 'ecx 'edi 'edx 'esi 't1))
(test (gen (build-L2stmt '(tail-call :l1))) (set 'eax 'ecx 'edi 'edx 'esi))
(test (kill (build-L2stmt '(tail-call t1))) (set))
(test (kill (build-L2stmt '(tail-call :l1))) (set))
(test (var (build-L2stmt '(tail-call t1))) (set 'eax 'ecx 'edi 'edx 'esi 't1))
(test (var (build-L2stmt '(tail-call :l1))) (set 'eax 'ecx 'edi 'edx 'esi))

;; return statement gen/kill sets
(test (gen (build-L2stmt '(return))) (set 'eax 'edi 'esi))
(test (kill (build-L2stmt '(return))) (set 'eax 'ecx 'edx))
(test (var (build-L2stmt '(return))) (set 'eax 'ecx 'edx 'edi 'esi))

;; c runtime calls gen/kill sets
(test (gen (build-L2stmt '(eax <- (print 5)))) (set))
(test (gen (build-L2stmt '(eax <- (print g54)))) (set 'g54))
(test (gen (build-L2stmt '(eax <- (allocate 5 10)))) (set))
(test (gen (build-L2stmt '(eax <- (allocate x 8)))) (set 'x))
(test (gen (build-L2stmt '(eax <- (allocate 4 y)))) (set 'y))
(test (gen (build-L2stmt '(eax <- (allocate r5 r6)))) (set 'r5 'r6))
(test (gen (build-L2stmt '(eax <- (array-error 12 11)))) (set))
(test (gen (build-L2stmt '(eax <- (array-error b 7)))) (set 'b))
(test (gen (build-L2stmt '(eax <- (array-error 1 c)))) (set 'c))
(test (gen (build-L2stmt '(eax <- (array-error q p)))) (set 'p 'q))
(test (kill (build-L2stmt '(eax <- (print 5)))) (set 'eax 'ecx 'edx))
(test (kill (build-L2stmt '(eax <- (print g54)))) (set 'eax 'ecx 'edx))
(test (kill (build-L2stmt '(eax <- (allocate 5 10)))) (set 'eax 'ecx 'edx))
(test (kill (build-L2stmt '(eax <- (allocate x 8)))) (set 'eax 'ecx 'edx))
(test (kill (build-L2stmt '(eax <- (allocate 4 y)))) (set 'eax 'ecx 'edx))
(test (kill (build-L2stmt '(eax <- (allocate r5 r6)))) (set 'eax 'ecx 'edx))
(test (kill (build-L2stmt '(eax <- (array-error 12 11)))) (set 'eax 'ecx 'edx))
(test (kill (build-L2stmt '(eax <- (array-error b 7)))) (set 'eax 'ecx 'edx))
(test (kill (build-L2stmt '(eax <- (array-error 1 c)))) (set 'eax 'ecx 'edx))
(test (kill (build-L2stmt '(eax <- (array-error q p)))) (set 'eax 'ecx 'edx))
(test (var (build-L2stmt '(eax <- (print 5)))) (set 'eax 'ecx 'edx))
(test (var (build-L2stmt '(eax <- (print g54)))) (set 'eax 'g54 'ecx 'edx))
(test (var (build-L2stmt '(eax <- (allocate 5 10)))) (set 'eax 'ecx 'edx))
(test (var (build-L2stmt '(eax <- (allocate x 8)))) (set 'eax 'x 'ecx 'edx))
(test (var (build-L2stmt '(eax <- (allocate 4 y)))) (set 'eax 'y 'ecx 'edx))
(test (var (build-L2stmt '(eax <- (allocate r5 r6)))) (set 'eax 'r5 'r6 'ecx 'edx))
(test (var (build-L2stmt '(eax <- (array-error 12 11)))) (set 'eax 'ecx 'edx))
(test (var (build-L2stmt '(eax <- (array-error b 7)))) (set 'eax 'b 'ecx 'edx))
(test (var (build-L2stmt '(eax <- (array-error 1 c)))) (set 'eax 'c 'ecx 'edx))
(test (var (build-L2stmt '(eax <- (array-error q p)))) (set 'eax 'q 'p 'ecx 'edx))

;; example gen and kill from slides
;; modified for correct return behavior
(test (gen (build-L2stmt ':f)) (set))
(test (gen (build-L2stmt '(x2 <- eax))) (set 'eax))
(test (gen (build-L2stmt '(x2 *= x2))) (set 'x2))
(test (gen (build-L2stmt '(2x2 <- x2))) (set 'x2))
(test (gen (build-L2stmt '(2x2 *= 2))) (set '2x2))
(test (gen (build-L2stmt '(3x <- eax))) (set 'eax))
(test (gen (build-L2stmt '(3x *= 3))) (set '3x))
(test (gen (build-L2stmt '(eax <- 2x2))) (set '2x2))
(test (gen (build-L2stmt '(eax += 3x))) (set '3x 'eax))
(test (gen (build-L2stmt '(eax += 4))) (set 'eax))
(test (gen (build-L2stmt '(return))) (set 'eax 'edi 'esi))
(test (kill (build-L2stmt ':f)) (set))
(test (kill (build-L2stmt '(x2 <- eax))) (set 'x2))
(test (kill (build-L2stmt '(x2 *= x2))) (set 'x2))
(test (kill (build-L2stmt '(2x2 <- x2))) (set '2x2))
(test (kill (build-L2stmt '(2x2 *= 2))) (set '2x2))
(test (kill (build-L2stmt '(3x <- eax))) (set '3x))
(test (kill (build-L2stmt '(3x *= 3))) (set '3x))
(test (kill (build-L2stmt '(eax <- 2x2))) (set 'eax))
(test (kill (build-L2stmt '(eax += 3x))) (set 'eax))
(test (kill (build-L2stmt '(eax += 4))) (set 'eax))
(test (kill (build-L2stmt '(return))) (set 'eax 'ecx 'edx))

(test (build-l2reg-base
       (build-L2fn '()))
      (l2reg-base
       `#()
       '#hash()))
(test (build-l2reg-base
       (build-L2fn '((eax <- 5))))
      (l2reg-base
       `#(,(l2s-assign 'eax 5))
       '#hash()))
(test (build-l2reg-base
       (build-L2fn '(:some_lbl)))
      (l2reg-base
       `#(,(l2s-label ':some_lbl))
       '#hash((:some_lbl . 0))))
(test (build-l2reg-base
       (build-L2fn '(:tr :gr :cr (eax <- 5) (edx <- 1) :hg (edx += eax))))
      (l2reg-base
       `#(,(l2s-label ':tr)
          ,(l2s-label ':gr)
          ,(l2s-label ':cr)
          ,(l2s-assign 'eax 5)
          ,(l2s-assign 'edx 1)
          ,(l2s-label ':hg)
          ,(l2s-aop 'edx '+= 'eax))
       '#hash((:hg . 5) (:cr . 2) (:gr . 1) (:tr . 0))))

(test (build-l2reg-succ
       (build-l2reg-base
        (build-L2fn '())))
      (l2reg-succ
       `#()
       `#()))
(test (build-l2reg-succ
       (build-l2reg-base
        (build-L2fn '((eax <- 5)))))
      (l2reg-succ
       `#(,(l2s-assign 'eax 5))
       `#(,(set))))
(test (build-l2reg-succ
       (build-l2reg-base
        (build-L2fn '(:some_lbl))))
      (l2reg-succ
       `#(,(l2s-label ':some_lbl))
       `#(,(set))))
(test (build-l2reg-succ
       (build-l2reg-base
        (build-L2fn '(:tr :gr :cr (eax <- 5) (edx <- 1) :hg (edx += eax)))))
      (l2reg-succ
       `#(,(l2s-label ':tr)
          ,(l2s-label ':gr)
          ,(l2s-label ':cr)
          ,(l2s-assign 'eax 5)
          ,(l2s-assign 'edx 1)
          ,(l2s-label ':hg)
          ,(l2s-aop 'edx '+= 'eax))
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

(test (fix-stack `#(,(l2s-assign 't 5) ,(l2s-assign 'eax 't)) 0)
      `#(,(l2s-assign 't 5)
         ,(l2s-assign 'eax 't)))
(test (fix-stack `#(,(l2s-assign 't 5) ,(l2s-assign 'eax 't)) -4)
      `#(,(l2s-aop 'esp '-= 4)
         ,(l2s-assign 't 5)
         ,(l2s-assign 'eax 't)
         ,(l2s-aop 'esp '+= 4)))
(test (fix-stack `#(,(l2s-assign 't 5) ,(l2s-assign 'eax 't)) -32)
      `#(,(l2s-aop 'esp '-= 32)
         ,(l2s-assign 't 5)
         ,(l2s-assign 'eax 't)
         ,(l2s-aop 'esp '+= 32)))
(test (fix-stack `#(,(l2s-label ':fn1) ,(l2s-assign 'g 2) ,(l2s-assign 'eax 'g)) 0)
      `#(,(l2s-label ':fn1)
         ,(l2s-assign 'g 2)
         ,(l2s-assign 'eax 'g)))
(test (fix-stack `#(,(l2s-label ':fn1) ,(l2s-assign 'g 2) ,(l2s-assign 'eax 'g)) -4)
      `#(,(l2s-label ':fn1)
         ,(l2s-aop 'esp '-= 4)
         ,(l2s-assign 'g 2)
         ,(l2s-assign 'eax 'g)
         ,(l2s-aop 'esp '+= 4)))
(test (fix-stack `#(,(l2s-label ':fn1) ,(l2s-assign 'g 2) ,(l2s-assign 'eax 'g)) -32)
      `#(,(l2s-label ':fn1)
         ,(l2s-aop 'esp '-= 32)
         ,(l2s-assign 'g 2)
         ,(l2s-assign 'eax 'g)
         ,(l2s-aop 'esp '+= 32)))

(printf "tests completed~n")
