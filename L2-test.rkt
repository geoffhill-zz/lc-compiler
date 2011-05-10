#lang plai

;;; EECS 322 L2->L1 Compiler -- Test
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require rackunit)
(require (file "L2.rkt"))

;; parse assignment statements
(check-equal? (build-stmt '(eax <- 5)) (stmt-assign 'eax 5))
(check-equal? (build-stmt '(v11 <- -12)) (stmt-assign 'v11 -12))
(check-equal? (build-stmt '(esi <- :fn1)) (stmt-assign 'esi ':fn1))
(check-equal? (build-stmt '(tt8 <- eax)) (stmt-assign 'tt8 'eax))

;; parse memory get operations
(check-equal? (build-stmt '(y6 <- (mem ebp -8))) (stmt-memget 'y6 'ebp -8))
(check-equal? (build-stmt '(eax <- (mem v12 20))) (stmt-memget 'eax 'v12 20))
(check-equal? (build-stmt '(cre <- (mem cre 0))) (stmt-memget 'cre 'cre 0))

;; parse memory set operations
(check-equal? (build-stmt '((mem ebp -12) <- 31)) (stmt-memset 'ebp -12 31))
(check-equal? (build-stmt '((mem t8 4) <- v1)) (stmt-memset 't8 4 'v1))
(check-equal? (build-stmt '((mem ds0 0) <- ds0)) (stmt-memset 'ds0 0 'ds0))

;; parse arithmetic operations
(check-equal? (build-stmt '(eax += ebx)) (stmt-aop 'eax '+= 'ebx))
(check-equal? (build-stmt '(esi -= eax)) (stmt-aop 'esi '-= 'eax))
(check-equal? (build-stmt '(edi *= edx)) (stmt-aop 'edi '*= 'edx))
(check-equal? (build-stmt '(eax &= eax)) (stmt-aop 'eax '&= 'eax))
(check-equal? (build-stmt '(t14 += -5)) (stmt-aop 't14 '+= -5))
(check-equal? (build-stmt '(v01 *= 12)) (stmt-aop 'v01 '*= 12))
(check-equal? (build-stmt '(e_1 -= 7)) (stmt-aop 'e_1 '-= 7))
(check-equal? (build-stmt '(v_2 &= 11)) (stmt-aop 'v_2 '&= 11))

;; parse shift operations
(check-equal? (build-stmt '(eax <<= ebx)) (stmt-sop 'eax '<<= 'ebx))
(check-equal? (build-stmt '(esi <<= eax)) (stmt-sop 'esi '<<= 'eax))
(check-equal? (build-stmt '(edi >>= edx)) (stmt-sop 'edi '>>= 'edx))
(check-equal? (build-stmt '(eax >>= eax)) (stmt-sop 'eax '>>= 'eax))
(check-equal? (build-stmt '(t14 <<= 2)) (stmt-sop 't14 '<<= 2))
(check-equal? (build-stmt '(v01 <<= 3)) (stmt-sop 'v01 '<<= 3))
(check-equal? (build-stmt '(e_1 >>= 5)) (stmt-sop 'e_1 '>>= 5))
(check-equal? (build-stmt '(v_2 >>= 0)) (stmt-sop 'v_2 '>>= 0))

;; parse stored comparison statements
(check-equal? (build-stmt '(t07 <- ecx < ebx)) (stmt-cmp 't07 'ecx '< 'ebx))
(check-equal? (build-stmt '(var <- r12 <= tt5)) (stmt-cmp 'var 'r12 '<= 'tt5))
(check-equal? (build-stmt '(e12 <- eax = edi)) (stmt-cmp 'e12 'eax '= 'edi))
(check-equal? (build-stmt '(yy6 <- 6 < ebx)) (stmt-cmp 'yy6 6 '< 'ebx))
(check-equal? (build-stmt '(zz1 <- t87 <= 4)) (stmt-cmp 'zz1 't87 '<= 4))
(check-equal? (build-stmt '(ebx <- 2 = w_1)) (stmt-cmp 'ebx 2 '= 'w_1))
(check-equal? (build-stmt '(edi <- 9 < -8)) (stmt-cmp 'edi 9 '< -8))
(check-equal? (build-stmt '(eax <- -2 <= 1)) (stmt-cmp 'eax -2 '<= 1))
(check-equal? (build-stmt '(k09 <- 4 = 7)) (stmt-cmp 'k09 4 '= 7))
(check-equal? (build-stmt '(e45 <- 5 = 5)) (stmt-cmp 'e45 5 '= 5))

;; parse label statements
(check-equal? (build-stmt ':some_fn1) (stmt-label ':some_fn1))
(check-equal? (build-stmt ':__3fn_) (stmt-label ':__3fn_))
(check-equal? (build-stmt ':eax) (stmt-label ':eax))

;; parse goto statements
(check-equal? (build-stmt '(goto :some_fn1)) (stmt-goto ':some_fn1))
(check-equal? (build-stmt '(goto :__3fn_)) (stmt-goto ':__3fn_))
(check-equal? (build-stmt '(goto :eax)) (stmt-goto ':eax))

;; parse conditional jumps
(check-equal? (build-stmt '(cjump ecx < ebx :start1 :end1)) (stmt-cjump 'ecx '< 'ebx ':start1 ':end1))
(check-equal? (build-stmt '(cjump r12 <= tt5 :a1 :b1)) (stmt-cjump 'r12 '<= 'tt5 ':a1 ':b1))
(check-equal? (build-stmt '(cjump eax = edi :cc01 :cc02)) (stmt-cjump 'eax '= 'edi ':cc01 ':cc02))
(check-equal? (build-stmt '(cjump 6 < ebx :zy :zz)) (stmt-cjump 6 '< 'ebx ':zy ':zz))
(check-equal? (build-stmt '(cjump t87 <= 4 :_tmp1 :_tmp2)) (stmt-cjump 't87 '<= 4 ':_tmp1 ':_tmp2))
(check-equal? (build-stmt '(cjump 2 = w_1 :t6 :t7)) (stmt-cjump 2 '= 'w_1 ':t6 ':t7))
(check-equal? (build-stmt '(cjump 9 < -8 :tr1 :tr2)) (stmt-cjump 9 '< -8 ':tr1 ':tr2))
(check-equal? (build-stmt '(cjump -2 <= 1 :y0 :y1)) (stmt-cjump -2 '<= 1 ':y0 ':y1))
(check-equal? (build-stmt '(cjump 4 = 7 :s3 :s4)) (stmt-cjump 4 '= 7 ':s3 ':s4))
(check-equal? (build-stmt '(cjump 5 = 5 :ap12 :ap13)) (stmt-cjump 5 '= 5 ':ap12 ':ap13))

;; parse function calls
(check-equal? (build-stmt '(call :some_fn5)) (stmt-call ':some_fn5))
(check-equal? (build-stmt '(call eax)) (stmt-call 'eax))
(check-equal? (build-stmt '(call t12)) (stmt-call 't12))

;; parse tail calls
(check-equal? (build-stmt '(tail-call :some_fn5)) (stmt-tcall ':some_fn5))
(check-equal? (build-stmt '(tail-call eax)) (stmt-tcall 'eax))
(check-equal? (build-stmt '(tail-call t12)) (stmt-tcall 't12))

;; parse return statements
(check-equal? (build-stmt '(return)) (stmt-return))

;; parse print statements
(check-equal? (build-stmt '(eax <- (print 11))) (stmt-print 'eax 11))
(check-equal? (build-stmt '(eax <- (print esi))) (stmt-print 'eax 'esi))
(check-equal? (build-stmt '(eax <- (print t_9))) (stmt-print 'eax 't_9))

;; parse allocate statements
(check-equal? (build-stmt '(eax <- (allocate eax t43))) (stmt-alloc 'eax 'eax 't43))
(check-equal? (build-stmt '(eax <- (allocate esi 8))) (stmt-alloc 'eax 'esi 8))
(check-equal? (build-stmt '(eax <- (allocate t89 12))) (stmt-alloc 'eax 't89 12))
(check-equal? (build-stmt '(eax <- (allocate 10 edi))) (stmt-alloc 'eax 10 'edi))
(check-equal? (build-stmt '(eax <- (allocate 7 r4))) (stmt-alloc 'eax 7 'r4))
(check-equal? (build-stmt '(eax <- (allocate 8 8))) (stmt-alloc 'eax 8 8))

;; parse array error statements
(check-equal? (build-stmt '(eax <- (array-error eax t43))) (stmt-arrayerr 'eax 'eax 't43))
(check-equal? (build-stmt '(eax <- (array-error esi 8))) (stmt-arrayerr 'eax 'esi 8))
(check-equal? (build-stmt '(eax <- (array-error t89 12))) (stmt-arrayerr 'eax 't89 12))
(check-equal? (build-stmt '(eax <- (array-error 10 edi))) (stmt-arrayerr 'eax 10 'edi))
(check-equal? (build-stmt '(eax <- (array-error 7 r4))) (stmt-arrayerr 'eax 7 'r4))
(check-equal? (build-stmt '(eax <- (array-error 8 8))) (stmt-arrayerr 'eax 8 8))

;; parse bad statement
(check-exn exn:fail? (λ () (build-stmt '(eax <-))))
(check-exn exn:fail? (λ () (build-stmt '((mem ebp -9) <- t12))))
(check-exn exn:fail? (λ () (build-stmt '((mem ebp -12) <- (mem ebp -8)))))
(check-exn exn:fail? (λ () (build-stmt '(eax <- (print 5 5)))))

;; assignment gen/kill/var sets
(check-equal? (gen (build-stmt '(a <- 5))) (set))
(check-equal? (gen (build-stmt '(b <- :v1))) (set))
(check-equal? (gen (build-stmt '(c <- v1))) (set 'v1))
(check-equal? (kill (build-stmt '(a <- 5))) (set 'a))
(check-equal? (kill (build-stmt '(b <- :v1))) (set 'b))
(check-equal? (kill (build-stmt '(c <- v1))) (set 'c))
(check-equal? (var (build-stmt '(a <- 5))) (set 'a))
(check-equal? (var (build-stmt '(b <- :v1))) (set 'b))
(check-equal? (var (build-stmt '(c <- v1))) (set 'c 'v1))

;; memory reference gen/kill sets
(check-equal? (gen (build-stmt '(a <- (mem ebp -4)))) (set 'ebp))
(check-equal? (gen (build-stmt '((mem ebp -8) <- b))) (set 'ebp 'b))
(check-equal? (kill (build-stmt '(a <- (mem ebp -4)))) (set 'a))
(check-equal? (kill (build-stmt '((mem ebp -8) <- b))) (set))
(check-equal? (var (build-stmt '(a <- (mem ebp -4)))) (set 'a 'ebp))
(check-equal? (var (build-stmt '((mem ebp -8) <- b))) (set 'ebp 'b))

;; arithmetic ops gen/kill sets
(check-equal? (gen (build-stmt '(st += 5))) (set 'st))
(check-equal? (gen (build-stmt '(yz += aaa))) (set 'aaa 'yz))
(check-equal? (gen (build-stmt '(pc += pc))) (set 'pc))
(check-equal? (gen (build-stmt '(st -= 10))) (set 'st))
(check-equal? (gen (build-stmt '(yz -= zzz))) (set 'yz 'zzz))
(check-equal? (gen (build-stmt '(gz -= gz))) (set 'gz))
(check-equal? (gen (build-stmt '(cc *= 2))) (set 'cc))
(check-equal? (gen (build-stmt '(dd *= y6))) (set 'dd 'y6))
(check-equal? (gen (build-stmt '(l1 *= l1))) (set 'l1))
(check-equal? (gen (build-stmt '(ee &= 1))) (set 'ee))
(check-equal? (gen (build-stmt '(ff &= t3))) (set 'ff 't3))
(check-equal? (gen (build-stmt '(gg &= gg))) (set 'gg))
(check-equal? (kill (build-stmt '(st += 5))) (set 'st))
(check-equal? (kill (build-stmt '(yz += aaa))) (set 'yz))
(check-equal? (kill (build-stmt '(pc += pc))) (set 'pc))
(check-equal? (kill (build-stmt '(st -= 10))) (set 'st))
(check-equal? (kill (build-stmt '(yz -= zzz))) (set 'yz))
(check-equal? (kill (build-stmt '(gz -= gz))) (set 'gz))
(check-equal? (kill (build-stmt '(cc *= 2))) (set 'cc))
(check-equal? (kill (build-stmt '(dd *= y6))) (set 'dd))
(check-equal? (kill (build-stmt '(l1 *= l1))) (set 'l1))
(check-equal? (kill (build-stmt '(ee &= 1))) (set 'ee))
(check-equal? (kill (build-stmt '(ff &= t3))) (set 'ff))
(check-equal? (kill (build-stmt '(gg &= gg))) (set 'gg))
(check-equal? (var (build-stmt '(st += 5))) (set 'st))
(check-equal? (var (build-stmt '(yz += aaa))) (set 'yz 'aaa))
(check-equal? (var (build-stmt '(pc += pc))) (set 'pc))
(check-equal? (var (build-stmt '(st -= 10))) (set 'st))
(check-equal? (var (build-stmt '(yz -= zzz))) (set 'yz 'zzz))
(check-equal? (var (build-stmt '(gz -= gz))) (set 'gz))
(check-equal? (var (build-stmt '(cc *= 2))) (set 'cc))
(check-equal? (var (build-stmt '(dd *= y6))) (set 'dd 'y6))
(check-equal? (var (build-stmt '(l1 *= l1))) (set 'l1))
(check-equal? (var (build-stmt '(ee &= 1))) (set 'ee))
(check-equal? (var (build-stmt '(ff &= t3))) (set 'ff 't3))
(check-equal? (var (build-stmt '(gg &= gg))) (set 'gg))

;; shift ops gen/kill sets
(check-equal? (gen (build-stmt '(w <<= 1))) (set 'w))
(check-equal? (gen (build-stmt '(y <<= z))) (set 'y 'z))
(check-equal? (gen (build-stmt '(a >>= 3))) (set 'a))
(check-equal? (gen (build-stmt '(c >>= b))) (set 'b 'c))
(check-equal? (kill (build-stmt '(w <<= 1))) (set 'w))
(check-equal? (kill (build-stmt '(y <<= z))) (set 'y))
(check-equal? (kill (build-stmt '(a >>= 3))) (set 'a))
(check-equal? (kill (build-stmt '(c >>= b))) (set 'c))
(check-equal? (var (build-stmt '(w <<= 1))) (set 'w))
(check-equal? (var (build-stmt '(y <<= z))) (set 'y 'z))
(check-equal? (var (build-stmt '(a >>= 3))) (set 'a))
(check-equal? (var (build-stmt '(c >>= b))) (set 'c 'b))

;; stored comparsion gen/kill sets
(check-equal? (gen (build-stmt '(ecx <- 5 < 3))) (set))
(check-equal? (gen (build-stmt '(ecx <- x <= 3))) (set 'x))
(check-equal? (gen (build-stmt '(ecx <- 5 = y))) (set 'y))
(check-equal? (gen (build-stmt '(ecx <- v < u))) (set 'u 'v))
(check-equal? (kill (build-stmt '(ecx <- 5 < 3))) (set 'ecx))
(check-equal? (kill (build-stmt '(ecx <- x <= 3))) (set 'ecx))
(check-equal? (kill (build-stmt '(ecx <- 5 = y))) (set 'ecx))
(check-equal? (kill (build-stmt '(ecx <- u < v))) (set 'ecx))
(check-equal? (var (build-stmt '(ecx <- 5 < 3))) (set 'ecx))
(check-equal? (var (build-stmt '(ecx <- x <= 3))) (set 'ecx 'x))
(check-equal? (var (build-stmt '(ecx <- 5 = y))) (set 'ecx 'y))
(check-equal? (var (build-stmt '(ecx <- u < v))) (set 'ecx 'u 'v))

;; labels gen/kill sets
(check-equal? (gen (build-stmt ':some_fn)) (set))
(check-equal? (gen (build-stmt ':_2)) (set))
(check-equal? (kill (build-stmt ':some_fn)) (set))
(check-equal? (kill (build-stmt ':_2)) (set))
(check-equal? (var (build-stmt ':some_fn)) (set))
(check-equal? (var (build-stmt ':_2)) (set))

;; gotos gen/kill sets
(check-equal? (gen (build-stmt '(goto :lb_1))) (set))
(check-equal? (kill (build-stmt '(goto :lb_1))) (set))
(check-equal? (var (build-stmt '(goto :lb_1))) (set))

;; conditional jumps gen/kill sets
(check-equal? (gen (build-stmt '(cjump 4 <= 5 :fn_a :fn_b))) (set))
(check-equal? (gen (build-stmt '(cjump x < 8 :fn_a :fn_b))) (set 'x))
(check-equal? (gen (build-stmt '(cjump 9 = y :fn_a :fn_b))) (set 'y))
(check-equal? (gen (build-stmt '(cjump i <= a :fn_a :fn_b))) (set 'i 'a))
(check-equal? (kill (build-stmt '(cjump 4 <= 5 :fn_a :fn_b))) (set))
(check-equal? (kill (build-stmt '(cjump x < 8 :fn_a :fn_b))) (set))
(check-equal? (kill (build-stmt '(cjump 9 = y :fn_a :fn_b))) (set))
(check-equal? (kill (build-stmt '(cjump i <= a :fn_a :fn_b))) (set))
(check-equal? (var (build-stmt '(cjump 4 <= 5 :fn_a :fn_b))) (set))
(check-equal? (var (build-stmt '(cjump x < 8 :fn_a :fn_b))) (set 'x))
(check-equal? (var (build-stmt '(cjump 9 = y :fn_a :fn_b))) (set 'y))
(check-equal? (var (build-stmt '(cjump i <= a :fn_a :fn_b))) (set 'i 'a))

;; function calls gen/kill sets
(check-equal? (gen (build-stmt '(call t1))) (set 't1 'eax 'ecx 'edx))
(check-equal? (gen (build-stmt '(call :l1))) (set 'eax 'ecx 'edx))
(check-equal? (kill (build-stmt '(call t1))) (set 'eax 'ebx 'ecx 'edx))
(check-equal? (kill (build-stmt '(call :l1))) (set 'eax 'ebx 'ecx 'edx))
(check-equal? (var (build-stmt '(call t1))) (set 't1 'eax 'ebx 'ecx 'edx))
(check-equal? (var (build-stmt '(call :l1))) (set 'eax 'ebx 'ecx 'edx))


;; tail calls gen/kill sets
(check-equal? (gen (build-stmt '(tail-call t1))) (set 'eax 'ecx 'edi 'edx 'esi 't1))
(check-equal? (gen (build-stmt '(tail-call :l1))) (set 'eax 'ecx 'edi 'edx 'esi))
(check-equal? (kill (build-stmt '(tail-call t1))) (set))
(check-equal? (kill (build-stmt '(tail-call :l1))) (set))
(check-equal? (var (build-stmt '(tail-call t1))) (set 'eax 'ecx 'edi 'edx 'esi 't1))
(check-equal? (var (build-stmt '(tail-call :l1))) (set 'eax 'ecx 'edi 'edx 'esi))

;; return statement gen/kill sets
(check-equal? (gen (build-stmt '(return))) (set 'eax 'edi 'esi))
(check-equal? (kill (build-stmt '(return))) (set 'eax 'ecx 'edx))
(check-equal? (var (build-stmt '(return))) (set 'eax 'ecx 'edx 'edi 'esi))

;; c runtime calls gen/kill sets
(check-equal? (gen (build-stmt '(eax <- (print 5)))) (set))
(check-equal? (gen (build-stmt '(eax <- (print g54)))) (set 'g54))
(check-equal? (gen (build-stmt '(eax <- (allocate 5 10)))) (set))
(check-equal? (gen (build-stmt '(eax <- (allocate x 8)))) (set 'x))
(check-equal? (gen (build-stmt '(eax <- (allocate 4 y)))) (set 'y))
(check-equal? (gen (build-stmt '(eax <- (allocate r5 r6)))) (set 'r5 'r6))
(check-equal? (gen (build-stmt '(eax <- (array-error 12 11)))) (set))
(check-equal? (gen (build-stmt '(eax <- (array-error b 7)))) (set 'b))
(check-equal? (gen (build-stmt '(eax <- (array-error 1 c)))) (set 'c))
(check-equal? (gen (build-stmt '(eax <- (array-error q p)))) (set 'p 'q))
(check-equal? (kill (build-stmt '(eax <- (print 5)))) (set 'eax 'ecx 'edx))
(check-equal? (kill (build-stmt '(eax <- (print g54)))) (set 'eax 'ecx 'edx))
(check-equal? (kill (build-stmt '(eax <- (allocate 5 10)))) (set 'eax 'ecx 'edx))
(check-equal? (kill (build-stmt '(eax <- (allocate x 8)))) (set 'eax 'ecx 'edx))
(check-equal? (kill (build-stmt '(eax <- (allocate 4 y)))) (set 'eax 'ecx 'edx))
(check-equal? (kill (build-stmt '(eax <- (allocate r5 r6)))) (set 'eax 'ecx 'edx))
(check-equal? (kill (build-stmt '(eax <- (array-error 12 11)))) (set 'eax 'ecx 'edx))
(check-equal? (kill (build-stmt '(eax <- (array-error b 7)))) (set 'eax 'ecx 'edx))
(check-equal? (kill (build-stmt '(eax <- (array-error 1 c)))) (set 'eax 'ecx 'edx))
(check-equal? (kill (build-stmt '(eax <- (array-error q p)))) (set 'eax 'ecx 'edx))
(check-equal? (var (build-stmt '(eax <- (print 5)))) (set 'eax 'ecx 'edx))
(check-equal? (var (build-stmt '(eax <- (print g54)))) (set 'eax 'g54 'ecx 'edx))
(check-equal? (var (build-stmt '(eax <- (allocate 5 10)))) (set 'eax 'ecx 'edx))
(check-equal? (var (build-stmt '(eax <- (allocate x 8)))) (set 'eax 'x 'ecx 'edx))
(check-equal? (var (build-stmt '(eax <- (allocate 4 y)))) (set 'eax 'y 'ecx 'edx))
(check-equal? (var (build-stmt '(eax <- (allocate r5 r6)))) (set 'eax 'r5 'r6 'ecx 'edx))
(check-equal? (var (build-stmt '(eax <- (array-error 12 11)))) (set 'eax 'ecx 'edx))
(check-equal? (var (build-stmt '(eax <- (array-error b 7)))) (set 'eax 'b 'ecx 'edx))
(check-equal? (var (build-stmt '(eax <- (array-error 1 c)))) (set 'eax 'c 'ecx 'edx))
(check-equal? (var (build-stmt '(eax <- (array-error q p)))) (set 'eax 'q 'p 'ecx 'edx))

;; example gen and kill from slides
;; modified for correct return behavior
(check-equal? (gen (build-stmt ':f)) (set))
(check-equal? (gen (build-stmt '(x2 <- eax))) (set 'eax))
(check-equal? (gen (build-stmt '(x2 *= x2))) (set 'x2))
(check-equal? (gen (build-stmt '(2x2 <- x2))) (set 'x2))
(check-equal? (gen (build-stmt '(2x2 *= 2))) (set '2x2))
(check-equal? (gen (build-stmt '(3x <- eax))) (set 'eax))
(check-equal? (gen (build-stmt '(3x *= 3))) (set '3x))
(check-equal? (gen (build-stmt '(eax <- 2x2))) (set '2x2))
(check-equal? (gen (build-stmt '(eax += 3x))) (set '3x 'eax))
(check-equal? (gen (build-stmt '(eax += 4))) (set 'eax))
(check-equal? (gen (build-stmt '(return))) (set 'eax 'edi 'esi))
(check-equal? (kill (build-stmt ':f)) (set))
(check-equal? (kill (build-stmt '(x2 <- eax))) (set 'x2))
(check-equal? (kill (build-stmt '(x2 *= x2))) (set 'x2))
(check-equal? (kill (build-stmt '(2x2 <- x2))) (set '2x2))
(check-equal? (kill (build-stmt '(2x2 *= 2))) (set '2x2))
(check-equal? (kill (build-stmt '(3x <- eax))) (set '3x))
(check-equal? (kill (build-stmt '(3x *= 3))) (set '3x))
(check-equal? (kill (build-stmt '(eax <- 2x2))) (set 'eax))
(check-equal? (kill (build-stmt '(eax += 3x))) (set 'eax))
(check-equal? (kill (build-stmt '(eax += 4))) (set 'eax))
(check-equal? (kill (build-stmt '(return))) (set 'eax 'ecx 'edx))

(check-equal? (build-l2fn-base '())
              (l2fn-base
               `#()
               '#hash()))
(check-equal? (build-l2fn-base '((eax <- 5)))
              (l2fn-base
               `#(,(stmt-assign 'eax 5))
               '#hash()))
(check-equal? (build-l2fn-base '(:some_lbl))
              (l2fn-base
               `#(,(stmt-label ':some_lbl))
               '#hash((:some_lbl . 0))))
(check-equal? (build-l2fn-base '(:tr :gr :cr (eax <- 5) (edx <- 1) :hg (edx += eax)))
              (l2fn-base
               `#(,(stmt-label ':tr)
                  ,(stmt-label ':gr)
                  ,(stmt-label ':cr)
                  ,(stmt-assign 'eax 5)
                  ,(stmt-assign 'edx 1)
                  ,(stmt-label ':hg)
                  ,(stmt-aop 'edx '+= 'eax))
               '#hash((:hg . 5) (:cr . 2) (:gr . 1) (:tr . 0))))

(check-equal? (build-l2fn-succ
               (build-l2fn-base '()))
              (l2fn-succ
               `#()
               `#()))
(check-equal? (build-l2fn-succ
               (build-l2fn-base '((eax <- 5))))
              (l2fn-succ
               `#(,(stmt-assign 'eax 5))
               `#(,(set))))
(check-equal? (build-l2fn-succ
               (build-l2fn-base '(:some_lbl)))
              (l2fn-succ
               `#(,(stmt-label ':some_lbl))
               `#(,(set))))
(check-equal? (build-l2fn-succ
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

(check-equal? (reg-from-edge (set 'eax 'v1)) 'eax)
(check-equal? (reg-from-edge (set 'ebx 'v1)) 'ebx)
(check-equal? (reg-from-edge (set 'ecx 'v1)) 'ecx)
(check-equal? (reg-from-edge (set 'edx 'v1)) 'edx)
(check-equal? (reg-from-edge (set 'edi 'v1)) 'edi)
(check-equal? (reg-from-edge (set 'esi 'v1)) 'esi)
(check-equal? (reg-from-edge (set 'a5 'eax)) 'eax)
(check-equal? (reg-from-edge (set 'a5 'ebx)) 'ebx)
(check-equal? (reg-from-edge (set 'a5 'ecx)) 'ecx)
(check-equal? (reg-from-edge (set 'a5 'edx)) 'edx)
(check-equal? (reg-from-edge (set 'a5 'edi)) 'edi)
(check-equal? (reg-from-edge (set 'a5 'esi)) 'esi)
(check-equal? (reg-from-edge (set 'eax 'g6)) 'eax)
(check-equal? (reg-from-edge (set 'ebx 'f7)) 'ebx)
(check-equal? (reg-from-edge (set 'mter 'ecx)) 'ecx)
(check-equal? (reg-from-edge (set 'y93 'edx)) 'edx)
(check-equal? (reg-from-edge (set 'et1 'edi)) 'edi)
(check-equal? (reg-from-edge (set 'edi 'g91i)) 'esi)

(printf "tests completed~n")
