#lang plai

;;; EECS 322 L1->x86 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "compiler-lib.rkt"))

;; L1stmt types
(define-type L1stmt
  [stmt-assign (lhs L1-x?) (rhs L1-s?)]
  [stmt-memget (lhs L1-x?) (base L1-x?) (offset n4?)]
  [stmt-memset (base L1-x?) (offset n4?) (rhs L1-s?)]
  [stmt-aop (lhs L1-x?) (op aop?) (rhs L1-s?)]
  [stmt-sop (lhs L1-x?) (op sop?) (rhs L1-s?)]
  [stmt-cmp (lhs L1-x?) (c1 L1-s?) (op cmp?) (c2 L1-s?)]
  [stmt-label (lbl label?)]
  [stmt-goto (lbl label?)]
  [stmt-cjump (c1 L1-s?) (op cmp?) (c2 L1-s?) (lbl1 label?) (lbl2 label?)]
  [stmt-call (dst L1-s?)]
  [stmt-tcall (dst L1-s?)]
  [stmt-return]
  [stmt-print (lhs L1-x?) (arg1 L1-s?)]
  [stmt-alloc (lhs L1-x?) (arg1 L1-s?) (arg2 L1-s?)]
  [stmt-arrayerr (lhs L2-x?) (arg1 L1-s?) (arg2 L1-s?)])

;; creates an L1stmt from S-expr
(define/contract (build-stmt stmt)
  (any/c . -> . L1stmt?)
  (match stmt
    [`(,(? L1-x? lhs) <- ,(? L1-s? rhs)) (stmt-assign lhs rhs)]
    [`(,(? L1-x? lhs) <- (mem ,(? L1-x? base) ,(? n4? offset))) (stmt-memget lhs base offset)]
    [`((mem ,(? L1-x? base) ,(? n4? offset)) <- ,(? L1-s? rhs)) (stmt-memset base offset rhs)]
    [`(,(? L1-x? lhs) ,(? aop? op) ,(? L1-s? rhs)) (stmt-aop lhs op rhs)]
    [`(,(? L1-x? lhs) ,(? sop? op) ,(? L1-s? rhs)) (stmt-sop lhs op rhs)]
    [`(,(? L1-x? lhs) <- ,(? L1-s? c1) ,(? cmp? op) ,(? L1-s? c2)) (stmt-cmp lhs c1 op c2)]
    [(? label? lbl) (stmt-label lbl)]
    [`(goto ,(? label? lbl)) (stmt-goto lbl)]
    [`(cjump ,(? L1-s? c1) ,(? cmp? op) ,(? L1-s? c2) ,(? label? lbl1) ,(? label? lbl2))
     (stmt-cjump c1 op c2 lbl1 lbl2)]
    [`(call ,(? L1-s? dst)) (stmt-call dst)]
    [`(tail-call ,(? L1-s? dst)) (stmt-tcall dst)]
    [`(return) (stmt-return)]
    [`(,(? L1-x? lhs) <- (print ,(? L1-s? arg1))) (stmt-print lhs arg1)]
    [`(,(? L1-x? lhs) <- (allocate ,(? L1-s? arg1) ,(? L1-s? arg2))) (stmt-alloc lhs arg1 arg2)]
    [`(,(? L1-x? lhs) <- (array-error ,(? L1-s? arg1) ,(? L1-s? arg2))) (stmt-arrayerr lhs arg1 arg2)]
    [_ (error 'build-stmt "no matching clause for ~a" stmt)]))

(define/contract (make-counter prefix)
  (symbol? . -> . (-> symbol?))
  (let ([p (symbol->string prefix)]
        [i 0])
    (λ ()
      (begin0 (string->symbol (string-append p (number->string i)))
              (set! i (+ i 1))))))

(define gen-new-label (make-counter ':__tlbl))

(define/contract (asm-s s)
  (L2-s? . -> . string?)
  (cond
    [(num? s) (string-append "$" (number->string s))]
    [(label? s) (string-append (substring (symbol->string s) 1) ":")]
    [else (string-append "%" (symbol->string s))]))

(define/contract (asm-stmt stmt)
  (L1stmt? . -> . string?)
  (type-case L1stmt stmt
    [stmt-assign (lhs rhs)
                 (format "  movl ~a, ~a~n" (asm-s rhs) (asm-s lhs))]
    [stmt-memget (lhs base offset)
                 (format "  movl ~a(~a), ~a~n" (asm-s offset) (asm-s base) (asm-s lhs))]
    [stmt-memset (base offset rhs)
                 (format "  movl ~a, ~a(~a)" (asm-s rhs) (asm-s offset) (asm-s base))]
    [stmt-aop (lhs op rhs)
              (format (case op
                        [(+=) "  addl ~a, ~a~n"]
                        [(-=) "  subl ~a, ~a~n"]
                        [(*=) "  imul ~a, ~a~n"]
                        [(&=) "  andl ~a, ~a~n"])
                      (asm-s rhs)
                      (asm-s lhs))]
    [stmt-sop (lhs op rhs)
              (format (case op
                        [(<<=) "  sall ~a, ~a~n"]
                        [(>>=) "  sall ~a, ~a~n"])
                      (asm-s rhs)
                      (asm-s lhs))]
    [stmt-cmp (lhs c1 op c2)
              (if (and (num? c1) (num? c2))
                  (format "  movl ~a, ~a~n"
                          (if (op c1 c2) (asm-s 1) (asm-s 0))
                          (asm-s lhs))
                  (format "  movl ~a, ~a~n  cmpl ~a, ~a~n  ~a ~a~n"
                          (asm-s 0) (asm-s lhs)
                          (asm-s c2) (asm-s c1)
                          (case op
                            [(<) "setl"]
                            [(<=) "setle"]
                            [(=) "sete"])
                          (asm-s lhs)))]
    [stmt-label (lbl)
                (format "~a~n" (asm-s lbl))]
    [stmt-goto (lbl)
               (format "  jmp ~a~n" (asm-s lbl))]
    [stmt-cjump (c1 op c2 lbl1 lbl2)
              (if (and (num? c1) (num? c2))
                  (format "  jmp ~a~n"
                          (if (op c1 c2) (asm-s lbl1) (asm-s lbl2)))
                  (format "  cmpl ~a, ~a~n  ~a ~a~n  jmp ~a~n"
                          (asm-s c2) (asm-s c1)
                          (case op
                            [(<) "jl"]
                            [(<=) "jle"]
                            [(=) "je"])
                          (asm-s lbl1)
                          (asm-s lbl2)))]
    [stmt-call (dst)
               (let ([new-label (gen-new-label)])
                 (format "  pushl ~a~n  pushl ~a~n  movl ~a, ~a~n  jmp ~a~n  ~a~n"
                         (asm-s new-label)
                         (asm-s 'ebp)
                         (asm-s 'esp) (asm-s 'ebp)
                         (asm-s dst)
                         (asm-s new-label)))]
    [stmt-tcall (dst)
                (format "  movl ~a, ~a~n  jmp ~a~n"
                        (asm-s 'ebp) (asm-s 'esp) (asm-s dst))]
    [stmt-return ()
                 (format "  movl ~a, ~a~n  popl ~a~n  ret~n"
                         (asm-s 'ebp) (asm-s 'esp) (asm-s 'ebp))]
    [stmt-print (lhs arg1)
                (format "  pushl ~a~n  call l1_print~n  addl ~a, ~a~n"
                        (asm-s arg1) (asm-s 4) (asm-s 'esp))]
    [stmt-alloc (lhs arg1 arg2)
                (format "  pushl ~a~n  pushl ~a~n  call l1_allocate~n  addl ~a, ~a~n"
                        (asm-s arg2) (asm-s arg1)
                        (asm-s 8) (asm-s 'esp))]
    [stmt-arrayerr (lhs arg1 arg2)
                (format "  pushl ~a~n  pushl ~a~n  call l1_arrayerror~n  addl ~a, ~a~n"
                        (asm-s arg2) (asm-s arg1)
                        (asm-s 8) (asm-s 'esp))]))

;; main function
(define/contract (main fname)
  (string? . -> . void?)
  (call-with-input-file fname main/compile))

(define/contract (main/compile port)
  (input-port? . -> . void?)
  (display (compile-l1 (read port)))
  (void))

(define/contract (compile-l1 src)
  ((non-empty-listof (non-empty-listof any/c)) . -> . string?)
  (let ([out (open-output-string)])
    (fprintf out ".file \"samp.c\"~n")
    (fprintf out ".text~n")
    (fprintf out ".globl go~n")
    (fprintf out ".type go, @function~n")
    (compile-gofn (first src) out)
    (map (λ (fn) (compile-fn fn out)) (rest src))
    (fprintf out "~n.size go, .-go~n")
    (fprintf out ".ident \"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2\"~n")
    (fprintf out ".section .note.GNU-stack,\"\",@progbits~n")
    (get-output-string out)))

(define/contract (compile-gofn fn out)
  ((non-empty-listof any/c) output-port? . -> . void?)
  (fprintf out (compile-stmt ':go))
  (fprintf out "  pushl %ebp~n")
  (fprintf out "  movl %esp, %ebp~n")
  (fprintf out "  pushal~n")
  (fprintf out "  movl %esp, %ebp~n")
  (map (λ (stmt) (fprintf out (compile-stmt stmt))) fn)
  (fprintf out "  popal~n")
  (fprintf out "  leave~n")
  (fprintf out "  ret~n")
  (void))

(define/contract (compile-fn fn out)
  ((non-empty-listof any/c) output-port? . -> . void?)
  (unless (label? (first fn))
    (error 'compile-l1 "first statement must be label, given ~a"))
  (map (λ (stmt) (fprintf out (compile-stmt stmt))) fn)
  (void))

(define/contract (compile-stmt stmt)
  (any/c . -> . string?)
  (asm-stmt (build-stmt stmt)))

#|
(provide main)
(provide compile-1)

;; main : string? -> void?
(define (main fname)
  (call-with-input-file fname
    (λ (in) (display (compile-1 (read in))))))

;; compile-1 : L1? -> string?
(define (compile-1 src)
  (let ([out (open-output-string)])
    (fprintf out ".file \"samp.c\"~n")
    (fprintf out ".text~n")
    (fprintf out ".globl go~n")
    (fprintf out ".type go, @function~n")
    (compile-1-cfn (cons ':go (first src)) out)
    (map (λ (fn) (compile-1-fn fn out)) (rest src))
    (fprintf out "~n.size go, .-go~n")
    (fprintf out ".ident \"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2\"~n")
    (fprintf out ".section .note.GNU-stack,\"\",@progbits~n")
    (get-output-bytes out #t)))

;; compile-1-cfn : L1-fn? output-port? -> void?
;      compile with the C calling convention
(define (compile-1-cfn fn out)
  (unless (label? (first fn))
    (error 'compile-1 "functions must start with a label"))
  (compile-1-stmt (first fn) out)
  (fprintf out "\tpushl %ebp~n")
  (fprintf out "\tmovl %esp, %ebp~n")
  (fprintf out "\tpushal~n")
  (fprintf out "\tmovl %esp, %ebp~n")
  (map (λ (stmt) (compile-1-stmt stmt out)) (rest fn))
  (fprintf out "\tpopal~n")
  (fprintf out "\tleave~n")
  (fprintf out "\tret~n"))

;; compile-1-fn : L1-fn? output-port? -> void?
;      compile with the custom calling convention
(define (compile-1-fn fn out)
  (unless (label? (first fn))
    (error 'compile-1 "functions must start with a label"))
  (map (λ (stmt) (compile-1-stmt stmt out)) fn))

;; compile-1-stmt : L1-stmt? output-port? -> void?
(define (compile-1-stmt stmt out)
  (or (match-1-assign stmt out)
      (match-1-arith stmt out)
      (match-1-shift stmt out)
      (match-1-cmp stmt out)
      (match-1-labels stmt out)
      (match-1-cjump stmt out)
      (match-1-calls stmt out)
      (match-1-callouts stmt out)
      (error 'compile-1 "invalid syntax: ~a" stmt)))

(define (match-1-assign stmt out)
  (match stmt
    [`(,(? x? lval) <- ,(? label? rval))
     (fprintf out "\tmovl $~a, ~a~n" (asm rval) (asm lval))]
    [`(,(? x? lval) <- ,(? s? rval))
     (fprintf out "\tmovl ~a, ~a~n" (asm rval) (asm lval))]
    [`(,(? x? lval) <- (mem ,(? x? rval) ,(? n4? os)))
     (fprintf out "\tmovl ~a(~a), ~a~n" os (asm rval) (asm lval))]
    [`((mem ,(? x? lval) ,(? n4? os)) <- ,(? label? rval))
     (fprintf out "\tmovl $~a, ~a(~a)~n" (asm rval) os (asm lval))]
    [`((mem ,(? x? lval) ,(? n4? os)) <- ,(? s? rval))
     (fprintf out "\tmovl ~a, ~a(~a)~n" (asm rval) os (asm lval))]
    [_ #f]))

(define (match-1-arith stmt out)
  (match stmt
    [`(,(? x? lval) += ,(? s? rval))
     (fprintf out "\taddl ~a, ~a~n" (asm rval) (asm lval))]
    [`(,(? x? lval) -= ,(? s? rval))
     (fprintf out "\tsubl ~a, ~a~n" (asm rval) (asm lval))]
    [`(,(? x? lval) *= ,(? s? rval))
     (fprintf out "\timull ~a, ~a~n" (asm rval) (asm lval))]
    [`(,(? x? lval) &= ,(? s? rval))
     (fprintf out "\tandl ~a, ~a~n" (asm rval) (asm lval))]
    [_ #f]))

(define (match-1-shift stmt out)
  (match stmt
    [`(,(? x? lval) <<= ,(? (or/c num? sx?) rval))
     (fprintf out "\tsall ~a, ~a~n" (asm rval) (asm lval))]
    [`(,(? x? lval) >>= ,(? (or/c num? sx?) rval))
     (fprintf out "\tsarl ~a, ~a~n" (asm rval) (asm lval))]
    [_ #f]))

(define (match-1-cmp stmt out)
  (match stmt
    [`(,(? cx? lval) <- ,(? num? v1) < ,(? num? v2))
     (if (< v1 v2)
         (fprintf out "\tmovl $1, ~a~n" (asm lval))
         (fprintf out "\tmovl $0, ~a~n" (asm lval)))]
    [`(,(? cx? lval) <- ,(? s? v1) < ,(? num? v2))
     (fprintf out "\tmovl $0, ~a~n" (asm lval))
     (fprintf out "\tcmpl ~a, ~a~n" (asm v1) (asm v2))
     (fprintf out "\tsetg ~a~n" (byte-asm lval))]
    [`(,(? cx? lval) <- ,(? s? v1) < ,(? s? v2))
     (fprintf out "\tmovl $0, ~a~n" (asm lval))
     (fprintf out "\tcmpl ~a, ~a~n" (asm v2) (asm v1))
     (fprintf out "\tsetl ~a~n" (byte-asm lval))]
    [`(,(? cx? lval) <- ,(? num? v1) <= ,(? num? v2))
     (if (<= v1 v2)
         (fprintf out "\tmovl $1, ~a~n" (asm lval))
         (fprintf out "\tmovl $0, ~a~n" (asm lval)))]
    [`(,(? cx? lval) <- ,(? s? v1) <= ,(? num? v2))
     (fprintf out "\tmovl $0, ~a~n" (asm lval))
     (fprintf out "\tcmpl ~a, ~a~n" (asm v1) (asm v2))
     (fprintf out "\tsetge ~a~n" (byte-asm lval))]
    [`(,(? cx? lval) <- ,(? s? v1) <= ,(? s? v2))
     (fprintf out "\tmovl $0, ~a~n" (asm lval))
     (fprintf out "\tcmpl ~a, ~a~n" (asm v2) (asm v1))
     (fprintf out "\tsetle ~a~n" (byte-asm lval))]
    [`(,(? cx? lval) <- ,(? num? v1) = ,(? num? v2))
     (if (= v1 v2)
         (fprintf out "\tmovl $1, ~a~n" (asm lval))
         (fprintf out "\tmovl $0, ~a~n" (asm lval)))]
    [`(,(? cx? lval) <- ,(? s? v1) = ,(? num? v2))
     (fprintf out "\tmovl $0, ~a~n" (asm lval))
     (fprintf out "\tcmpl ~a, ~a~n" (asm v1) (asm v2))
     (fprintf out "\tsete ~a~n" (byte-asm lval))]
    [`(,(? cx? lval) <- ,(? s? v1) = ,(? s? v2))
     (fprintf out "\tmovl $0, ~a~n" (asm lval))
     (fprintf out "\tcmpl ~a, ~a~n" (asm v2) (asm v1))
     (fprintf out "\tsete ~a~n" (byte-asm lval))]
    [_ #f]))

(define (match-1-labels stmt out)
  (if (label? stmt)
      (fprintf out "~a:~n" (asm stmt))
      (match stmt
        [`(goto ,(? label? lbl))
         (fprintf out "\tjmp ~a~n" (asm lbl))]
        [_ #f])))

(define (match-1-cjump stmt out)
  (match stmt
    [`(cjump ,(? num? c1) < ,(? num? c2) ,(? label? lbl1) ,(? label? lbl2))
     (fprintf out "\tjmp ~a~n" (asm (if (< c1 c2) lbl1 lbl2)))]
    [`(cjump ,(? num? c1) < ,(? x? c2) ,(? label? lbl1) ,(? label? lbl2))
     (fprintf out "\tcmpl ~a, ~a~n" (asm c1) (asm c2))
     (fprintf out "\tjg ~a~n" (asm lbl1))
     (fprintf out "\tjmp ~a~n" (asm lbl2))]
    [`(cjump ,(? s? c1) < ,(? s? c2) ,(? label? lbl1) ,(? label? lbl2))
     (fprintf out "\tcmpl ~a, ~a~n" (asm c2) (asm c1))
     (fprintf out "\tjl ~a~n" (asm lbl1))
     (fprintf out "\tjmp ~a~n" (asm lbl2))]
    [`(cjump ,(? num? c1) <= ,(? num? c2) ,(? label? lbl1) ,(? label? lbl2))
     (fprintf out "\tjmp ~a~n" (asm (if (<= c1 c2) lbl1 lbl2)))]
    [`(cjump ,(? num? c1) <= ,(? x? c2) ,(? label? lbl1) ,(? label? lbl2))
     (fprintf out "\tcmpl ~a, ~a~n" (asm c1) (asm c2))
     (fprintf out "\tjge ~a~n" (asm lbl1))
     (fprintf out "\tjmp ~a~n" (asm lbl2))]
    [`(cjump ,(? s? c1) <= ,(? s? c2) ,(? label? lbl1) ,(? label? lbl2))
     (fprintf out "\tcmpl ~a, ~a~n" (asm c2) (asm c1))
     (fprintf out "\tjle ~a~n" (asm lbl1))
     (fprintf out "\tjmp ~a~n" (asm lbl2))]
    [`(cjump ,(? num? c1) = ,(? num? c2) ,(? label? lbl1) ,(? label? lbl2))
     (fprintf out "\tjmp ~a~n" (asm (if (= c1 c2) lbl1 lbl2)))]
    [`(cjump ,(? num? c1) = ,(? x? c2) ,(? label? lbl1) ,(? label? lbl2))
     (fprintf out "\tcmpl ~a, ~a~n" (asm c1) (asm c2))
     (fprintf out "\tjne ~a~n" (asm lbl1))
     (fprintf out "\tjmp ~a~n" (asm lbl2))]
    [`(cjump ,(? s? c1) = ,(? s? c2) ,(? label? lbl1) ,(? label? lbl2))
     (fprintf out "\tcmpl ~a, ~a~n" (asm c2) (asm c1))
     (fprintf out "\tje ~a~n" (asm lbl1))
     (fprintf out "\tjmp ~a~n" (asm lbl2))]
    [_ #f]))

(define (match-1-calls stmt out)
  (match stmt
    [`(call ,(? s? dst))
     (let ([new-label (gen-new-label)])
       (fprintf out "\tpushl $~a~n" (asm new-label))
       (fprintf out "\tpushl %ebp~n")
       (fprintf out "\tmovl %esp, %ebp~n")
       (if (label? dst)
           (fprintf out "\tjmp ~a~n" (asm dst))
           (fprintf out "\tjmp *~a~n" (asm dst)))
       (fprintf out "~a:~n" (asm new-label)))]
    [`(tail-call ,(? s? dst))
     (fprintf out "\tmovl %ebp, %esp~n")
       (if (label? dst)
           (fprintf out "\tjmp ~a~n" (asm dst))
           (fprintf out "\tjmp *~a~n" (asm dst)))]
    [`(return)
     (fprintf out "\tmovl %ebp, %esp~n")
     (fprintf out "\tpopl %ebp~n")
     (fprintf out "\tret~n")]
    [_ #f]))

(define (match-1-callouts stmt out)
  (match stmt
    [`(eax <- (print ,(? s? arg1)))
     (fprintf out "\tpushl ~a~n" (asm arg1))
     (fprintf out "\tcall l1_print~n")
     (fprintf out "\taddl $4, %esp~n")]
    [`(eax <- (allocate ,(? s? arg1) ,(? s? arg2)))
     (fprintf out "\tpushl ~a~n" (asm arg2))
     (fprintf out "\tpushl ~a~n" (asm arg1))
     (fprintf out "\tcall l1_allocate~n")
     (fprintf out "\taddl $8, %esp~n")]
    [`(eax <- (array-error ,(? s? arg1) ,(? s? arg2)))
     (fprintf out "\tpushl ~a~n" (asm arg2))
     (fprintf out "\tpushl ~a~n" (asm arg1))
     (fprintf out "\tcall l1_arrayerror~n")
     (fprintf out "\taddl $8, %esp~n")]
    [_ #f]))

|#
