#lang plai

;;; EECS 322 L1->x86 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))
(require (file "utils.rkt"))
(require (file "types.rkt"))
(require (file "input.rkt"))
(require (file "output.rkt"))

;;;
;;; L1 -> x86 COMPILATION
;;;

(define gen-new-label (make-counter ':l1_lbl_))

(define-with-contract (compile-L1prog prog)
  (L1prog? . -> . string?)
  (let ([out (open-output-string)])
    (type-case L1prog prog
      [l1prog (main others)
              (begin
                (fprintf out ".text~n")
                (fprintf out ".globl very_first_fn~n")
                (fprintf out ".type very_first_fn, @function~n~n")
                (compile-L1fn main out #t)
                (map (λ (fn) (compile-L1fn fn out #f)) others)
                (fprintf out "~n.size very_first_fn, .-very_first_fn~n")
                (fprintf out ".section .note.GNU-stack,\"\",@progbits~n")
                (get-output-string out))])))

(define-with-contract (compile-L1fn fn out main?)
  (L1fn? output-port? boolean? . -> . void?)
  (type-case L1fn fn
    [l1fn (stmts)
          (begin
            (when main?
              (fprintf out (format "very_first_fn:~n"))
              (fprintf out (format "  pushl ~a~n" (asm-s 'ebp)))
              (fprintf out (format "  movl ~a, ~a~n" (asm-s 'esp) (asm-s 'ebp)))
              (fprintf out "  pushal~n")
              (fprintf out (format "  movl ~a, ~a~n" (asm-s 'esp) (asm-s 'ebp))))
            (map (λ (stmt) (compile-L1stmt stmt out)) stmts)
            (when main?
              (fprintf out "  popal~n")
              (fprintf out "  leave~n")
              (fprintf out "  ret~n"))
            (void))]))

(define-with-contract (compile-L1stmt stmt out)
  (L1stmt? output-port? . -> . void?)
  (fprintf out (asm-stmt stmt))
  (void))

(define-with-contract (asm-stmt stmt)
  (L1stmt? . -> . string?)
  (type-case L1stmt stmt
    [l1s-assign (lhs rhs)
                 (if (and (num? rhs) (zero? rhs))
                     (format "  xorl ~a, ~a~n" (asm-s lhs) (asm-s lhs))
                     (format "  movl ~a, ~a~n"
                             (if (label? rhs)
                                 (string-append "$" (asm-s rhs))
                                 (asm-s rhs))
                             (asm-s lhs)))]
    [l1s-memget (lhs base offset)
                 (format "  movl ~a(~a), ~a~n" offset (asm-s base) (asm-s lhs))]
    [l1s-memset (base offset rhs)
                 (format "  movl ~a, ~a(~a)~n" (asm-s rhs) offset (asm-s base))]
    [l1s-aop (lhs op rhs)
              (format (case op
                        [(+=) "  addl ~a, ~a~n"]
                        [(-=) "  subl ~a, ~a~n"]
                        [(*=) "  imul ~a, ~a~n"]
                        [(&=) "  andl ~a, ~a~n"])
                      (asm-s rhs)
                      (asm-s lhs))]
    [l1s-sop (lhs op rhs)
              (format (case op
                        [(<<=) "  sall ~a, ~a~n"]
                        [(>>=) "  sarl ~a, ~a~n"])
                      (asm-s-lsb rhs)
                      (asm-s lhs))]
    [l1s-cmp (lhs c1 op c2)
              (cond [(and (num? c1) (num? c2))
                     (format "  movl ~a, ~a~n"
                             (if ((case op [(<) <] [(<=) <=] [(=) =]) c1 c2)
                                 (asm-s 1)
                                 (asm-s 0))
                             (asm-s lhs))]
                    [(num? c1)
                     (format "  cmpl ~a, ~a~n  ~a ~a~n  andl ~a, ~a~n"
                             (asm-s c1) (asm-s c2)
                             (case op [(<) "setg"] [(<=) "setge"] [(=) "sete"]) (asm-s-lsb lhs)
                             (asm-s 1) (asm-s lhs))]
                    [else
                     (format "  cmpl ~a, ~a~n  ~a ~a~n  andl ~a, ~a~n"
                             (asm-s c2) (asm-s c1)
                             (case op [(<) "setl"] [(<=) "setle"] [(=) "sete"]) (asm-s-lsb lhs)
                             (asm-s 1) (asm-s lhs))])]
    [l1s-label (lbl)
                (format "~n~a:~n" (asm-s lbl))]
    [l1s-goto (lbl)
               (format "  jmp ~a~n" (asm-s lbl))]
    [l1s-cjump (c1 op c2 lbl1 lbl2)
                (cond [(and (num? c1) (num? c2))
                       (format "  jmp ~a~n"
                               (if ((case op [(<) <] [(<=) <=] [(=) =]) c1 c2)
                                   (asm-s lbl1)
                                   (asm-s lbl2)))]
                      [(num? c1)
                       (format "  cmpl ~a, ~a~n  ~a ~a~n  jmp ~a~n"
                               (asm-s c1) (asm-s c2)
                               (case op [(<) "jg"] [(<=) "jge"] [(=) "je"]) (asm-s-lsb lbl1)
                               (asm-s lbl2))]
                      [else
                       (format "  cmpl ~a, ~a~n  ~a ~a~n  jmp ~a~n"
                               (asm-s c2) (asm-s c1)
                               (case op [(<) "jl"] [(<=) "jle"] [(=) "je"]) (asm-s-lsb lbl1)
                               (asm-s lbl2))])]
    [l1s-call (dst)
               (let ([new-label (gen-new-label)])
                 (format "  pushl $~a~n  pushl ~a~n  movl ~a, ~a~n  jmp ~a~n  ~a:~n"
                         (asm-s new-label)
                         (asm-s 'ebp)
                         (asm-s 'esp) (asm-s 'ebp)
                         (if (label? dst)
                             (asm-s dst)
                             (string-append "*" (asm-s dst)))
                         (asm-s new-label)))]
    [l1s-tcall (dst)
                (format "  movl ~a, ~a~n  jmp ~a~n"
                        (asm-s 'ebp) (asm-s 'esp)
                        (if (label? dst)
                            (asm-s dst)
                            (string-append "*" (asm-s dst))))]
    [l1s-return ()
                 (format "  movl ~a, ~a~n  popl ~a~n  ret~n"
                         (asm-s 'ebp) (asm-s 'esp)
                         (asm-s 'ebp))]
    [l1s-print (lhs arg1)
                (format "  pushl ~a~n  call l1_print~n  addl ~a, ~a~n"
                        (asm-s arg1)
                        (asm-s 4) (asm-s 'esp))]
    [l1s-alloc (lhs arg1 arg2)
                (format "  pushl ~a~n  pushl ~a~n  call l1_alloc~n  addl ~a, ~a~n"
                        (asm-s arg2)
                        (asm-s arg1)
                        (asm-s 8) (asm-s 'esp))]
    [l1s-arrayerr (lhs arg1 arg2)
                   (format "  pushl ~a~n  pushl ~a~n  call l1_arrayerr~n  addl ~a, ~a~n"
                           (asm-s arg2)
                           (asm-s arg1)
                           (asm-s 8) (asm-s 'esp))]))

(define-with-contract (asm-s s)
  (L2-s? . -> . string?)
  (cond
    [(num? s) (string-append "$" (number->string s))]
    [(label? s) (substring (symbol->string s) 1)]
    [else (string-append "%" (symbol->string s))]))

(define-with-contract (asm-s-lsb s)
  (L2-s? . -> . string?)
  (if (L1-cx? s)
      (case s
        [(eax) "%al"]
        [(ebx) "%bl"]
        [(ecx) "%cl"]
        [(edx) "%dl"])
      (asm-s s)))

;; main function
(define-with-contract (main fname)
  (string? . -> . void?)
  (call-with-input-file fname main/compile))

(define-with-contract (main/compile port)
  (input-port? . -> . void?)
  (display
   (compile-L1prog
    (build-L1prog
     (read port))))
  (void))
