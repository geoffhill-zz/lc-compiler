#lang racket

;;; randomness utilities

(define/contract (decide po)
  (real? . -> . boolean?)
  (if (< (random) po) #t #f))

(define/contract (choice l)
  ((or/c vector? list?) . -> . any/c)
  (cond
    [(vector? l) (vector-ref l (random (vector-length l)))]
    [(list? l) (list-ref l (random (length l)))]))

(define/contract (build-list* n proc)
  (integer? (-> any/c) . -> . (listof any/c))
  (build-list n (λ (x) (proc))))

;;; random components

(define/contract (random-letter)
  (-> char?)
  (choice `#(#\a #\b #\c #\d #\e #\f #\g #\h #\u #\v #\w #\x #\y #\z #\_)))

(define/contract (random-digit)
  (-> char?)
  (choice `#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(define/contract (random-alphanumeric)
  (-> char?)
  (if (decide 0.5) (random-letter) (random-digit)))

(define/contract (random-letter-string)
  (-> string?)
  (list->string (build-list* (+ 1 (random 7)) random-letter)))

(define/contract (random-alphanumeric-string)
  (-> string?)
  (list->string (build-list* (+ 1 (random 7)) random-alphanumeric)))

;;; random terms

(define regs `#(eax ebx ecx edx esi edi))

(define/contract (random-reg)
  (-> symbol?)
  (choice regs))

(define/contract (random-var)
  (-> symbol?)
  (string->symbol (random-letter-string)))

(define/contract (random-x)
  (-> symbol?)
  (if (decide 0.5) (random-reg) (random-var)))

(define/contract (random-label)
  (-> symbol?)
  (string->symbol (string-append ":" (random-letter-string) (random-alphanumeric-string))))

(define/contract (random-num)
  (-> integer?)
  (- 100 (random 200)))

(define/contract (random-n4)
  (-> integer?)
  (- 0 (* 4 (random 8))))

(define/contract (random-odd)
  (-> integer?)
  (- 1 (* 2 (- 40 (random 80)))))

(define/contract (random-aop)
  (-> symbol?)
  (choice `#(+= -= *= &=)))

(define/contract (random-sop)
  (-> symbol?)
  (choice `#(<<= >>=)))

(define/contract (random-cmp)
  (-> symbol?)
  (choice `#(< <= =)))

;;; random statements

(define/contract (random-stmt lbl1 lbl2 x1 x2 x3)
  (symbol? symbol? symbol? symbol? symbol? . -> . any/c)
  (choice
   `#((,x1 <- s1)
      ((mem ebp ,(random-n4)) <- ,x1)
      (,x1 <- (mem ebp ,(random-n4)))
      (,x1 ,(random-aop) ,x2)
      (,x1 ,(random-aop) ,(random-num))
      (,x1 ,(random-sop) ,x2)
      (,x1 ,(random-sop) ,(random-num))
      (,x1 <- ,(choice `#(,x2 ,(random-num))) ,(random-cmp) ,(choice `#(,x3 ,(random-num))))
      ,lbl1
      (goto ,lbl1)
      (cjump ,(choice `#(,x1 ,(random-num))) ,(random-cmp) ,(choice `#(,x2 ,(random-num))) ,lbl1 ,lbl2)
      (call ,lbl1)
      (tail-call ,lbl1)
      (return)
      (eax <- (print ,(choice `#(,x1 ,(random-num)))))
      (eax <- (allocate ,(choice `#(,x1 ,(random-num))) ,(choice `#(,x2 ,(random-num)))))
      (eax <- (array-error ,(choice `#(,x1 ,(random-num))) ,(choice `#(,x2 ,(random-num))))))))
    

;;; random functions

(define/contract (random-fn)
  (-> list?)
  (let ([lbls (build-list* 4 random-label)]
        [xs (build-list* 6 random-x)])
    (random-fn-accum lbls '() xs '(eax ecx edx) '())))

(define/contract (random-fn-accum lbls usedlbls xs usedxs accum)
  ((listof symbol?) (listof symbol?) (listof symbol?) (listof symbol?) list? . -> . list?)
  (if (decide 0.1)
      (reverse
       (cons `(return)
             (cons `(eax <- (print ,(random-odd))) accum)))
      (let* ([lbl1 (choice lbls)]
             [lbl2 (choice lbls)]
             [x1 (choice xs)]
             [x2 (choice xs)]
             [x3 (choice xs)]
             [stmt (random-stmt lbl1 lbl2 x1 x2 x3)]
             [accum (cons stmt accum)])
        (random-fn-accum lbls usedlbls xs usedxs accum))))

(define (write-tests)
  (for ([i (in-range 500)])
    (call-with-output-file (format "tests/L2/~a.L2f" i)
      (λ (out) (pretty-write `(,(random-fn)) out)))))