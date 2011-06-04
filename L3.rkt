#lang plai

;;; EECS 322 L3->L2 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "preds.rkt"))
(require (file "utils.rkt"))
(require (file "types.rkt"))
(require (file "input.rkt"))
(require (file "output.rkt"))
(require (file "renaming.rkt"))

;; TODO: move counter closures out of global for testing

;;;
;;; L3 -> L2 COMPILATION
;;;

(define arg-regs '(ecx edx eax))
(define gen-new-var (make-counter 'tvar))
(define gen-new-thenlbl (make-counter ':__tthen))
(define gen-new-elselbl (make-counter ':__telse))
(define gen-new-passlbl (make-counter ':__tpass))
(define gen-new-faillbl (make-counter ':__tfail))
(define endlbl ':__endofmain)

;; compile an L3prog into an L2prog
(define-with-contract (compile-L3prog prog)
  (L3prog? . -> . L2prog?)
  (let ([renamed-prog (rename-L3prog prog)])
    (type-case L3prog renamed-prog
      [l3prog (main others)
              (l2prog (compile-L3fn main)
                      (map compile-L3fn others))])))

;; compile an L3fn into an L2fn
(define-with-contract (compile-L3fn fn)
  (L3fn? . -> . L2fn?)
  (type-case L3fn fn
    [l3mainfn (body)
              (l2fn (append (compile-L3expr body #t #t)
                            `(,(l2s-label endlbl))))]
    [l3fn (lbl args body)
          (l2fn (append `(,(l2s-label lbl))
                        (for/list ([arg args] [reg arg-regs])
                          (l2s-assign arg reg))
                        (compile-L3expr body #f #t)))]))

;; compile an L3expr into a list of L2stmts
;; main? is #t when currently processing main
;; end? is #t when main must jump to end to return
(define-with-contract (compile-L3expr e main? end?)
  (L3expr? boolean? boolean? . -> . (listof L2stmt?))
  (type-case L3expr e
    [l3e-let (id binding body)
             (append (compile-L3term binding id)
                     (compile-L3expr body main? end?))]
    [l3e-if (test then else)
            (let ([thenlbl (gen-new-thenlbl)]
                  [elselbl (gen-new-elselbl)])
              (append `(,(l2s-cjump (encode test) '= (encode 0) elselbl thenlbl)
                        ,(l2s-label thenlbl))
                      (compile-L3expr then main? #f)
                      `(,(l2s-label elselbl))
                      (compile-L3expr else main? end?)))]
    [l3e-t (t)
           (if main?
               (if end?
                   (compile-L3term t 'eax)
                   (append (compile-L3term t 'eax)
                           `(,(l2s-goto endlbl))))
               (type-case L3term t
                 [l3t-apply (fn args)
                            (append
                             (for/list ([arg args] [reg arg-regs])
                               (l2s-assign reg (encode arg)))
                             `(,(l2s-tcall fn)))]
                 [else (append (compile-L3term t 'eax)
                               `(,(l2s-return)))]))]))

;; compile an L3term into a list of L2stmts
(define-with-contract (compile-L3term t dst)
  (L3term? L3-x? . -> . (listof L2stmt?))
  (type-case L3term t
    [l3t-biop (op v1 v2)
              (let ([tmp (gen-new-var)])
                (if (and (num? v1) (num? v2))
                    (case op
                      [(+) `(,(l2s-assign dst (encode (+ v1 v2))))]
                      [(-) `(,(l2s-assign dst (encode (- v1 v2))))]
                      [(*) `(,(l2s-assign dst (encode (* v1 v2))))]
                      [(<) `(,(l2s-assign dst (encode (if (< v1 v2) 1 0))))]
                      [(<=) `(,(l2s-assign dst (encode (if (<= v1 v2) 1 0))))]
                      [(=) `(,(l2s-assign dst (encode (if (= v1 v2) 1 0))))])
                    (case op
                      [(+) `(,(l2s-assign dst (encode v1))
                             ,(l2s-aop dst '+= (encode v2))
                             ,(l2s-aop dst '-= 1))]
                      [(-) `(,(l2s-assign dst (encode v1))
                             ,(l2s-aop dst '+= (encode v2))
                             ,(l2s-aop dst '+= 1))]
                      [(*) `(,(l2s-assign tmp (encode v1))
                             ,(l2s-sop tmp '>>= 1)
                             ,(l2s-assign dst (encode v2))
                             ,(l2s-sop dst '>>= 1)
                             ,(l2s-aop dst '*= tmp)
                             ,(l2s-sop dst '<<= 1)
                             ,(l2s-aop dst '+= 1))]
                      [(<) `(,(l2s-cmp dst (encode v1) '< (encode v2))
                             ,(l2s-sop dst '<<= 1)
                             ,(l2s-aop dst '+= 1))]
                      [(<=) `(,(l2s-cmp dst (encode v1) '<= (encode v2))
                              ,(l2s-sop dst '<<= 1)
                              ,(l2s-aop dst '+= 1))]
                      [(=) `(,(l2s-cmp dst (encode v1) '= (encode v2))
                             ,(l2s-sop dst '<<= 1)
                             ,(l2s-aop dst '+= 1))])))]
    [l3t-pred (pred v)
              (if (num? v)
                  (case pred
                    [(number?) `(,(l2s-assign dst 1))]
                    [(a?) `(,(l2s-assign dst 0))])
                  (case pred
                    [(number?) `(,(l2s-assign dst v)
                                 ,(l2s-aop dst '&= 1)
                                 ,(l2s-sop dst '<<= 1)
                                 ,(l2s-aop dst '+= 1))]
                    [(a?) `(,(l2s-assign dst v)
                            ,(l2s-aop dst '+= 1)
                            ,(l2s-aop dst '&= 1)
                            ,(l2s-sop dst '<<= 1)
                            ,(l2s-aop dst '+= 1))]))]
    [l3t-apply (fn args)
               (append
                (for/list ([arg args] [reg arg-regs])
                  (l2s-assign reg (encode arg)))
                `(,(l2s-call fn))
                (if (equal? dst 'eax)
                    '()
                    `(,(l2s-assign dst 'eax))))]
    [l3t-newarray (len init)
                  (append
                   `(,(l2s-alloc 'eax (encode len) (encode init)))
                   (if (equal? dst 'eax)
                       '()
                       `(,(l2s-assign dst 'eax))))]
    [l3t-newtuple (args)
                  (append
                   `(,(l2s-alloc 'eax (encode (length args)) (encode 0)))
                   (for/list ([arg args] [i (in-range 0 (length args))])
                     (l2s-memset 'eax (* 4 (+ i 1)) (encode arg)))
                   (if (equal? dst 'eax)
                       '()
                       `(,(l2s-assign dst 'eax))))]
    [l3t-aref (arr i)
              (let ([tmp (gen-new-var)]
                    [passlbl (gen-new-passlbl)]
                    [faillbl (gen-new-faillbl)])
                `(,(l2s-assign dst (encode i))
                  ,(l2s-sop dst '>>= 1)
                  ,(l2s-memget tmp (encode arr) 0)
                  ,(l2s-cjump dst '< tmp passlbl faillbl)
                  ,(l2s-label faillbl)
                  ,(l2s-arrayerr 'eax (encode arr) (encode i))
                  ,(l2s-label passlbl)
                  ,(l2s-sop dst '<<= 2)
                  ,(l2s-aop dst '+= arr)
                  ,(l2s-memget dst dst 4)))]
    [l3t-aset (arr i v)
              (let ([tmp (gen-new-var)]
                    [passlbl (gen-new-passlbl)]
                    [faillbl (gen-new-faillbl)])
                `(,(l2s-assign dst (encode i))
                  ,(l2s-sop dst '>>= 1)
                  ,(l2s-memget tmp (encode arr) 0)
                  ,(l2s-cjump dst '< tmp passlbl faillbl)
                  ,(l2s-label faillbl)
                  ,(l2s-arrayerr 'eax (encode arr) (encode i))
                  ,(l2s-label passlbl)
                  ,(l2s-sop dst '<<= 2)
                  ,(l2s-aop dst '+= arr)
                  ,(l2s-memset dst 4 (encode v))
                  ,(l2s-assign dst (encode 0))))]
    [l3t-alen (arr)
              `(,(l2s-memget dst arr 0)
                ,(l2s-sop dst '<<= 1)
                ,(l2s-aop dst '+= 1))]
    [l3t-print (v)
               (append
                `(,(l2s-print 'eax (encode v)))
                (if (equal? dst 'eax)
                    '()
                    `(,(l2s-assign dst 'eax))))]
    [l3t-makeclj (proc vars) (compile-L3term (l3t-newtuple `(,proc ,vars)) dst)]
    [l3t-cljproc (clj) (compile-L3term (l3t-aref clj 0) dst)]
    [l3t-cljvars (clj) (compile-L3term (l3t-aref clj 1) dst)]
    [l3t-v (v) `(,(l2s-assign dst (encode v)))]))

(define-with-contract (encode s)
  (L3-v? . -> . L2-s?)
  (if (num? s)
      (+ 1 (* s 2))
      s))
