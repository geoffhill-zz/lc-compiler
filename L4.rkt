#lang plai

;;; EECS 322 L4->L3 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "types.rkt"))
(require (file "preds.rkt"))
(require (file "utils.rkt"))

;;;
;;; VARIABLE AND LABEL RENAMING
;;;

;; renames all the variables in L4 programs
;; labels carry across functions
;; variables do not carry across functions
;; lets always generate new variables, ignoring existing ones

(define-with-contract (rename-L4prog prog)
  (L4prog? . -> . L4prog?)
  (define varfn (make-counter 'var_))
  (define lblfn (make-counter ':lbl_))
  (type-case L4prog prog
    [l4prog (main others)
            (let-values ([(main changes) (rename-L4fn main (namemap) varfn lblfn)])
              (let ([changes (namemap-lbls-only changes)])
                (l4prog main
                        (let loop ([others others]
                                   [renamed '()]
                                   [changes changes])
                          (if (null? others)
                              (reverse renamed)
                              (let-values ([(fn changes) (rename-L4fn (first others) changes varfn lblfn)])
                                (let ([changes (namemap-lbls-only changes)])
                                  (loop (rest others) (cons fn renamed) changes))))))))]))

(define-with-contract (rename-L4fn fn changes varfn lblfn)
  (L4fn? namemap? (-> symbol?) (-> label?) . -> . (values L4fn? namemap?))
  (type-case L4fn fn
    [l4mainfn (body)
              (let-values ([(body changes) (rename-L4expr body changes varfn lblfn)])
                (values (l4mainfn body) changes))]
    [l4fn (lbl args body)
          (let* ([newlbl (hash-ref changes lbl lblfn)]
                 [changes (hash-set changes lbl newlbl)])
            (let-values ([(newargs changes)
                          (let loop ([args args]
                                     [renamed '()]
                                     [changes changes])
                            (if (null? args)
                                (values (reverse renamed) changes)
                                (let* ([arg (first args)]
                                       [newarg (hash-ref changes arg varfn)]
                                       [changes (hash-set changes arg newarg)])
                                  (loop (rest args) (cons newarg renamed) changes))))])
              (let-values ([(body changes) (rename-L4expr body changes varfn lblfn)])
                (values (l4fn newlbl newargs body) changes))))]))

(define-with-contract (rename-L4expr expr changes varfn lblfn)
  (L4expr? namemap? (-> symbol?) (-> label?) . -> . (values L4expr? namemap?))
  (type-case L4expr expr
    [l4e-let (id binding body)
             (let* ([newid (varfn)]
                    [changes (hash-set changes id newid)])
               (let-values ([(binding changes) (rename-L4expr binding changes varfn lblfn)])
                 (let-values ([(body changes) (rename-L4expr body changes varfn lblfn)])
                   (values (l4e-let newid binding body) changes))))]
    [l4e-if (test then else)
            (let-values ([(test changes) (rename-L4expr test changes varfn lblfn)])
              (let-values ([(then changes) (rename-L4expr then changes varfn lblfn)])
                (let-values ([(else changes) (rename-L4expr else changes varfn lblfn)])
                  (values (l4e-if test then else) changes))))]
    [l4e-app (fn args)
             (let-values ([(fn changes) (rename-L4expr fn changes varfn lblfn)])
               (let loop ([args args]
                          [renamed '()]
                          [changes changes])
                 (if (null? args)
                     (values (l4e-app fn (reverse renamed)) changes)
                     (let-values ([(arg changes) (rename-L4expr (first args) changes varfn lblfn)])
                       (loop (rest args) (cons arg renamed) changes)))))]
    [l4e-begin (fst snd)
               (let-values ([(fst changes) (rename-L4expr fst changes varfn lblfn)])
                 (let-values ([(snd changes) (rename-L4expr snd changes varfn lblfn)])
                   (values (l4e-begin fst snd) changes)))]
    [l4e-v (v)
           (cond
             [(number? v) (values expr changes)]
             [(L4-builtin? v) (values expr changes)]
             [(label? v) (let* ([newlbl (hash-ref changes v lblfn)]
                                [changes (hash-set changes v newlbl)])
                           (values (l4e-v newlbl) changes))]
             [else (let* ([newv (hash-ref changes v varfn)]
                          [changes (hash-set changes v newv)])
                     (values (l4e-v newv) changes))])]))


;;;
;;; L4 -> L3 COMPILATION
;;;

(define-with-contract (compile-L4prog prog)
  (L4prog? . -> . L3prog?)
  (type-case L4prog (rename-L4prog prog)
    [l4prog (main others)
            (l3prog (compile-L4fn main)
                    (map compile-L4fn others))]))

(define-with-contract (compile-L4fn fn)
  (L4fn? . -> . L3fn?)
  (type-case L4fn fn
    [l4mainfn (body) (l3mainfn (compile-L4expr (normalize body)))]
    [l4fn (lbl args body) (l3fn lbl args (compile-L4expr (normalize body)))]))

(define-with-contract (compile-L4expr expr)
  (L4expr? . -> . L3expr?)
  (type-case L4expr expr
    [l4e-let (id binding body)
             (let ([binding (compile-L4expr binding)])
               (unless (l3e-t? binding)
                 (error 'L4 "should never find binding expressions in normalized expr"))
               (l3e-let id (l3e-t-t binding) (compile-L4expr body)))]
    [l4e-if (test then else)
            (let ([test (compile-L4expr test)])
              (unless (l3e-t? test)
                (error 'L4 "should never find test expressions in normalized expr, got ~a" test))
              (l3e-if (l3t-v-v (l3e-t-t test))
                      (compile-L4expr then)
                      (compile-L4expr else)))]
    [l4e-begin (fst snd)
               (error 'L4 "should never find begin in normalized expr")]
    [l4e-app (fn args)
             (l3e-t (build-L3term (format-L4expr expr)))]
    [l4e-v (v) (l3e-t (l3t-v v))]))

;;;
;;; A-NORMALIZATION
;;;

;; transforms an L4expr to an ANF L4expr
(define-with-contract (normalize expr)
  (L4expr? . -> . L4expr?)
  (define varfn (make-counter 'letvar_))
  (find expr (mt-ctxt) varfn))

(define-type L4ctxt
  [mt-ctxt]
  [let-ctxt (id L4-x?)
            (body L4expr?)
            (ctxt L4ctxt?)]
  [if-ctxt (then L4expr?)
           (else L4expr?)
           (ctxt L4ctxt?)]
  [fn-ctxt (done (listof l4e-v?))
           (todo (listof L4expr?))
           (ctxt L4ctxt?)])

(define-with-contract (find expr ctxt varfn)
  (L4expr? L4ctxt? (-> symbol?) . -> . L4expr?)
  (type-case L4expr expr
    [l4e-let (id binding body)
             (find binding (let-ctxt id body ctxt) varfn)]
    [l4e-if (test then else)
            (find test (if-ctxt then else ctxt) varfn)]
    [l4e-begin (fst snd)
               (find fst (let-ctxt (varfn) snd ctxt) varfn)]
    [l4e-app (fn args)
             (find fn (fn-ctxt '() args ctxt) varfn)]
    [l4e-v (v) (fill expr ctxt varfn)]))

(define-with-contract (fill expr ctxt varfn)
  (L4expr? L4ctxt? (-> symbol?) . -> . L4expr?)
  (type-case L4ctxt ctxt
    [mt-ctxt () expr]
    [let-ctxt (id body ctxt)
              (l4e-let id expr (find body ctxt varfn))]
    [if-ctxt (then else ctxt)
             (if (l4e-v? expr)
                 (l4e-if expr (find then ctxt varfn) (find else ctxt varfn))
                 (let ([id (varfn)])
                   (l4e-let id expr (l4e-if (l4e-v id) (find then ctxt varfn) (find else ctxt varfn)))))]
    [fn-ctxt (done todo ctxt)
             (if (l4e-v? expr)
                 (if (null? todo)
                     (let ([terms (reverse (cons expr done))])
                       (fill (l4e-app (first terms) (rest terms)) ctxt varfn))
                     (find (first todo) (fn-ctxt (cons expr done) (rest todo) ctxt) varfn))
                 (let* ([id (varfn)]
                        [newdone (cons (l4e-v id) done)])
                   (if (null? todo)
                       (let ([terms (reverse newdone)])
                         (l4e-let id expr (fill (l4e-app (first terms) (rest terms)) ctxt varfn)))
                       (l4e-let id expr (find (first todo)
                                              (fn-ctxt newdone (rest todo) ctxt)
                                              varfn)))))]))

;;;
;;; EXTERNAL INTERFACE
;;;

(define-with-contract (main fname)
  (string? . -> . void?)
  (call-with-input-file fname main/compile-L4))

(define-with-contract (main/compile-L4 port)
  (input-port? . -> . void?)
  (pretty-write
   (format-L3prog
    (compile-L4prog
     (build-L4prog (read port))))))
