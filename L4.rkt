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
  (L4fn? namemap? (-> symbol?) (-> symbol?) . -> . (values L4fn? namemap?))
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
  (L4expr? namemap? (-> symbol?) (-> symbol?) . -> . (values L4expr? namemap?))
  (type-case L4expr expr
    [l4e-let (id binding body)
             (let* ([newid (hash-ref changes id varfn)]
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
  (type-case L4prog prog
    [l4prog (main others)
            (l3prog (compile-L4fn main)
                    (map compile-L4fn others))]))

(define-with-contract (compile-L4fn fn)
  (L4fn? . -> . L3fn?)
  (type-case L4fn fn
    [l4mainfn (body) (l3mainfn (compile-L4expr body))]
    [l4fn (lbl args body) (l3fn lbl args (compile-L4expr body))]))

(define-with-contract (compile-L4expr expr)
  (L4expr? . -> . L3expr?)
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
  [fn-ctxt (done (listof L3-v?))
           (todo (listof L4expr?))
           (ctxt L4ctxt?)])

(define-with-contract (find expr ctxt varfn)
  (L4expr? L4ctxt? (-> symbol?) . -> . L3expr?)
  (type-case L4expr expr
    [l4e-let (id binding body)
             (find binding (let-ctxt id body ctxt) varfn)]
    [l4e-if (test then else)
            (find test (if-ctxt then else ctxt) varfn)]
    [l4e-app (fn args)
             (find fn (fn-ctxt '() args ctxt) varfn)]
    [l4e-v (v) (fill (l3t-v v) ctxt varfn)]))

(define-with-contract (fill var ctxt varfn)
  (L3term? L4ctxt? (-> symbol?) . -> . L3expr?)
  (type-case L4ctxt ctxt
    [mt-ctxt () (l3e-t var)]
    [let-ctxt (id body ctxt)
              (l3e-let id var (find body ctxt varfn))]
    [if-ctxt (then else ctxt)
             (if (l3t-v? var)
                 (l3e-if (l3t-v-v var) (find then ctxt varfn) (find else ctxt varfn))
                 (let ([id (varfn)])
                   (l3e-let id var (l3e-if id (find then ctxt varfn) (find else ctxt varfn)))))]
    [fn-ctxt (done todo ctxt)
             (cond
               [(null? todo)
                (let ([terms (reverse (cons (l3t-v-v var) done))])
                  (fill (l3t-apply (first terms) (rest terms)) ctxt varfn))]
               [(l3t-v? var)
                (find (first todo)
                      (fn-ctxt (cons (l3t-v-v var) done) (rest todo) ctxt)
                      varfn)]
               [else
                (let ([id (varfn)])
                  (l3e-let id var (find (first todo)
                                        (fn-ctxt (cons id done) (rest todo) ctxt)
                                        varfn)))])]))

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
