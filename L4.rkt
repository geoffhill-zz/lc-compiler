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
;; variables do not carry across functions
;; lets always generate new variables, ignoring existing ones

(define-with-contract (rename-L4prog prog)
  (L4prog? . -> . L4prog?)
  (define varfn (make-counter 'var_))
  (type-case L4prog prog
    [l4prog (main others)
            (l4prog (rename-L4fn main varfn)
                    (map (λ (fn) (rename-L4fn fn varfn)) others))]))

(define-with-contract (rename-L4fn fn varfn)
  (L4fn? (-> symbol?) . -> . L4fn?)
  (type-case L4fn fn
    [l4mainfn (body)
              (l4mainfn (rename-L4expr body (namemap) varfn))]
    [l4fn (lbl args body)
          (let-values ([(newargs changes)
                        (let loop ([args args]
                                   [renamed '()]
                                   [changes (namemap)])
                          (if (null? args)
                              (values (reverse renamed) changes)
                              (let* ([arg (first args)]
                                     [newarg (hash-ref changes arg varfn)]
                                     [changes (hash-set changes arg newarg)])
                                (loop (rest args) (cons newarg renamed) changes))))])
            (l4fn lbl newargs (rename-L4expr body changes varfn)))]))

(define-with-contract (rename-L4expr expr changes varfn)
  (L4expr? namemap? (-> symbol?) . -> . L4expr?)
  (type-case L4expr expr
    [l4e-let (id binding body)
             (let* ([newid (varfn)]
                    [newbinding (rename-L4expr binding changes varfn)]
                    [newchanges (hash-set changes id newid)]
                    [newbody (rename-L4expr body newchanges varfn)])
               (l4e-let newid newbinding newbody))]
    [l4e-if (test then else)
            (let* ([newtest (rename-L4expr test changes varfn)]
                   [newthen (rename-L4expr then changes varfn)]
                   [newelse (rename-L4expr else changes varfn)])
              (l4e-if newtest newthen newelse))]
    [l4e-app (fn args)
             (let ([newfn (rename-L4expr fn changes varfn)]
                   [newargs (let loop ([args args]
                                       [renamed '()])
                              (if (null? args)
                                  (reverse renamed)
                                  (let ([newarg (rename-L4expr (first args) changes varfn)])
                                    (loop (rest args) (cons newarg renamed)))))])
               (l4e-app newfn newargs))]
    [l4e-begin (fst snd)
               (let* ([newfst (rename-L4expr fst changes varfn)]
                      [newsnd (rename-L4expr snd changes varfn)])
                 (l4e-begin newfst newsnd))]
    [l4e-v (v)
           (if (or (number? v) (label? v) (L4-builtin? v))
               (l4e-v v)
               (l4e-v (hash-ref changes v varfn)))]))


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
