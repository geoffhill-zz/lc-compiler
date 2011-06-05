#lang plai

;;; EECS 322 L Compiler Suite
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "utils.rkt"))
(require (file "types.rkt"))
(require (file "input.rkt"))
(require (file "output.rkt"))

(require (file "L5.rkt"))
(require (file "L4.rkt"))
(require (file "L3.rkt"))
(require (file "L2.rkt"))
(require (file "L1.rkt"))

;;;
;;; EXTERNAL INTERFACE
;;;

(define-with-contract (main fn fname)
  (string? string? . -> . void?)
  (cond
    [(equal? fn "L1")
     (call-with-input-file fname main/L1)]
    [(equal? fn "spill")
     (call-with-input-file fname main/spill)]
    [(equal? fn "liveness")
     (call-with-input-file fname main/liveness)]
    [(equal? fn "graph")
     (call-with-input-file fname main/graph)]
    [(equal? fn "L2")
     (call-with-input-file fname main/L2)]
    [(equal? fn "L3")
     (call-with-input-file fname main/L3)]
    [(equal? fn "L4")
     (call-with-input-file fname main/L4)]
    [(equal? fn "L5")
     (call-with-input-file fname main/L5)]
    [(equal? fn "LC")
     (call-with-input-file fname main/LC)]
    [#t (error 'main "unrecognized function: ~a" fn)]))

(define-with-contract (main/L1 port)
  (input-port? . -> . void?)
  (display
   (compile-L1prog
    (build-L1prog
     (read port))))
  (void))

(define-with-contract (main/spill port)
  (input-port? . -> . void?)
  (let* ([stmts (map build-L2stmt (read port))]
         [spilled (spill stmts (read port) (read port) (make-counter (read port)))]
         [lstform (map format-L2stmt spilled)])
    (pretty-write lstform)
    (void)))

(define-with-contract (main/liveness port)
  (input-port? . -> . void?)
  (let* ([stmts (map build-L2stmt (read port))]
         [base (build-l2reg-base stmts)]
         [more (build-l2reg-succ base)]
         [liveness (build-l2reg-liveness more)]
         [ins (l2reg-liveness-ins liveness)]
         [outs (l2reg-liveness-outs liveness)]
         [ins-lst (vector->list (vector-map alphabetize ins))]
         [outs-lst (vector->list (vector-map alphabetize outs))])
    (pretty-write `(,(cons 'in ins-lst)
                    ,(cons 'out outs-lst)))
    (void)))

(define-with-contract (main/graph port)
  (input-port? . -> . void?)
  (let* ([stmts (map build-L2stmt (read port))]
         [base (build-l2reg-base stmts)]
         [more (build-l2reg-succ base)]
         [liveness (build-l2reg-liveness more)]
         [graph (build-l2reg-graph liveness)]
         [adjlist
          (map (λ (n)
                 (cons n
                       (alphabetize
                        (set-map
                         (set-filter
                          (l2reg-graph-edges graph)
                          (λ (e) (set-member? e n)))
                         (λ (e) (opposite e n))))))
               (alphabetize (l2reg-graph-nodes graph)))]
         [coloring (color (l2reg-graph-nodes graph)
                          (l2reg-graph-edges graph))]
         [colorlst (and
                    coloring
                    (map
                     (λ (p) `(,(car p) ,(cdr p)))
                     (filter
                      (λ (p) (not (set-member? used-regs (car p))))
                      (alphabetize (hash->list coloring)))))])
    (pretty-write adjlist)
    (pretty-write colorlst)
    (void)))

(define-with-contract (main/L2 port)
  (input-port? . -> . void?)
  (pretty-write
   (format-L1prog
    (compile-L2prog
     (build-L2prog (read port)))))
  (void))

(define-with-contract (main/L3 port)
  (input-port? . -> . void?)
  (pretty-write
   (format-L2prog
    (compile-L3prog
     (build-L3prog (read port)))))
  (void))

(define-with-contract (main/L4 port)
  (input-port? . -> . void?)
  (pretty-write
   (format-L3prog
    (compile-L4prog
     (build-L4prog (read port)))))
  (void))

(define-with-contract (main/L5 port)
  (input-port? . -> . void?)
  (pretty-write
   (format-L4prog
    (compile-L5expr
     (build-L5expr (read port)))))
  (void))

(define-with-contract (compile-LC src)
  (list? . -> . string?)
  (compile-L1prog
   (compile-L2prog
    (compile-L3prog
     (compile-L4prog
      (compile-L5expr
       (build-L5expr src)))))))

(define-with-contract (main/LC port)
  (input-port? . -> . void?)
  (display
   (compile-LC (read port)))
  (void))
