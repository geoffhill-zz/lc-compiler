#lang slideshow
(require slideshow/code)

(slide
 #:title "An L2 to L1 Compiler"
 (vc-append 60.0
            (t "Using PLAI to structure transformations.")
            (code \#lang plai)
            (vc-append 30.0
                       (t "Passes 158 out of 161 (98%) tests.")
                       (t "Still full of bugs, known and unknown."))))

(slide
 #:title "An L2 to L1 Compiler"
 (vc-append 50.0
            (t "PLAI: a type for statements")
            (scale
             (code
              (define-type L2stmt
                [stmt-assign (lhs L2-x?) (rhs L2-s?)]
                [stmt-memget (lhs L2-x?) (base L2-x?) (offset n4?)]
                [stmt-memset (base L2-x?) (offset n4?) (rhs L2-s?)]
                [stmt-aop (lhs L2-x?) (op aop?) (rhs L2-s?)]
                [stmt-sop (lhs L2-x?) (op sop?) (rhs L2-s?)]
                [stmt-cmp (lhs L2-x?) (c1 L2-s?) (op cmp?) (c2 L2-s?)]
                [stmt-label (lbl label?)]
                [stmt-goto (lbl label?)]
                [stmt-cjump (c1 L2-s?) (op cmp?) (c2 L2-s?) (lbl1 label?) (lbl2 label?)]
                [stmt-call (dst L2-s?)]
                [stmt-tcall (dst L2-s?)]
                [stmt-return]
                [stmt-print (lhs L2-x?) (arg1 L2-s?)]
                [stmt-alloc (lhs L2-x?) (arg1 L2-s?) (arg2 L2-s?)]
                [stmt-arrayerr (lhs L2-x?) (arg1 L2-s?) (arg2 L2-s?)]))
             0.6)))

(slide
 #:title "An L2 to L1 Compiler"
 (vc-append 50.0
            (t "PLAI: a type for functions (multiple stages)")
            (scale
             (code
              (define-type L2fn
                [l2fn-base (stmts (vectorof L2stmt?))
                           (lblmap labelmap?)]
                [l2fn-succ (stmts (vectorof L2stmt?))
                           (succs (vectorof (setof integer?)))]
                [l2fn-liveness (stmts (vectorof L2stmt?))
                               (ins (vectorof (setof L2-x?)))
                               (outs (vectorof (setof L2-x?)))]
                [l2fn-graph (stmts (vectorof L2stmt?))
                            (nodes (setof L2-x?))
                            (edges (setof (pairof L2-x?)))]
                [l2fn-color (stmts (vectorof L2stmt?))
                            (coloring colormap?)]))
             0.8)))

(slide
 #:title "An L2 to L1 Compiler"
 (hc-append 30.0
            (vc-append
             20.0
             (t "L2 S-Expression")
             (arrow 20.0 (* pi 1.5))
             (code l2fn-base)
             (arrow 20.0 (* pi 1.5))
             (code l2fn-succ)
             (arrow 20.0 (* pi 1.5))
             (code l2fn-liveness)
             (arrow 20.0 (* pi 1.5))
             (code l2fn-graph)
             (arrow 20.0 (* pi 1.5))
             (code l2fn-color)
             (arrow 20.0 (* pi 1.5))
             (t "L1 S-Expression"))
            (scale
             (vl-append
              80.0
              (code (define-with-contract (build-l2fn-base lstform)
                      (-> list? l2fn-base?)
                      ...))
              (code (define-with-contract (build-l2fn-succ l2fn)
                      (-> l2fn-base? l2fn-succ?)
                      ...))
              (code (define-with-contract (build-l2fn-liveness l2fn)
                      (-> l2fn-succ? l2fn-liveness?)
                      ...))
              (code (define-with-contract (build-l2fn-graph l2fn)
                      (-> l2fn-liveness? l2fn-graph?)
                      ...))
              (code (define-with-contract (build-l2fn-color l2fn)
                      (-> l2fn-graph? l2fn-color?)
                      ...))
              (code (define-with-contract (compile-stmts stmts)
                      (-> (vectorof L2stmt?) list?)
                      ...)))
             0.5)))

(slide
 #:title "An L2 to L1 Compiler"
 (vc-append 50.0
            (t "Other data types:")
            (vl-append 10.0
                       (ht-append 30.0 (t "Statement list within functions.") (code vector?) )
                       (ht-append 30.0 (t "Gen set, kill set, in set, out set.") (code set?))
                       (ht-append 30.0 (t "Single edge, graph node/edge sets.") (code set?))
                       (ht-append 30.0 (t "Label mapping, color mapping.") (code hash?)))))

(slide
 #:title "An L2 to L1 Compiler"
 (vc-append 50.0
            (t "Using Racket's contract system:")
            (vl-append 10.0
                       (t "Makes extensive use of contracts.")
                       (t "Predicates are defined in separate file for crossover use.")
                       (t "Every predicate is a named contract for easier debugging."))))
