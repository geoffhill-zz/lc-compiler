#lang plai

;;; EECS 322 L2->L1 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "aggr-utils.rkt"))
(require (file "compiler-lib.rkt"))

;; L2stmt types
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
  [stmt-arrayerr (lhs L2-x?) (arg1 L2-s?) (arg2 L2-s?)])

;; labelmap type
;; maps labels to stmt numbers
;; defines constructor and contract
(define labelmap hash)
(define labelmap?
  (flat-named-contract
   'labelmap?
   (hash/c label? integer? #:immutable #t #:flat? #t)))

;; colormap type
;; maps variables to registers
;; defines constructor and contract
(define (colormap init-set)
  (make-immutable-hash (set-map init-set (λ (x) (cons x x)))))
(define colormap?
  (flat-named-contract
   'colormap?
   (hash/c L2-x? L1-x? #:immutable #t #:flat? #t)))

;; L2fn types
;; each type for different stage in the data buildup
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
              (coloring colormap?)])

;; creates an L2stmt from S-expr
(define/contract (build-stmt stmt)
  (any/c . -> . L2stmt?)
  (match stmt
    [`(,(? L2-x? lhs) <- ,(? L2-s? rhs)) (stmt-assign lhs rhs)]
    [`(,(? L2-x? lhs) <- (mem ,(? L2-x? base) ,(? n4? offset))) (stmt-memget lhs base offset)]
    [`((mem ,(? L2-x? base) ,(? n4? offset)) <- ,(? L2-s? rhs)) (stmt-memset base offset rhs)]
    [`(,(? L2-x? lhs) ,(? aop? op) ,(? L2-s? rhs)) (stmt-aop lhs op rhs)]
    [`(,(? L2-x? lhs) ,(? sop? op) ,(? L2-s? rhs)) (stmt-sop lhs op rhs)]
    [`(,(? L2-x? lhs) <- ,(? L2-s? c1) ,(? cmp? op) ,(? L2-s? c2)) (stmt-cmp lhs c1 op c2)]
    [(? label? lbl) (stmt-label lbl)]
    [`(goto ,(? label? lbl)) (stmt-goto lbl)]
    [`(cjump ,(? L2-s? c1) ,(? cmp? op) ,(? L2-s? c2) ,(? label? lbl1) ,(? label? lbl2))
     (stmt-cjump c1 op c2 lbl1 lbl2)]
    [`(call ,(? L2-s? dst)) (stmt-call dst)]
    [`(tail-call ,(? L2-s? dst)) (stmt-tcall dst)]
    [`(return) (stmt-return)]
    [`(,(? L2-x? lhs) <- (print ,(? L2-s? arg1))) (stmt-print lhs arg1)]
    [`(,(? L2-x? lhs) <- (allocate ,(? L2-s? arg1) ,(? L2-s? arg2))) (stmt-alloc lhs arg1 arg2)]
    [`(,(? L2-x? lhs) <- (array-error ,(? L2-s? arg1) ,(? L2-s? arg2))) (stmt-arrayerr lhs arg1 arg2)]
    [_ (error 'build-stmt "no matching clause for ~a" stmt)]))

;; gets the raw code form of an L2stmt
(define/contract (raw-stmt stmt)
  (L2stmt? . -> . any/c)
  (type-case L2stmt stmt
    [stmt-assign (lhs rhs) `(,lhs <- ,rhs)]
    [stmt-memget (lhs base offset) `(,lhs <- (mem ,base ,offset))]
    [stmt-memset (base offset rhs) `((mem ,base ,offset) <- ,rhs)]
    [stmt-aop (lhs op rhs) `(,lhs ,op ,rhs)]
    [stmt-sop (lhs op rhs) `(,lhs ,op ,rhs)]
    [stmt-cmp (lhs c1 op c2) `(,lhs <- ,c1 ,op ,c2)]
    [stmt-label (lbl) lbl]
    [stmt-goto (lbl) `(goto ,lbl)]
    [stmt-cjump (c1 op c2 lbl1 lbl2) `(cjump ,c1 ,op ,c2 ,lbl1 ,lbl2)]
    [stmt-call (dst) `(call ,dst)]
    [stmt-tcall (dst) `(tail-call ,dst)]
    [stmt-return () `(return)]
    [stmt-print (lhs arg1) `(,lhs <- (print ,arg1))]
    [stmt-alloc (lhs arg1 arg2) `(,lhs <- (allocate ,arg1 ,arg2))]
    [stmt-arrayerr (lhs arg1 arg2) `(,lhs <- (array-error ,arg1 ,arg2))]))

(define/contract (raw-stmts stmts)
  ((vectorof L2stmt?) . -> . list?)
  (vector->list (vector-map raw-stmt stmts)))

;; register sets used in gen/kill generations
;; just for reference
(define arg-regs (set 'ecx 'edx 'eax))
(define result-regs (set 'eax))
(define caller-save-regs (set 'eax 'ebx 'ecx 'edx))
(define callee-save-regs (set 'edi 'esi))
(define c-caller-save-regs (set 'eax 'ecx 'edx))
(define c-callee-save-regs (set 'ebx 'edi 'esi))

;; gets the gen set for an L2stmt
(define/contract (gen stmt)
  (L2stmt? . -> . (setof L2-x?))
  (list->set
   (filter L2-x?
           (type-case L2stmt stmt
             [stmt-assign (lhs rhs) `(,rhs)]
             [stmt-memget (lhs base offset) `(,base)]
             [stmt-memset (base offset rhs) `(,base ,rhs)]
             [stmt-aop (lhs op rhs) `(,lhs ,rhs)]
             [stmt-sop (lhs op rhs) `(,lhs ,rhs)]
             [stmt-cmp (lhs c1 op c2) `(,c1 ,c2)]
             [stmt-label (lbl) `()]
             [stmt-goto (lbl) `()]
             [stmt-cjump (c1 op c2 lbl1 lbl2) `(,c1 ,c2)]
             [stmt-call (dst) `(,dst eax ecx edx)]
             [stmt-tcall (dst) `(,dst eax ecx edx edi esi)]
             [stmt-return () `(eax edi esi)]
             [stmt-print (lhs arg1) `(,arg1)]
             [stmt-alloc (lhs arg1 arg2) `(,arg1 ,arg2)]
             [stmt-arrayerr (lhs arg1 arg2) `(,arg1 ,arg2)]))))

;; gets the kill set for an L2stmt
(define/contract (kill stmt)
  (L2stmt? . -> . (setof L2-x?))
  (list->set
   (filter L2-x?
           (type-case L2stmt stmt
             [stmt-assign (lhs rhs) `(,lhs)]
             [stmt-memget (lhs base offset) `(,lhs)]
             [stmt-memset (base offset rhs) `()]
             [stmt-aop (lhs op rhs) `(,lhs)]
             [stmt-sop (lhs op rhs) `(,lhs)]
             [stmt-cmp (lhs c1 op c2) `(,lhs)]
             [stmt-label (lbl) `()]
             [stmt-goto (lbl) `()]
             [stmt-cjump (c1 op c2 lbl1 lbl2) `()]
             [stmt-call (dst) `(eax ebx ecx edx)]
             [stmt-tcall (dst) `()]
             [stmt-return () `(eax ecx edx)]
             [stmt-print (lhs arg1) `(,lhs eax ecx edx)]
             [stmt-alloc (lhs arg1 arg2) `(,lhs eax ecx edx)]
             [stmt-arrayerr (lhs arg1 arg2) `(,lhs eax ecx edx)]))))

;; gets the full variable set for an L2stmt
;; including all the implicitly used variables
;; from gen/kill
(define/contract (var stmt)
  (L2stmt? . -> . (setof L2-x?))
  (set-union (gen stmt) (kill stmt)))
  
;; creates an L2fn-base from S-expr
(define/contract (build-l2fn-base lstform)
  (list? . -> . l2fn-base?)
  (let loop ([lstform lstform]
             [index 0]
             [stmts (make-vector (length lstform))]
             [lblmap (labelmap)])
    (if (null? lstform)
        (l2fn-base stmts lblmap)
        (let ([parsed-stmt (build-stmt (car lstform))])
          (vector-set! stmts index parsed-stmt)
          (loop (cdr lstform)
                (+ index 1)
                stmts
                (if (stmt-label? parsed-stmt)
                    (hash-set lblmap (stmt-label-lbl parsed-stmt) index)
                    lblmap))))))

(define/contract (spill fn name offset prefix)
  (l2fn-base? L2-x? n4? L2-x? . -> . l2fn-base?)
  (let* ([stmts (l2fn-base-stmts fn)]
         [len (vector-length stmts)])
    (let loop ([i 0]
               [temp-count 0]
               [accum '()])
      (if (= i len)
          (build-l2fn-base (map raw-stmt (reverse accum)))
          (let* ([stmt (vector-ref stmts i)]
                 [temp (temp-var prefix temp-count)]
                 [in-gen (set-member? (gen stmt) name)]
                 [in-kill (set-member? (kill stmt) name)])
            (cond
              ; special case: unneccesary assignment
              [(and (stmt-assign? stmt)
                    (equal? (stmt-assign-lhs stmt) name)
                    (equal? (stmt-assign-rhs stmt) name))
               (loop (+ i 1)
                     temp-count
                     accum)]
              ; special case: assignment lhs
              [(and (stmt-assign? stmt)
                    (equal? (stmt-assign-lhs stmt) name))
               (loop (+ i 1)
                     temp-count
                     (cons (stmt-memset 'ebp offset (stmt-assign-rhs stmt)) accum))]
              ; special case: assignment rhs
              [(and (stmt-assign? stmt)
                    (equal? (stmt-assign-rhs stmt) name))
               (loop (+ i 1)
                     temp-count
                     (cons (stmt-memget (stmt-assign-lhs stmt) 'ebp offset) accum))]
              ; general case: gen'd and kill'd
              [(and in-gen in-kill)
               (loop (+ i 1)
                     (+ temp-count 1)
                     (append
                      `(,(stmt-memset 'ebp offset temp)
                        ,(replace-stmt stmt name temp)
                        ,(stmt-memget temp 'ebp offset))
                      accum))]
              ; general case: gen'd only
              [in-gen
               (loop (+ i 1)
                     (+ temp-count 1)
                     (append
                      `(,(replace-stmt stmt name temp)
                        ,(stmt-memget temp 'ebp offset))
                      accum))]
              ; general case: kill'd only
              [in-kill
               (loop (+ i 1)
                     (+ temp-count 1)
                     (append
                      `(,(stmt-memset 'ebp offset temp)
                        ,(replace-stmt stmt name temp))
                      accum))]
              ; general case: not gen'd or kill'd
              [else
               (loop (+ i 1)
                     temp-count
                     (cons stmt accum))]))))))

;; create a temporary variable symbol by
;; concatenating a prefix and a counter
(define/contract (temp-var prefix counter)
  (L2-x? integer? . -> . L2-x?)
  (string->symbol (string-append (symbol->string prefix)
                                 (number->string counter))))

;; replaces all occurances of y with z in an L2stmt
(define/contract (replace-stmt stmt y z)
  (L2stmt? L2-x? L2-x? . -> . L2stmt?)
  (let ([r (λ (x) (if (equal? x y) z x))])
    (type-case L2stmt stmt
      [stmt-assign (lhs rhs) (stmt-assign (r lhs) (r rhs))]
      [stmt-memget (lhs base offset) (stmt-memget (r lhs) (r base) offset)]
      [stmt-memset (base offset rhs) (stmt-memset (r base) offset (r rhs))]
      [stmt-aop (lhs op rhs) (stmt-aop (r lhs) op (r rhs))]
      [stmt-sop (lhs op rhs) (stmt-sop (r lhs) op (r rhs))]
      [stmt-cmp (lhs c1 op c2) (stmt-cmp (r lhs) (r c1) op (r c2))]
      [stmt-label (lbl) (stmt-label lbl)]
      [stmt-goto (lbl) (stmt-goto lbl)]
      [stmt-cjump (c1 op c2 lbl1 lbl2) (stmt-cjump (r c1) op (r c2) lbl1 lbl2)]
      [stmt-call (dst) (stmt-call (r dst))]
      [stmt-tcall (dst) (stmt-tcall (r dst))]
      [stmt-return () (stmt-return)]
      [stmt-print (lhs arg1) (stmt-print (r lhs) (r arg1))]
      [stmt-alloc (lhs arg1 arg2) (stmt-alloc (r lhs) (r arg1) (r arg2))]
      [stmt-arrayerr (lhs arg1 arg2) (stmt-arrayerr (r lhs) (r arg1) (r arg2))])))

;; replaces all L2stmt variables with their colored regs
(define/contract (replace-coloring stmt coloring)
  (L2stmt? colormap? . -> . L2stmt?)
  (let ([r (λ (x) (hash-ref coloring x x))])
    (type-case L2stmt stmt
      [stmt-assign (lhs rhs) (stmt-assign (r lhs) (r rhs))]
      [stmt-memget (lhs base offset) (stmt-memget (r lhs) (r base) offset)]
      [stmt-memset (base offset rhs) (stmt-memset (r base) offset (r rhs))]
      [stmt-aop (lhs op rhs) (stmt-aop (r lhs) op (r rhs))]
      [stmt-sop (lhs op rhs) (stmt-sop (r lhs) op (r rhs))]
      [stmt-cmp (lhs c1 op c2) (stmt-cmp (r lhs) (r c1) op (r c2))]
      [stmt-label (lbl) (stmt-label lbl)]
      [stmt-goto (lbl) (stmt-goto lbl)]
      [stmt-cjump (c1 op c2 lbl1 lbl2) (stmt-cjump (r c1) op (r c2) lbl1 lbl2)]
      [stmt-call (dst) (stmt-call (r dst))]
      [stmt-tcall (dst) (stmt-tcall (r dst))]
      [stmt-return () (stmt-return)]
      [stmt-print (lhs arg1) (stmt-print (r lhs) (r arg1))]
      [stmt-alloc (lhs arg1 arg2) (stmt-alloc (r lhs) (r arg1) (r arg2))]
      [stmt-arrayerr (lhs arg1 arg2) (stmt-arrayerr (r lhs) (r arg1) (r arg2))])))

;; creates an L2fn-succ from L2fn-base
(define/contract (build-l2fn-succ l2fn)
  (l2fn-base? . -> . l2fn-succ?)
  (let* ([stmts (l2fn-base-stmts l2fn)]
         [lblmap (l2fn-base-lblmap l2fn)]
         [len (vector-length stmts)]
         [succs (make-vector len)])
    (for ([i (in-range len)])
      (vector-set!
       succs
       i
       (type-case L2stmt (vector-ref stmts i)
         [stmt-goto (lbl)
                    (set (hash-ref lblmap lbl))]
         [stmt-cjump (c1 op c2 lbl1 lbl2)
                     (set (hash-ref lblmap lbl1)
                          (hash-ref lblmap lbl2))]
         [stmt-tcall (dst) (set)]
         [stmt-return () (set)]
         [stmt-arrayerr (lhs arg1 arg2) (set)]
         [else (if (= i (- len 1))
                   (set)
                   (set (+ i 1)))])))
    (l2fn-succ stmts succs)))

;; creates an L2fn-liveness frpm L2fn-succ
(define/contract (build-l2fn-liveness l2fn)
  (l2fn-succ? . -> . l2fn-liveness?)
  (let* ([stmts (l2fn-succ-stmts l2fn)]
         [succs (l2fn-succ-succs l2fn)]
         [len (vector-length stmts)])
    (let loop ([ins (vector-map gen stmts)]
               [outs (build-vector len (λ (n) (set)))])
      (let ([new-ins (vector-copy ins)]
            [new-outs (vector-copy outs)])
        (for ([i (in-range len)])
          (vector-set! new-ins i
                       (set-union (vector-ref ins i)
                                  (set-subtract (vector-ref outs i)
                                                (kill (vector-ref stmts i)))))
          (vector-set! new-outs i
                       (setlst-union (set-map (vector-ref succs i)
                                              (λ (s) (vector-ref ins s))))))
        (if (and (equal? ins new-ins) (equal? outs new-outs))
            (l2fn-liveness stmts ins outs)
            (loop new-ins new-outs))))))

;; register sets for graph generation
(define all-regs (set 'eax 'ebx 'ecx 'edx 'edi 'esi 'ebp 'esp))
(define used-regs (set 'eax 'ebx 'ecx 'edx 'edi 'esi))
(define ignored-regs (set 'ebp 'esp))

;; predicate to see if a set contains no ignored regs
(define/contract (no-ignored-regs s)
  (set? . -> . boolean?)
  (set-empty? (set-intersect s ignored-regs)))

;; generates the set of nodes of the graph
(define/contract (graph-nodes l2fn)
  (l2fn-liveness? . -> . (setof L2-x?))
  (set-union
   used-regs
   (setlst-union
    (vector->list
     (vector-map (λ (s) (set-subtract s ignored-regs))
                 (vector-map var (l2fn-liveness-stmts l2fn)))))))

(define valid-cmp-regs (set 'eax 'ebx 'ecx 'edx))
(define valid-sop-regs (set 'ecx))

;; generates the set of edges of the graph
(define/contract (graph-edges l2fn)
  (l2fn-liveness? . -> . (setof (pairof L2-x?)))
  (let* ([stmts (l2fn-liveness-stmts l2fn)]
         [len (vector-length stmts)]
         [ins (l2fn-liveness-ins l2fn)]
         [outs (l2fn-liveness-outs l2fn)])
    (let loop ([i 0]
               [edges (powerset used-regs)])
      (if (= i len)
          edges
          (let* ([stmt (vector-ref stmts i)]
                 [in (vector-ref ins i)]
                 [out (vector-ref outs i)]
                 [interfering-pairs
                  (set-union
                   (powerset (set-union out (kill stmt)))
                   (if (= i 0) (powerset (gen stmt)) (set))
                   (if (stmt-sop? stmt)
                       (pairs (stmt-sop-lhs stmt)
                              (set-subtract used-regs valid-sop-regs))
                       (set))
                   (if (stmt-cmp? stmt)
                       (pairs (stmt-cmp-lhs stmt)
                              (set-subtract used-regs valid-cmp-regs))
                       (set)))]
                 [important-pairs (set-filter interfering-pairs no-ignored-regs)])
            (loop (+ i 1) (set-union edges important-pairs)))))))

;; creates an L2fn-graph from L2fn-liveness
(define/contract (build-l2fn-graph l2fn)
  (l2fn-liveness? . -> . l2fn-graph?)
  (let ([nodes (graph-nodes l2fn)]
        [edges (graph-edges l2fn)])
    (l2fn-graph (l2fn-liveness-stmts l2fn) nodes edges)))

; TODO: fix prefixing
(define spill-prefix 'vdvdvdvd_)

(define/contract (build-l2fn-color l2fn)
  (l2fn-graph? . -> . l2fn-color?)
  (let loop ([l2fn l2fn]
             [offset 0]
             [spillables (set-subtract (l2fn-graph-nodes l2fn) used-regs)])
    (let* ([stmts (l2fn-graph-stmts l2fn)]
           [nodes (l2fn-graph-nodes l2fn)]
           [edges (l2fn-graph-edges l2fn)]
           [coloring (color nodes edges)])
      (if coloring
          (l2fn-color stmts coloring) ; TODO: change stack pointer to match offset
          (if (set-empty? spillables)
              (error 'l2 "no variables left to spill")
              (let-values ([(next rest) (choose-spill-var spillables nodes edges)])
                (let* ([new-base (spill (build-l2fn-base (raw-stmts stmts))
                             next
                             (- offset 4)
                             spill-prefix)]
                       [new-succ (build-l2fn-succ new-base)]
                       [new-liveness (build-l2fn-liveness new-succ)]
                       [new-graph (build-l2fn-graph new-liveness)])
                (loop new-graph (- offset 4) rest))))))))

(define/contract (choose-spill-var spillables nodes edges)
  ((non-empty-setof L2-x?) (setof L2-x?) (setof (pairof L2-x?)) . -> . (values L2-x? (setof L2-x?)))
  (let ([splst (set->list spillables)])
    (values (first splst) (list->set (rest splst)))))

;; create a color mapping from an interference graph
(define/contract (color nodes edges)
  ((setof L2-x?) (setof (pairof L2-x?)) . -> . (or/c colormap? false?))
  (build-colormap (set-subtract nodes used-regs)
                  nodes
                  edges
                  (colormap used-regs)))

(define/contract (build-colormap rem-nodes nodes edges colors)
  ((setof L2-x?) (setof L2-x?) (setof (pairof L2-x?)) colormap? . -> . (or/c colormap? false?))
  (if (set-empty? rem-nodes)
      colors
      (let-values ([(next rest) (choose-next-node rem-nodes nodes edges)])
        (let* ([possible-edges (pairs next used-regs)]
               [valid-edges (set-subtract possible-edges edges)])
          (and (not (set-empty? valid-edges))
               (let* ([edge (choose-edge valid-edges nodes edges)]
                      [reg (reg-from-edge edge)]
                      [new-edges (interference-union edge edges)])
                 (build-colormap rest nodes new-edges (hash-set colors next reg))))))))

(define/contract (choose-next-node rem-nodes nodes edges)
  ((non-empty-setof L2-x?) (setof L2-x?) (setof (pairof L2-x?)) . -> . (values L2-x? (setof L2-x?)))
  (let ([nodelst (set->list rem-nodes)])
    (values (first nodelst) (list->set (rest nodelst)))))

(define/contract (choose-edge valid-edges nodes egdes)
  ((non-empty-setof (pairof L2-x?)) (setof L2-x?) (setof (pairof L2-x?)) . -> . (pairof L2-x?))
  (first (set->list valid-edges)))

;; extends edges so that everything that interfered
;; with one member of edge also interferes with other
(define/contract (interference-union edge edges)
  ((pairof L2-x?) (setof (pairof L2-x?)) . -> . (setof (pairof L2-x?)))
  (let* ([ns (set->list edge)]
         [n1 (first ns)]
         [n2 (second ns)])
    (set-union edges
               (list->set
                (set-map (set-filter edges (λ (e) (set-member? e n1)))
                         (λ (e) (set n2 (opposite e n1)))))
               (list->set
                (set-map (set-filter edges (λ (e) (set-member? e n2)))
                         (λ (e) (set n1 (opposite e n2))))))))

(define/contract (reg-from-edge edge)
  ((pairof L2-x?) . -> . L2-x?)
  (first (set->list (set-intersect edge all-regs))))

(define/contract (opposite edge node)
  ((pairof L2-x?) L2-x? . -> . L2-x?)
  (first (set->list (set-remove edge node))))

(define/contract (compile-l2 src)
  ((listof list?) . -> . (listof list?))
  (map compile-l2fn src))

(define/contract (compile-l2fn fn)
  (list? . -> . list?)
  (let* ([base (build-l2fn-base fn)]
         [more (build-l2fn-succ base)]
         [liveness (build-l2fn-liveness more)]
         [graph (build-l2fn-graph liveness)]
         [color (build-l2fn-color graph)])
    (raw-stmts
     (vector-map
      (λ (stmt) (replace-coloring stmt (l2fn-color-coloring color)))
      (l2fn-color-stmts color)))))


;; main function
;; calls helper mains based on first arg
(define/contract (main fn fname)
  (string? string? . -> . any/c)
  (cond
    [(equal? fn "spill")
     (call-with-input-file fname main/spill)]
    [(equal? fn "liveness")
     (call-with-input-file fname main/liveness)]
    [(equal? fn "graph")
     (call-with-input-file fname main/graph)]
    [(equal? fn "compile")
     (call-with-input-file fname main/compile)]
    [#t (error 'main "unrecognized function: ~a" fn)]))

(define/contract (main/spill port)
  (input-port? . -> . any/c)
  (let* ([base (build-l2fn-base (read port))]
         [spilled (spill base (read port) (read port) (read port))]
         [lstform (raw-stmts (l2fn-base-stmts spilled))])
    (pretty-write lstform)))

(define/contract (main/liveness port)
  (input-port? . -> . any/c)
  (let* ([base (build-l2fn-base (read port))]
         [more (build-l2fn-succ base)]
         [liveness (build-l2fn-liveness more)]
         [ins (l2fn-liveness-ins liveness)]
         [outs (l2fn-liveness-outs liveness)]
         [ins-lst (vector->list (vector-map alphabetize ins))]
         [outs-lst (vector->list (vector-map alphabetize outs))])
    (pretty-write `(,(cons 'in ins-lst)
                    ,(cons 'out outs-lst)))))

(define/contract (main/graph port)
  (input-port? . -> . any/c)
  (let* ([base (build-l2fn-base (read port))]
         [more (build-l2fn-succ base)]
         [liveness (build-l2fn-liveness more)]
         [graph (build-l2fn-graph liveness)]
         [adjlist
          (map (λ (n)
                 (cons n
                       (alphabetize
                        (set-map
                         (set-filter
                          (l2fn-graph-edges graph)
                          (λ (e) (set-member? e n)))
                         (λ (e) (opposite e n))))))
           (alphabetize (l2fn-graph-nodes graph)))]
         [coloring (color (l2fn-graph-nodes graph) (l2fn-graph-edges graph))]
         [colorlst (and
                    coloring
                    (map
                    (λ (p) `(,(car p) ,(cdr p)))
                    (filter
                     (λ (p) (not (set-member? used-regs (car p))))
                     (alphabetize (hash->list coloring)))))])
    (pretty-write adjlist)
    (pretty-write colorlst)))

(define/contract (main/compile port)
  (input-port? . -> . any/c)
  (pretty-write (compile-l2 (read port))))