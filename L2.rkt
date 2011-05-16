#lang plai

;;; EECS 322 L2->L1 Compiler
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "types.rkt"))
(require (file "preds.rkt"))
(require (file "utils.rkt"))

;;;
;;; GENERAL UTILITY FUNCTIONS
;;;

;; executes a replacement function on every var in an L2stmt
(define/contract (replace-stmt-vars stmt r)
  (L2stmt? (L2-s? . -> . L2-s?) . -> . L2stmt?)
  (type-case L2stmt stmt
      [l2s-assign (lhs rhs) (l2s-assign (r lhs) (r rhs))]
      [l2s-memget (lhs base offset) (l2s-memget (r lhs) (r base) offset)]
      [l2s-memset (base offset rhs) (l2s-memset (r base) offset (r rhs))]
      [l2s-aop (lhs op rhs) (l2s-aop (r lhs) op (r rhs))]
      [l2s-sop (lhs op rhs) (l2s-sop (r lhs) op (r rhs))]
      [l2s-cmp (lhs c1 op c2) (l2s-cmp (r lhs) (r c1) op (r c2))]
      [l2s-label (lbl) (l2s-label lbl)]
      [l2s-goto (lbl) (l2s-goto lbl)]
      [l2s-cjump (c1 op c2 lbl1 lbl2) (l2s-cjump (r c1) op (r c2) lbl1 lbl2)]
      [l2s-call (dst) (l2s-call (r dst))]
      [l2s-tcall (dst) (l2s-tcall (r dst))]
      [l2s-return () (l2s-return)]
      [l2s-print (lhs arg1) (l2s-print (r lhs) (r arg1))]
      [l2s-alloc (lhs arg1 arg2) (l2s-alloc (r lhs) (r arg1) (r arg2))]
      [l2s-arrayerr (lhs arg1 arg2) (l2s-arrayerr (r lhs) (r arg1) (r arg2))]))

;;;
;;; REGISTER ALLOCATION TYPES
;;;

;; labelmap type
;; immutable hash, maps labels to stmt numbers
;; defines constructor and contract
(define labelmap hash)
(define labelmap?
  (flat-named-contract
   'labelmap?
   (hash/c label? integer? #:immutable #t #:flat? #t)))

;; colormap type
;; immutable hash, maps variables to registers
;; defines constructor and contract
(define (colormap init-set)
  (make-immutable-hash (set-map init-set (λ (x) (cons x x)))))
(define colormap?
  (flat-named-contract
   'colormap?
   (hash/c L2-x? L1-x? #:immutable #t #:flat? #t)))

;; L2reg types for register allocation
;; each type for different stage in the data buildup
(define-type L2reg
  [l2reg-base (stmts (vectorof L2stmt?))
              (lblmap labelmap?)]
  [l2reg-succ (stmts (vectorof L2stmt?))
              (succs (vectorof (setof integer?)))]
  [l2reg-liveness (stmts (vectorof L2stmt?))
                  (ins (vectorof (setof L2-x?)))
                  (outs (vectorof (setof L2-x?)))]
  [l2reg-graph (stmts (vectorof L2stmt?))
               (nodes (setof L2-x?))
               (edges (setof (pairof L2-x?)))]
  [l2reg-color (stmts (vectorof L2stmt?))
               (coloring colormap?)])

;;;
;;; L2REG-BASE GENERATION
;;;

;; creates an L2reg-base from an L2fn
(define/contract (build-l2reg-base fn)
  (L2fn? . -> . l2reg-base?)
  (type-case L2fn fn
    [l2fn (stmts)
          (let ([vecstmts (make-vector (length stmts))])
            (let loop ([stmts stmts]
                       [index 0]
                       [lblmap (labelmap)])
              (if (null? stmts)
                  (l2reg-base vecstmts lblmap)
                  (let ([stmt (car stmts)])
                    (vector-set! vecstmts index stmt)
                    (loop (cdr stmts)
                          (+ index 1)
                          (if (l2s-label? stmt)
                              (hash-set lblmap (l2s-label-lbl stmt) index)
                              lblmap))))))]))

;;;
;;; L2REG-SUCC GENERATION
;;;

;; creates an L2reg-succ from an L2reg-base
(define/contract (build-l2reg-succ l2fn)
  (l2reg-base? . -> . l2reg-succ?)
  (let* ([stmts (l2reg-base-stmts l2fn)]
         [lblmap (l2reg-base-lblmap l2fn)]
         [len (vector-length stmts)]
         [succs (make-vector len)])
    (for ([i (in-range len)])
      (vector-set! succs i (successors (vector-ref stmts i) lblmap i len)))
    (l2reg-succ stmts succs)))

;; gets the set of all successors of an L2stmt
;; within the context of an function
(define/contract (successors stmt lblmap line len)
  (L2stmt? labelmap? integer? integer? . -> . (set/c integer?))
  (type-case L2stmt stmt
    [l2s-goto (lbl)
               (set (hash-ref lblmap lbl))]
    [l2s-cjump (c1 op c2 lbl1 lbl2)
                (set (hash-ref lblmap lbl1)
                     (hash-ref lblmap lbl2))]
    [l2s-tcall (dst) (set)]
    [l2s-return () (set)]
    [l2s-arrayerr (lhs arg1 arg2) (set)]
    [else (if (= line (- len 1))
              (set)
              (set (+ line 1)))]))

;;;
;;; L2REG-LIVENESS GENERATION
;;;

;; creates an L2reg-liveness frpm L2reg-succ
(define/contract (build-l2reg-liveness l2fn)
  (l2reg-succ? . -> . l2reg-liveness?)
  (let* ([stmts (l2reg-succ-stmts l2fn)]
         [succs (l2reg-succ-succs l2fn)]
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
            (l2reg-liveness stmts ins outs)
            (loop new-ins new-outs))))))

;; gets the gen set for an L2stmt
(define/contract (gen stmt)
  (L2stmt? . -> . (setof L2-x?))
  (list->set
   (filter L2-x?
           (type-case L2stmt stmt
             [l2s-assign (lhs rhs) `(,rhs)]
             [l2s-memget (lhs base offset) `(,base)]
             [l2s-memset (base offset rhs) `(,base ,rhs)]
             [l2s-aop (lhs op rhs) `(,lhs ,rhs)]
             [l2s-sop (lhs op rhs) `(,lhs ,rhs)]
             [l2s-cmp (lhs c1 op c2) `(,c1 ,c2)]
             [l2s-label (lbl) `()]
             [l2s-goto (lbl) `()]
             [l2s-cjump (c1 op c2 lbl1 lbl2) `(,c1 ,c2)]
             [l2s-call (dst) `(,dst eax ecx edx)]
             [l2s-tcall (dst) `(,dst eax ecx edx edi esi)]
             [l2s-return () `(eax edi esi)]
             [l2s-print (lhs arg1) `(,arg1)]
             [l2s-alloc (lhs arg1 arg2) `(,arg1 ,arg2)]
             [l2s-arrayerr (lhs arg1 arg2) `(,arg1 ,arg2)]))))

;; gets the kill set for an L2stmt
(define/contract (kill stmt)
  (L2stmt? . -> . (setof L2-x?))
  (list->set
   (filter L2-x?
           (type-case L2stmt stmt
             [l2s-assign (lhs rhs) `(,lhs)]
             [l2s-memget (lhs base offset) `(,lhs)]
             [l2s-memset (base offset rhs) `()]
             [l2s-aop (lhs op rhs) `(,lhs)]
             [l2s-sop (lhs op rhs) `(,lhs)]
             [l2s-cmp (lhs c1 op c2) `(,lhs)]
             [l2s-label (lbl) `()]
             [l2s-goto (lbl) `()]
             [l2s-cjump (c1 op c2 lbl1 lbl2) `()]
             [l2s-call (dst) `(eax ebx ecx edx)]
             [l2s-tcall (dst) `()]
             [l2s-return () `(eax ecx edx)]
             [l2s-print (lhs arg1) `(,lhs eax ecx edx)]
             [l2s-alloc (lhs arg1 arg2) `(,lhs eax ecx edx)]
             [l2s-arrayerr (lhs arg1 arg2) `(,lhs eax ecx edx)]))))

;;;
;;; L2REG-GRAPH GENERATION
;;;

;; creates an L2reg-graph from L2reg-liveness
(define/contract (build-l2reg-graph l2fn)
  (l2reg-liveness? . -> . l2reg-graph?)
  (let ([nodes (graph-nodes l2fn)]
        [edges (graph-edges l2fn)])
    (l2reg-graph (l2reg-liveness-stmts l2fn) nodes edges)))

;; register sets for graph generation
(define all-regs (set 'eax 'ebx 'ecx 'edx 'edi 'esi 'ebp 'esp))
(define used-regs (set 'eax 'ebx 'ecx 'edx 'edi 'esi))
(define ignored-regs (set 'ebp 'esp))
(define valid-cmp-regs (set 'eax 'ebx 'ecx 'edx))
(define valid-sop-regs (set 'ecx))

;; predicate to see if a set contains no ignored regs
(define/contract (no-ignored-regs s)
  (set? . -> . boolean?)
  (set-empty? (set-intersect s ignored-regs)))

;; generates the set of nodes of the graph
(define/contract (graph-nodes l2fn)
  (l2reg-liveness? . -> . (setof L2-x?))
  (set-union
   used-regs
   (setlst-union
    (vector->list
     (vector-map (λ (s) (set-subtract s ignored-regs))
                 (vector-map var (l2reg-liveness-stmts l2fn)))))))

;; gets the full variable set for an L2stmt
;; including all the implicitly used variables
;; from gen/kill
(define/contract (var stmt)
  (L2stmt? . -> . (setof L2-x?))
  (set-union (gen stmt) (kill stmt)))

;; generates the set of edges of the graph
(define/contract (graph-edges l2fn)
  (l2reg-liveness? . -> . (setof (pairof L2-x?)))
  (let* ([stmts (l2reg-liveness-stmts l2fn)]
         [len (vector-length stmts)]
         [ins (l2reg-liveness-ins l2fn)]
         [outs (l2reg-liveness-outs l2fn)])
    (let loop ([i 0]
               [edges (powerset used-regs)])
      (if (= i len)
          edges
          (let* ([stmt (vector-ref stmts i)]
                 [in (vector-ref ins i)]
                 [out (vector-ref outs i)]
                 [toomany-interfering-pairs
                  (set-union
                   ; all var combos of the kill set of every stmt
                   (powerset (set-union out (kill stmt)))
                   ; all var combos of the gen set of first stmt
                   (if (= i 0) (powerset (gen stmt)) (set))
                   ; all regs that cannot be result of a shift for sop stmt
                   (if (l2s-sop? stmt)
                       (pairs (l2s-sop-lhs stmt)
                              (set-subtract used-regs valid-sop-regs))
                       (set))
                   ; all regs that cannot be result of a compare for cmp stmt
                   (if (l2s-cmp? stmt)
                       (pairs (l2s-cmp-lhs stmt)
                              (set-subtract used-regs valid-cmp-regs))
                       (set)))]
                 [interfering-pairs
                   (type-case L2stmt stmt
                     [l2s-assign (lhs rhs) (set-remove toomany-interfering-pairs (set lhs rhs))]
                     [else toomany-interfering-pairs])]
                 [important-pairs (set-filter interfering-pairs no-ignored-regs)])
            (loop (+ i 1) (set-union edges important-pairs)))))))

;;;
;;; L3FN-COLOR GENERATION
;;;

; TODO: fix prefixing
(define spill-prefix '____spillpref)

;; creates an L2reg-color from L2reg-graph
(define/contract (build-l2reg-color l2fn)
  (l2reg-graph? . -> . l2reg-color?)
  (let loop ([l2fn l2fn]
             [offset 0]
             [spillables (set-subtract (l2reg-graph-nodes l2fn) all-regs)])
    (let* ([stmts (l2reg-graph-stmts l2fn)]
           [nodes (l2reg-graph-nodes l2fn)]
           [edges (l2reg-graph-edges l2fn)]
           [coloring (color nodes edges)])
      (if coloring
          (l2reg-color (fix-stack stmts offset) coloring)
          (if (set-empty? spillables)
              (error 'l2 "no variables left to spill")
              (let-values ([(next rest) (choose-spill-var spillables nodes edges)])
                (let* ([fn (build-L2fn (vector->list
                                        (vector-map format-L2stmt stmts)))]
                       [spilled-fn (spill fn next (- offset 4) spill-prefix)]
                       [new-base (build-l2reg-base spilled-fn)]
                       [new-succ (build-l2reg-succ new-base)]
                       [new-liveness (build-l2reg-liveness new-succ)]
                       [new-graph (build-l2reg-graph new-liveness)])
                  (loop new-graph (- offset 4) rest))))))))

;; spills a variable in an L2fn
(define/contract (spill fn name offset prefix)
  (L2fn? L2-x? n4? L2-x? . -> . L2fn?)
  (let loop ([stmts (l2fn-stmts fn)]
             [temp-count 0]
             [accum '()])
    (if (null? stmts)
        (l2fn (reverse accum))
        (let* ([stmt (car stmts)]
               [temp (temp-var prefix temp-count)]
               [in-gen (set-member? (gen stmt) name)]
               [in-kill (set-member? (kill stmt) name)]
               [rfn (λ (x) (if (equal? x name) temp x))])
          (cond
            ; special case: unneccesary assignment
            [(and (l2s-assign? stmt)
                  (equal? (l2s-assign-lhs stmt) name)
                  (equal? (l2s-assign-rhs stmt) name))
             (loop (cdr stmts) temp-count accum)]
            ; special case: assignment lhs
            [(and (l2s-assign? stmt)
                  (equal? (l2s-assign-lhs stmt) name))
             (loop (cdr stmts)
                   temp-count
                   (cons (l2s-memset 'ebp offset (l2s-assign-rhs stmt)) accum))]
            ; special case: assignment rhs
            [(and (l2s-assign? stmt)
                  (equal? (l2s-assign-rhs stmt) name))
             (loop (cdr stmts)
                   temp-count
                   (cons (l2s-memget (l2s-assign-lhs stmt) 'ebp offset) accum))]
            ; general case: gen'd and kill'd
            [(and in-gen in-kill)
             (loop (cdr stmts)
                   (+ temp-count 1)
                   (append
                    `(,(l2s-memset 'ebp offset temp)
                      ,(replace-stmt-vars stmt rfn)
                      ,(l2s-memget temp 'ebp offset))
                    accum))]
            ; general case: gen'd only
            [in-gen
             (loop (cdr stmts)
                   (+ temp-count 1)
                   (append
                    `(,(replace-stmt-vars stmt rfn)
                      ,(l2s-memget temp 'ebp offset))
                    accum))]
            ; general case: kill'd only
            [in-kill
             (loop (cdr stmts)
                   (+ temp-count 1)
                   (append
                    `(,(l2s-memset 'ebp offset temp)
                      ,(replace-stmt-vars stmt rfn))
                    accum))]
            ; general case: not gen'd or kill'd
            [else
             (loop (cdr stmts) temp-count (cons stmt accum))])))))


;; given a spillable set, choose the best one to spill
;; policy decision
(define/contract (choose-spill-var spillables nodes edges)
  ((non-empty-setof L2-x?) (setof L2-x?) (setof (pairof L2-x?)) . -> . (values L2-x? (setof L2-x?)))
  (let ([splst (alphabetize spillables)])
    (values (first splst) (list->set (rest splst)))))

;; add statements to fix stack alignment
;; ensures that labeled functions stay labeled
;; TODO: fix possible multiple stack decrement bug (check interpreter)
;; TODO: fix addition of esp to every return/tail-call
(define/contract (fix-stack stmts offset)
  ((vectorof L2stmt?) integer? . -> . (vectorof L2stmt?))
  (if (zero? offset)
      stmts
      (let* ([pos-offset (- 0 offset)]
             [len (+ (vector-length stmts) 2)]
             [newstmts (make-vector len)])
        (if (l2s-label? (vector-ref stmts 0))
            (begin
              (vector-set! newstmts 0 (vector-ref stmts 0))
              (vector-set! newstmts 1 (l2s-aop 'esp '-= pos-offset))
              (vector-copy! newstmts 2 stmts 1))
            (begin
              (vector-set! newstmts 0 (l2s-aop 'esp '-= pos-offset))
              (vector-copy! newstmts 1 stmts)))
        (vector-set! newstmts (- len 1) (l2s-aop 'esp '+= pos-offset))
        newstmts)))

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

;; given a colorable node set, choose the best one to color
;; policy decision
(define/contract (choose-next-node rem-nodes nodes edges)
  ((non-empty-setof L2-x?) (setof L2-x?) (setof (pairof L2-x?)) . -> . (values L2-x? (setof L2-x?)))
  (let ([nodelst (alphabetize rem-nodes)])
    (values (first nodelst) (list->set (rest nodelst)))))

;; given a set of valid register associations, choose the best one
;; policy decision
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

;; given an edge with one register, get the register
(define/contract (reg-from-edge edge)
  ((pairof L2-x?) . -> . L2-x?)
  (first (set->list (set-intersect edge all-regs))))

;; given an edge and one of the nodes, get the other node
(define/contract (opposite edge node)
  ((pairof L2-x?) L2-x? . -> . L2-x?)
  (first (set->list (set-remove edge node))))

;;;
;;; L2 -> L1 COMPILATION
;;;

;; compile an L2prog into an L1prog
(define/contract (compile-L2prog prog)
  (L2prog? . -> . L1prog?)
  (type-case L2prog prog
    [l2prog (main others)
            (l1prog (compile-L2fn main)
                    (map compile-L2fn others))]))

;; compile an L2fn into an L1fn
(define/contract (compile-L2fn fn)
  (L2fn? . -> . L1fn?)
  (let* ([base (build-l2reg-base fn)]
         [more (build-l2reg-succ base)]
         [liveness (build-l2reg-liveness more)]
         [graph (build-l2reg-graph liveness)]
         [color (build-l2reg-color graph)]
         [coloring (l2reg-color-coloring color)])
    (l1fn
     (map compile-L2stmt
          (vector->list
           (vector-map
            (λ (stmt) (replace-stmt-vars stmt (λ (x) (hash-ref coloring x x))))
            (l2reg-color-stmts color)))))))

;; compile an L2stmt into an L1stmt
(define/contract (compile-L2stmt stmt)
  (L2stmt? . -> . L1stmt?)
  (type-case L2stmt stmt
      [l2s-assign (lhs rhs) (l1s-assign lhs rhs)]
      [l2s-memget (lhs base offset) (l1s-memget lhs base offset)]
      [l2s-memset (base offset rhs) (l1s-memset base offset rhs)]
      [l2s-aop (lhs op rhs) (l1s-aop lhs op rhs)]
      [l2s-sop (lhs op rhs) (l1s-sop lhs op rhs)]
      [l2s-cmp (lhs c1 op c2) (l1s-cmp lhs c1 op c2)]
      [l2s-label (lbl) (l1s-label lbl)]
      [l2s-goto (lbl) (l1s-goto lbl)]
      [l2s-cjump (c1 op c2 lbl1 lbl2) (l1s-cjump c1 op c2 lbl1 lbl2)]
      [l2s-call (dst) (l1s-call dst)]
      [l2s-tcall (dst) (l1s-tcall dst)]
      [l2s-return () (l1s-return)]
      [l2s-print (lhs arg1) (l1s-print lhs arg1)]
      [l2s-alloc (lhs arg1 arg2) (l1s-alloc lhs arg1 arg2)]
      [l2s-arrayerr (lhs arg1 arg2) (l1s-arrayerr lhs arg1 arg2)]))

;;;
;;; EXTERNAL INTERFACE
;;;

(define/contract (main fn fname)
  (string? string? . -> . void?)
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
  (input-port? . -> . void?)
  (let* ([fn (build-L2fn (read port))]
         [spilled (spill fn (read port) (read port) (read port))]
         [lstform (format-L2fn spilled)])
    (pretty-write lstform)
    (void)))

(define/contract (main/liveness port)
  (input-port? . -> . void?)
  (let* ([fn (build-L2fn (read port))]
         [base (build-l2reg-base fn)]
         [more (build-l2reg-succ base)]
         [liveness (build-l2reg-liveness more)]
         [ins (l2reg-liveness-ins liveness)]
         [outs (l2reg-liveness-outs liveness)]
         [ins-lst (vector->list (vector-map alphabetize ins))]
         [outs-lst (vector->list (vector-map alphabetize outs))])
    (pretty-write `(,(cons 'in ins-lst)
                    ,(cons 'out outs-lst)))
    (void)))

(define/contract (main/graph port)
  (input-port? . -> . void?)
  (let* ([fn (build-L2fn (read port))]
         [base (build-l2reg-base fn)]
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

(define/contract (main/compile port)
  (input-port? . -> . void?)
  (pretty-write
   (format-L1prog
    (compile-L2prog
     (build-L2prog (read port)))))
  (void))
