#lang plai

;;; EECS 322 Aggregate Type Utilities -- Test
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "aggr-utils.rkt"))
(require rackunit)

(define/contract (test)
  (-> void?)
  
  (check-equal? (alphabetize '()) '())
  (check-equal? (alphabetize '(t)) '(t))
  (check-equal? (alphabetize '(z y x)) '(x y z))
  (check-equal? (alphabetize '(b d e a c f h g)) '(a b c d e f g h))
  
  (check-equal? (alphabetize (set)) '())
  (check-equal? (alphabetize (set 'z)) '(z))
  (check-equal? (alphabetize (set 'a 'c 'b)) '(a b c))
  
  (check-equal? (alphabetize '((z . z) (y . y))) '((y . y) (z . z)))
  (check-equal? (alphabetize '((a . 5) (c . 3) (b . 1))) '((a . 5) (b . 1) (c . 3)))
  
  (check-equal? (alphabetize (set)) '())
  (check-equal? (alphabetize (set 't)) '(t))
  (check-equal? (alphabetize (set 'z 'y 'x)) '(x y z))
  (check-equal? (alphabetize (set 'b 'd 'e 'a 'c 'f 'h 'g)) '(a b c d e f g h))
  
  (check-equal? (list->set '()) (set))
  (check-equal? (list->set '(c a b)) (set 'a 'b 'c))
  (check-equal? (list->set '(5 4 u 9 (+ 4 5))) (set 4 5 9 'u '(+ 4 5)))
  
  (check-equal? (setlst-union '()) (set))
  (check-equal? (setlst-union `(,(set))) (set))
  (check-equal? (setlst-union `(,(set 'a 'b 'c) ,(set 'y 'z) ,(set 'm)))
                (set 'a 'b 'c 'm 'y 'z))
  
  (check-equal? (set-filter (set 0 1 2 3 4 5 6) even?) (set 0 2 4 6))
  (check-equal? (set-filter (set 0 1 2 3 4 5 6) odd?) (set 1 3 5))
  (check-equal? (set-filter (set 'eax 'ebx 'ecx) (λ (x) (symbol=? 'esi x))) (set))
  (check-equal? (set-filter (set 'edx 'esi 'edi) (λ (x) (symbol=? 'edi x))) (set 'edi))
  
  (check-equal? (vs->ll (vector)) '())
  (check-equal? (vs->ll (vector (set))) (list '()))
  (check-equal? (vs->ll (vector (set) (set 'a 'b) (set 'd 'c) (set 'z 'x 'y)))
                (list '() '(a b) '(c d) '(x y z)))
  
  (check-equal? (hts->list
                 (make-hash
                  `((b . eax)
                    (ecx . ecx)
                    (f . edi)
                    (a . edx)
                    (eax . eax)
                    (esi . esi)
                    (edi . edi))))
                `((a edx)
                  (b eax)
                  (eax eax)
                  (ecx ecx)
                  (edi edi)
                  (esi esi)
                  (f edi)))
  
  (check-equal? (htss->list
                 (make-hash
                  `((b . ,(set 'eax 'esi 'ecx 'edi))
                    (ecx . ,(set 'a 'b))
                    (a . ,(set 'ecx 'edi 'edx))
                    (eax . ,(set 'b 'esi 'edi))
                    (esi . ,(set 'eax 'b)))))
                `((a ecx edi edx)
                  (b eax ecx edi esi)
                  (eax b edi esi)
                  (ecx a b)
                  (esi b eax)))
  
  (check-equal? (powerset (set 7)) (set))
  (check-equal? (powerset (set 9 't)) (set (set 9 't)))
  (check-equal? (powerset (set 'a 'b 'c)) (set (set 'a 'b) (set 'a 'c) (set 'b 'c)))
  (check-equal? (set-count (powerset (apply set (build-list 50 values)))) 1225)
  (check-equal? (set-count (powerset (apply set (build-list 100 values)))) 4950)
  
  (printf "tests completed~n")
  (void))