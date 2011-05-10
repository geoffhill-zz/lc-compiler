#lang plai
(print-only-errors #t)

;;; EECS 322 Aggregate Type Utilities -- Test
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(require (file "utils.rkt"))

(test (alphabetize '()) '())
(test (alphabetize '(t)) '(t))
(test (alphabetize '(z y x)) '(x y z))
(test (alphabetize '(b d e a c f h g)) '(a b c d e f g h))

(test (alphabetize (set)) '())
(test (alphabetize (set 'z)) '(z))
(test (alphabetize (set 'a 'c 'b)) '(a b c))

(test (alphabetize '((z . z) (y . y))) '((y . y) (z . z)))
(test (alphabetize '((a . 5) (c . 3) (b . 1))) '((a . 5) (b . 1) (c . 3)))

(test (alphabetize (set)) '())
(test (alphabetize (set 't)) '(t))
(test (alphabetize (set 'z 'y 'x)) '(x y z))
(test (alphabetize (set 'b 'd 'e 'a 'c 'f 'h 'g)) '(a b c d e f g h))

(test (list->set '()) (set))
(test (list->set '(c a b)) (set 'a 'b 'c))
(test (list->set '(5 4 u 9 (+ 4 5))) (set 4 5 9 'u '(+ 4 5)))

(test (setlst-union '()) (set))
(test (setlst-union `(,(set))) (set))
(test (setlst-union `(,(set 'a 'b 'c) ,(set 'y 'z) ,(set 'm)))
      (set 'a 'b 'c 'm 'y 'z))

(test (set-filter (set 0 1 2 3 4 5 6) even?) (set 0 2 4 6))
(test (set-filter (set 0 1 2 3 4 5 6) odd?) (set 1 3 5))
(test (set-filter (set 'eax 'ebx 'ecx) (λ (x) (symbol=? 'esi x))) (set))
(test (set-filter (set 'edx 'esi 'edi) (λ (x) (symbol=? 'edi x))) (set 'edi))

(test (vs->ll (vector)) '())
(test (vs->ll (vector (set))) (list '()))
(test (vs->ll (vector (set) (set 'a 'b) (set 'd 'c) (set 'z 'x 'y)))
      (list '() '(a b) '(c d) '(x y z)))

(test (hts->list
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

(test (htss->list
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

(test (powerset (set 7)) (set))
(test (powerset (set 9 't)) (set (set 9 't)))
(test (powerset (set 'a 'b 'c)) (set (set 'a 'b) (set 'a 'c) (set 'b 'c)))
(test (set-count (powerset (apply set (build-list 50 values)))) 1225)
(test (set-count (powerset (apply set (build-list 100 values)))) 4950)

(printf "tests completed~n")
