#lang plai

;;; EECS 322 Library of Common Predicates
;;; Geoff Hill <GeoffreyHill2012@u.northwestern.edu>
;;; Spring 2011

(define label-re #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$")

(define label?
  (flat-named-contract
   'label?
   (and/c symbol?
          (flat-contract (λ (s) (regexp-match? label-re (symbol->string s)))))))

(define num?
  (flat-named-contract
   'num?
   integer?))

(define n4?
  (flat-named-contract
   'n4?
   (λ (x) (and (integer? x) (zero? (modulo x 4))))))

(define aop?
  (flat-named-contract
   'aop?
   (symbols '+= '-= '*= '&=)))

(define sop?
  (flat-named-contract
   'sop?
   (symbols '>>= '<<=)))

(define cmp?
  (flat-named-contract
   'cmp?
   (symbols '< '<= '=)))

(define L1-sx?
  (flat-named-contract
   'L1-sx?
   (symbols 'ecx)))

(define L1-cx?
  (flat-named-contract
   'L1-cx?
   (symbols 'eax 'ebx 'ecx 'edx)))

(define L1-x?
  (flat-named-contract
   'L1-x?
   (symbols 'eax 'ebx 'ecx 'edx
            'esi 'edi 'ebp 'esp)))

(define L1-s?
  (flat-named-contract
   'L1-s?
   (or/c num? L1-x? label?)))

(define L2-x?
  (flat-named-contract
   'L2-x?
   (and/c symbol?
          (not/c label?)
          (not/c aop?)
          (not/c sop?)
          (not/c cmp?))))

(define L2-s?
  (flat-named-contract
   'L2-s?
   (or/c num? label? L2-x?)))

(define L3-keyword?
  (flat-named-contract
   'L3-keyword?
   (symbols
    'let 'if
    'new-array 'new-tuple
    'aref 'aset 'alen
    'print 'make-closure
    'closure-proc 'closure-vars)))

(define L3-biop?
  (flat-named-contract
   'L3-biop?
   (symbols '+ '- '* '< '<= '=)))

(define L3-pred?
  (flat-named-contract
   'L3-pred?
   (symbols 'number? 'a?)))

(define L3-x?
  (flat-named-contract
   'L3-x?
   (and/c symbol?
          (not/c label?)
          (not/c L3-keyword?)
          (not/c L3-biop?)
          (not/c L3-pred?))))

(define L3-v?
  (flat-named-contract
   'L3-v?
   (or/c num? label? L3-x?)))

(define L4-keyword?
  (flat-named-contract
   'L4-keyword?
   (symbols 'let 'if 'begin)))

(define L4-x?
  (flat-named-contract
   'L4-x?
   (and/c symbol?
          (not/c label?)
          (not/c L4-keyword?))))

(define L4-v?
  (flat-named-contract
   'L4-v?
   (or/c num? label? L4-x?)))

(define L4-builtin?
  (flat-named-contract
   'L4-builtin?
   (symbols
    '+ '- '* '< '<= '=
    'number? 'a?
    'new-array 'new-tuple
    'aref 'aset 'alen
    'print 'make-closure
    'closure-proc 'closure-vars)))
