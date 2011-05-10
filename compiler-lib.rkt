#lang plai

;;; EECS 322 Library of Common Functions
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
   'L2-x
   (and/c symbol?
          (not/c label?)
          (not/c aop?)
          (not/c sop?)
          (not/c cmp?))))

(define L2-s?
  (flat-named-contract
   'L2-s
   (or/c num? label? L2-x?)))
