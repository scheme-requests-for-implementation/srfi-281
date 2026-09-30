; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

;; TODO: This test suite should be refactored to make it easier to test
;; individual components.

(import (except (scheme base)
                make-bytevector
                bytevector
                bytevector-u8-ref
                bytevector-u8-set!
                string->utf8
                utf8->string)
        (scheme inexact)
        (scheme write)
        (scheme eval)                  ; Optional
        (srfi 1)
        (rename (srfi 64) (test-group %test-group))
        (srfi 158)
        (srfi 194)
        (rename (srfi 252)
                (test-property %test-property))
        (srfi 281))

;;; CHANGE ME!
;;;
;;; If your implementation does not support bixnums, change this to a
;;; smaller number.
(define max-test-integer-width-in-bytes 16)
(define property-test-iterations
  (cond-expand
    (chibi 20)
    (gauche 30)
    (else 100)))
(define-syntax test-property
  (syntax-rules ()
    ((_ prop gens)
     (%test-property prop gens property-test-iterations))))

(cond-expand
  ((or chibi tr7)
   (define-syntax test-group
     (syntax-rules ()
       ((_ name expr ...)
        (begin
          (display name)
          (newline)
          (%test-group name expr ...))))))
  (else
   (define-syntax test-group
     (syntax-rules ()
       ((_ name expr ...)
        (%test-group name expr ...))))))

;;; ;;;;;;;;;;
;;; Generators
;;; ;;;;;;;;;;

(define (make-random-unsigned-integer-generator size)
  (let ((gen-u8 (make-random-u8-generator)))
    (gcons*
     0
     (- (expt 256 size) 1)
     (lambda ()
       (do ((i 0 (+ (* i 256) (gen-u8)))
            (j 0 (+ j 1)))
           ((= j size) i))))))

(define (make-random-signed-integer-generator size)
  (let ((gen-us (make-random-unsigned-integer-generator size))
        (max (- (/ (expt 256 size) 2) 1)))
    (gcons*
     (- (/ (expt 256 size) 2))
     max
     (lambda ()
       (let ((v (gen-us)))
         (if (< max v)
             (- v (expt 256 size))
             v))))))

(define (bytevector-generator-of-length n)
  (lambda ()
    (do ((i 0 (+ i 1))
         (bv (make-bytevector n))
         (u8-generator (make-random-u8-generator)))
        ((= i n) bv)
      (bytevector-u8-set! bv i (u8-generator)))))

(define (make-range-generator generator get-length)
  (lambda ()
    (let* ((obj (generator))
           (len (get-length obj))
           (start ((make-random-integer-generator
                    0
                    (+ len 1))))
           (end ((make-random-integer-generator
                  start
                  (+ len 1)))))
        (vector obj start end))))

(define (make-codepoint-generator)
  (gmap
   (lambda (el)
     (if (< el #xD800)
         (integer->char el)
         (integer->char (+ el (- #xE000 #xD800)))))
   (make-random-integer-generator
    0 (- #x110000 (- #xE000 #xD800)))))

(define (make-unicode-string-generator)
  (gcons*
   ""
   (let ((char-gen (make-codepoint-generator))
         (len-gen (make-random-integer-generator 0 100)))
     (gmap
      (lambda (len)
        (generator->string char-gen len))
      len-gen))))

(define (string+range-generator)
  (make-range-generator (make-unicode-string-generator)
                        string-length))

(define (bytevector+range-generator)
  (make-range-generator (bytevector-generator)
                        bytevector-length))

(define (list+range-generator subgenerator)
  (make-range-generator (list-generator-of subgenerator)
                        length))

(define (endianness-generator)
  (gmap (lambda (boolean)
          (if boolean
              'little
              'big))
        (boolean-generator)))

;;; ;;;;;;;;;;;;;;;
;;; (srfi 281 base)
;;; ;;;;;;;;;;;;;;;
(test-begin "SRFI 281")

(define skip-current-tests? #f)
(test-skip (lambda (x) skip-current-tests?))

(test-group "endianness?"
  (test-assert "big endian" (endianness? 'big))
  (test-assert "little endian" (endianness? 'little))
  (test-assert "native endianness" (endianness? (native-endianness)))
  (test-equal
   "(endianness big)"
   'big
   (eval '(endianness big) (environment '(srfi 281 endianness))))
  (test-equal
   "(endianness little)"
   'little
   (eval '(endianness little) (environment '(srfi 281 endianness)))))

(test-group "make-bytevector"
  (test-equal "empty" #u8() (make-bytevector 0))
  (test-group "length"
    (test-property (lambda (n)
                     (= n (bytevector-length (make-bytevector n))))
                   (list (make-random-unsigned-integer-generator 2))))
  #;(test-group "exhausive check for negative numbers"
      (do ((input -128 (+ input 1))
           (expected 128 (+ expected 1)))
          ((not (negative? input)))
        (test-equal (bytevector expected)
                    (make-bytevector 1 input))))
  #;(test-group "exhausive check for non-negative numbers"
      (do ((input 0 (+ input 1)))
          ((> input 255))
        (test-equal (bytevector input)
                    (make-bytevector 1 input)))))

(test-group "bytevector=?"
  (test-group "equal to self"
    (test-property (lambda (bv)
                     (bytevector=? bv bv))
                   (list (bytevector-generator))))
  (test-assert "truncated left" (not (bytevector=? #u8(1 2 3) #u8(1 2 3 4))))
  (test-assert "truncated right" (not (bytevector=? #u8(1 2 3 4) #u8(1 2 3))))
  (test-assert "multiple equal followed by unequal" (not (bytevector=? #u8() #u8() #u8(1)))))

(test-group "bytevector"
  (test-equal #u8(#x80 #xFF 0 1 #x80)
              (bytevector -128 -1 0 1 128)))

(test-group "bytevector-fill!"
  #;(test-group "exhaustive test of negative values"
      (do ((input -128 (+ input 1))
           (expected 128 (+ expected 1)))
          ((not (negative? input)))
        (test-equal "bytevector-fill!"
                    (bytevector expected expected expected)
                    (let ((bv (make-bytevector 3)))
                      (bytevector-fill! bv input)
                      bv))))
  #;(test-group "exhaustive test of non-negative values"
      (do ((input 0 (+ input 1)))
          ((> input 255))
        (test-equal "bytevector-fill!"
                    (make-bytevector 3 input)
                    (let ((bv (make-bytevector 3)))
                      (bytevector-fill! bv input)
                      bv))))
  (test-group "fill partially"
    (test-property (lambda (n data)
                     (let* ((bv (vector-ref data 0))
                            (start (vector-ref data 1))
                            (end (vector-ref data 2))
                            (cbv (bytevector-copy bv)))
                       (bytevector-fill! cbv n start end)
                       (and (equal? (bytevector-copy bv 0 start)
                                    (bytevector-copy cbv 0 start))
                            (equal? (bytevector-copy cbv end)
                                    (bytevector-copy cbv end))
                            (equal? (make-bytevector (- end start) n)
                                    (bytevector-copy cbv start end)))))
                   (list (make-random-integer-generator -128 256)
                         (bytevector+range-generator)))))

;;; ;;;;;;;;;;;;;;
;;; (srfi 281 u8)
;;; ;;;;;;;;;;;;;;

(test-group "u8-list->bytevector and bytevector->u8-list"
  (test-group "inverses 1"
    (test-property (lambda (list)
                     (equal? list (bytevector->u8-list
                                   (u8-list->bytevector list))))
                   (list (list-generator-of (make-random-u8-generator)))))
  (test-group "inverses 2"
    (test-property (lambda (bv)
                     (equal? bv (u8-list->bytevector
                                 (bytevector->u8-list bv))))
                   (list (bytevector-generator))))
  (test-group "inverses with range 1"
    (test-property (lambda (list+range)
                     (let ((list (vector-ref list+range 0))
                           (start (vector-ref list+range 1))
                           (end (vector-ref list+range 2)))
                       (equal? (take (drop list start) (- end start))
                               (bytevector->u8-list
                                (u8-list->bytevector list start end)))))
                   (list (list+range-generator (make-random-u8-generator)))))
  (test-group "inverses with range 2"
    (test-property (lambda (list+range)
                     (let ((list (vector-ref list+range 0))
                           (start (vector-ref list+range 1))
                           (end (vector-ref list+range 2)))
                       (equal? (take (drop list start) (- end start))
                               (bytevector->u8-list
                                (u8-list->bytevector list)
                                start
                                end))))
                   (list (list+range-generator (make-random-u8-generator)))))
  (test-group "inverses with range 3"
    (test-property (lambda (bv+range)
                     (let ((bv (vector-ref bv+range 0))
                           (start (vector-ref bv+range 1))
                           (end (vector-ref bv+range 2)))
                       (equal? (bytevector-copy bv start end)
                               (u8-list->bytevector
                                (bytevector->u8-list bv start end)))))
                   (list (bytevector+range-generator))))
  (test-group "inverses with range 4"
    (test-property (lambda (bv+range)
                     (let ((bv (vector-ref bv+range 0))
                           (start (vector-ref bv+range 1))
                           (end (vector-ref bv+range 2)))
                       (equal? (bytevector-copy bv start end)
                               (u8-list->bytevector
                                (bytevector->u8-list bv)
                                start
                                end))))
                   (list (bytevector+range-generator)))))

;;; ;;;;;;;;;;;;;;
;;; (srfi 281 s8)
;;; ;;;;;;;;;;;;;;

(test-group "bytevector-s8-ref for all negative integers"
  (do ((input 128 (+ input 1))
       (expected -128 (+ expected 1)))
      ((> input 255))
    (test-equal
     "bytevector-s8-ref"
     expected
     (bytevector-s8-ref (bytevector input) 0))))

(test-group "bytevector-s8-ref for all non-negative integers"
  (do ((input 0 (+ input 1)))
      ((> input 127))
    (test-equal
     "bytevector-s8-ref"
     input
     (bytevector-s8-ref (bytevector input) 0))))

(test-group "bytevector-s8-set! for all negative integers"
  (do ((input -128 (+ input 1)))
      ((not (negative? input)))
    (test-equal
     "bytevector-s8-set!"
     input
     (let ((bv (make-bytevector 1)))
       (bytevector-s8-set! bv 0 input)
       (bytevector-s8-ref bv 0)))))

(test-group "bytevector-s8-set! for all non-negative integers"
  (do ((input 0 (+ input 1)))
      ((> input 127))
    (test-equal
     "bytevector-s8-set!"
     input
     (let ((bv (make-bytevector 1)))
       (bytevector-s8-set! bv 0 input)
       (bytevector-s8-ref bv 0)))))

(test-group "bytevector-s8-ref and bytevector-s8-set! are inverses"
  (test-property
   (lambda (signed-byte)
     (let ((bv (make-bytevector 1)))
       (bytevector-s8-set! bv 0 signed-byte)
       (= (bytevector-s8-ref bv 0) signed-byte)))
   (list (make-random-signed-integer-generator 1))))

;;; ;;;;;;;;;;;;;
;;; (srfi 281 int)
;;; ;;;;;;;;;;;;;

(define (make-width-generator)
  (make-random-integer-generator
   1
   (+ max-test-integer-width-in-bytes)))

(test-group "bytevector-uint-ref endianness"
  (test-equal
   "big endian"
   #x123456781234567812345678
   (bytevector-uint-ref #u8(#x12 #x34 #x56 #x78 #x12 #x34 #x56 #x78 #x12 #x34 #x56 #x78)
                        0
                        'big
                        12))
  (test-equal
   "little endian"
   #x123456781234567812345678
   (bytevector-uint-ref #u8(#x78 #x56 #x34 #x12 #x78 #x56 #x34 #x12 #x78 #x56 #x34 #x12)
                        0
                        'little
                        12)))

(test-group "bytevector-uint-ref and bytevector-uint-set! are inverses"
  (test-property
   (lambda (width endianness)
     (let* ((bv (make-bytevector width))
            (generator (make-random-unsigned-integer-generator width))
            (i (generator)))
       (bytevector-uint-set! bv
                             0
                             i
                             endianness
                             width)
       (= (bytevector-uint-ref bv 0 endianness width)
          i)))
   (list (make-width-generator)
         (endianness-generator))))

(test-group "bytevector-sint-ref endianness"
  (let ((bv (make-bytevector 12)))
    (test-equal
     "big endian"
     #u8(#x12 #x34 #x56 #x78 #x12 #x34 #x56 #x78 #x12 #x34 #x56 #x78)
     (begin
       (bytevector-sint-set! bv
                             0
                             #x123456781234567812345678
                             'big
                             12)
       bv))
    (test-equal
     "little endian"
     #u8(#x78 #x56 #x34 #x12 #x78 #x56 #x34 #x12 #x78 #x56 #x34 #x12)
     (begin
       (bytevector-sint-set! bv
                             0
                             #x123456781234567812345678
                             'little
                             12)
       bv))))

(test-group "bytevector-sint-ref and bytevector-sint-set! are inverses"
  (test-property
   (lambda (width endianness)
     (let* ((bv (make-bytevector width))
            (generator (make-random-signed-integer-generator width))
            (i (generator)))
       (bytevector-sint-set! bv
                             0
                             i
                             endianness
                             width)
       (= (bytevector-sint-ref bv 0 endianness width)
          i)))
   (list (make-random-integer-generator
          1
          (+ max-test-integer-width-in-bytes))
         (endianness-generator))))

(define (make-unsigned-int-list+range-generator width)
  (list+range-generator (make-random-unsigned-integer-generator
                         width)))
(define (make-signed-int-list+range-generator width)
  (list+range-generator (make-random-signed-integer-generator
                         width)))

(define (test size bytevector-> ->bytevector)
  (lambda (list+range endianness)
    (let ((list (vector-ref list+range 0))
          (start (vector-ref list+range 1))
          (end (vector-ref list+range 2)))
      (equal? (take (drop list start) (- end start))
              (bytevector-> (->bytevector list
                                          endianness
                                          size
                                          start
                                          end)
                            endianness
                            size)))))

(test-group "bytevector->uint-list and uint-list->bytevector are inverses"
  (do ((i 1 (+ i 1)))
      ((= i max-test-integer-width-in-bytes))
    (test-property
     (test i bytevector->uint-list uint-list->bytevector)
     (list (make-unsigned-int-list+range-generator i)
           (endianness-generator)))))

(test-group "bytevector->uint-list and uint-list->bytevector are inverses"
  (do ((i 1 (+ i 1)))
      ((= i max-test-integer-width-in-bytes))
    (test-property
     (test i bytevector->sint-list sint-list->bytevector)
     (list (make-signed-int-list+range-generator i)
           (endianness-generator)))))

(test-group "r6rs examples for list procedures"
  (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
    (test-equal
     '(513 -253 513 513)
     (bytevector->sint-list b (endianness little) 2)))
  (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
    (test-equal
     '(513 65283 513 513)
     (bytevector->uint-list b (endianness little) 2))))

;;; ;;;;;;;;;;;;;;
;;; (srfi 281 u16)
;;; ;;;;;;;;;;;;;;

(define (u16:exhaustive-test endianness)
  (do ((i 0 (+ i 1)))
      ((> i #xFFFF))
    (test-equal
     "bytevector-u16-ref"
     i
     (let ((bv (make-bytevector 2)))
       (bytevector-u16-set! bv 0 i endianness)
       (bytevector-u16-ref bv 0 endianness)))))

#;(test-group "exhaustive test for u16, big endian"
  (u16:exhaustive-test 'big))
#;(test-group "exhaustive test for u16, little endian"
  (u16:exhaustive-test 'little))

;;; ;;;;;;;;;;;;;;
;;; (srfi 281 s16)
;;; ;;;;;;;;;;;;;;

(define (s16:exhaustive-test endianness)
  (do ((i #x-8000 (+ i 1)))
      ((>= i #x8000))
    (test-equal
     "bytevector-s16-ref"
     i
     (let ((bv (make-bytevector 2)))
       (bytevector-s16-set! bv 0 i endianness)
       (bytevector-s16-ref bv 0 endianness)))))

#;(test-group "exhaustive test for s16, big endian"
  (s16:exhaustive-test 'big))
#;(test-group "exhaustive test for s16, little endian"
  (s16:exhaustive-test 'little))

;;; ;;;;;;;;;;;;;;;;;
;;; (srfi 281 u32)
;;; ;;;;;;;;;;;;;;;;;

(test-group "u32"
  (test-property
   (lambda (endianness word len)
     (let* ((bv (make-bytevector (* len 4) 0))
            (idx ((make-random-integer-generator 0 len))))
       (bytevector-u32-set! bv idx word endianness)
       (= (bytevector-u32-ref bv idx endianness)
          word)))
   (list (endianness-generator)
         (make-random-unsigned-integer-generator 4)
         (make-random-integer-generator 1 100))))

(test-group "s32"
  (test-property
   (lambda (endianness word len)
     (let* ((bv (make-bytevector (* len 4) 0))
            (idx ((make-random-integer-generator 0 len))))
       (bytevector-s32-set! bv idx word endianness)
       (= (bytevector-s32-ref bv idx endianness)
          word)))
   (list (endianness-generator)
         (make-random-signed-integer-generator 4)
         (make-random-integer-generator 1 100))))

;;; ;;;;;;;;;;;;;;;;;
;;; (srfi 281 x64)
;;; ;;;;;;;;;;;;;;;;;

(test-group "u64"
  (test-property
   (lambda (endianness word len)
     (let* ((bv (make-bytevector (* len 8) 0))
            (idx ((make-random-integer-generator 0 len))))
       (bytevector-u64-set! bv idx word endianness)
       (= (bytevector-u64-ref bv idx endianness)
          word)))
   (list (endianness-generator)
         (make-random-unsigned-integer-generator 8)
         (make-random-integer-generator 1 100))))

(test-group "s64"
  (test-property
   (lambda (endianness word len)
     (let* ((bv (make-bytevector (* len 8) 0))
            (idx ((make-random-integer-generator 0 len))))
       (bytevector-s64-set! bv idx word endianness)
       (= (bytevector-s64-ref bv idx endianness)
          word)))
   (list (endianness-generator)
         (make-random-signed-integer-generator 8)
         (make-random-integer-generator 1 100))))

(test-group "bytevector->hex-string"
  (test-equal "AABBCCDD"
              (bytevector->hex-string #u8(#xAA #xBB #xCC #xDD)))
  (test-equal "BBCCDD"
              (bytevector->hex-string #u8(#xAA #xBB #xCC #xDD)
                                      1))
  (test-equal "BBCC"
              (bytevector->hex-string #u8(#xAA #xBB #xCC #xDD)
                                      1
                                      3)))

(test-group "hex-string->bytevector"
  (test-equal #u8(#xAA #xBB #xCC #xDD)
              (hex-string->bytevector "AaBBcCdd"))
  (let ((s "AAB!CCDD"))
    (guard (x (else (and (deserialization-error? x)
                         (eqv? (deserialization-error-meesage x) s))))
      (hex-string->bytevector s)))
  (test-equal #u8(#xBB #xCC #xDD)
              (hex-string->bytevector "AaBBcCdd"
                                      2))
  (test-equal #u8(#xBB #xCC)
              (hex-string->bytevector "AaBBcCdd"
                                      2
                                      6)))

(test-group "hex-string->bytevector and bytevector->hex-string are inverses, 1"
  (test-property
   (lambda (bv+range)
     (let ((bv (vector-ref bv+range 0))
           (start (vector-ref bv+range 1))
           (end (vector-ref bv+range 2)))
       (equal? (hex-string->bytevector (bytevector->hex-string
                                        bv
                                        start
                                        end))
               (bytevector-copy bv start end))))
   (list (bytevector+range-generator))))

(test-group "hex-string->bytevector and bytevector->hex-string are inverses, 1"
  (test-property
   (lambda (bv+range)
     (let ((bv (vector-ref bv+range 0))
           (start (vector-ref bv+range 1))
           (end (vector-ref bv+range 2)))
       (equal? (hex-string->bytevector
                (bytevector->hex-string bv)
                (* start 2)
                (* end 2))
               (bytevector-copy bv start end))))
   (list (bytevector+range-generator))))

#| Some of this test data comes from RFC 4648.

   Copyright (c) 2000-2006 Simon Josefsson

   Regarding the abstract and sections 1, 3, 8, 10, 12, 13, and 14 of
   this document, that were written by Simon Josefsson ("the author",
   for the remainder of this section), the author makes no guarantees
   and is not responsible for any damage resulting from its use.  The
   author grants irrevocable permission to anyone to use, modify, and
   distribute it in any way that does not diminish the rights of anyone
   else to use, modify, and distribute it, provided that redistributed
   derivative works do not contain misleading author or version
   information and do not falsely purport to be IETF RFC documents.
   Derivative works need not be licensed under similar terms.
|#

;;; TODO: Test begin, end, and URL-safe variants
(test-group "bytevector->base64"
  (test-equal "" (bytevector->base64 #u8()))
  (test-equal "Zg==" (bytevector->base64 #u8(102)))
  (test-equal "Zm8=" (bytevector->base64 #u8(102 111)))
  (test-equal "Zm9v" (bytevector->base64 #u8(102 111 111)))
  (test-equal "Zm9vYg==" (bytevector->base64 #u8(102 111 111 98)))
  (test-equal "Zm9vYmE=" (bytevector->base64 #u8(102 111 111 98 97)))
  (test-equal "Zm9vYmFy" (bytevector->base64 #u8(102 111 111 98 97 114)))
  (test-equal "+/++" (bytevector->base64
                      #u8(#b11111011 #b11111111 #b10111110)))
  (test-equal "-_--" (bytevector->base64
                      #u8(#b11111011 #b11111111 #b10111110)
                      "-_")))

(test-group "base64->bytevector"
  (test-equal #u8() (base64->bytevector ""))
  (test-equal #u8(102) (base64->bytevector "Zg=="))
  (test-equal #u8(102 111) (base64->bytevector "Zm8="))
  (test-equal #u8(102 111 111) (base64->bytevector "Zm9v"))
  (let ((s "Zm9!v"))
    (guard (x (else (and (deserialization-error? x)
                         (eqv? (deserialization-error-meesage x) s))))
      (base64->bytevector s)))
  (test-equal #u8(102 111 111 98) (base64->bytevector "Zm9vYg=="))
  (test-equal #u8(102 111 111 98 97) (base64->bytevector "Zm9vYmE="))
  (test-equal #u8(102 111 111 98 97 114) (base64->bytevector "Zm9vYmFy"))
  (test-equal #u8(102 111 111 98 97 114) (base64->bytevector "    Z m 9 v\n Y m F y    "))
  (test-equal #u8(#b11111011 #b11111111 #b10111110)
              (base64->bytevector "+/++"))
  (test-equal #u8(#b11111011 #b11111111 #b10111110)
              (base64->bytevector "-_--" "-_")))

(test-group "bytevector->base64 and base64->bytevector are inverses, 1"
  (test-property
   (lambda (bv+range)
     (let ((bv (vector-ref bv+range 0))
           (start (vector-ref bv+range 1))
           (end (vector-ref bv+range 2)))
       (equal? (base64->bytevector (bytevector->base64
                                    bv
                                    #f
                                    start
                                    end))
               (bytevector-copy bv start end))))
   (list (bytevector+range-generator))))

(test-group "base64->bytevector with start and end"
  (test-property
   (lambda (bv s1 s2)
     (let* ((base64 (bytevector->base64 bv))
            (padded (string-append s1
                                   base64
                                   s2)))
       (equal? (base64->bytevector padded
                                   #f
                                   (string-length s1)
                                   (- (string-length padded)
                                      (string-length s2)))
               bv)))
   (list (bytevector-generator)
         (string-generator)
         (string-generator))))

;;; ;;;;;;;;;;;;;;
;;; (srfi 281 f32)
;;; ;;;;;;;;;;;;;;

(define (bytevector-reverse bv)
  (do ((new (make-bytevector (bytevector-length bv)))
       (i 0 (+ i 1)))
      ((= i (bytevector-length bv)) new)
    (bytevector-u8-set! new i (bytevector-u8-ref bv
                                                 (- (bytevector-length bv)
                                                    i
                                                    1)))))
(test-group "binary32-set! and binary32-ref"
  (letrec-syntax ((test-endian
                   (syntax-rules ()
                     ((_ name expected input endianness)
                      (begin
                        (test-equal
                         name
                         expected
                         (let ((bv (make-bytevector 4)))
                           (bytevector-binary32-set! bv
                                                     0
                                                     input
                                                     endianness)
                           bv))
                        (test-assert
                         name
                         (= input
                            (bytevector-binary32-ref expected
                                                     0
                                                     endianness)))))))
                  (test
                   (syntax-rules ()
                     ((_ name expected input)
                      (begin
                        (test-endian name expected input 'big)
                        (test-endian name
                                     (bytevector-reverse expected)
                                     input
                                     'little))))))
    (test "1" #u8(#x3F #x80 #x00 #x00) 1)
    (test "1.5" #u8(#x3F #xC0 #x00 #x00) 1.5)
    (test "-1" #u8(#xBF #x80 #x00 #x00) -1)
    (test "0.5" #u8(#x3f #x00 #x00 #x00) 0.5)
    (test "-0.5" #u8(#xBf #x00 #x00 #x00) -0.5)
    (test "0" #u8(#x00 #x00 #x00 #x00) 0.0)
    (test "+inf.0" #u8(#x7F #x80 #x00 #x00) +inf.0)
    (test "-inf.0" #u8(#xFF #x80 #x00 #x00) -inf.0))
  (test-assert "NaN round tripping"
               (let ((bv (make-bytevector 4)))
                 (bytevector-binary32-native-set! bv
                                                  0
                                                  +nan.0)
                 (nan? (bytevector-binary32-native-ref bv 0)))))

;;; Only test this when floats are binary32 or more precise

(test-group "idempotency of binary32-native-set! and binary32-native-ref"
  (test-property
   (lambda (x)
     (let ((bv (make-bytevector 4))
           (bv2 (make-bytevector 4)))
       (bytevector-binary32-native-set! bv 0 x)
       (bytevector-binary32-native-set!
        bv2
        0
        (bytevector-binary32-native-ref bv 0))
       (equal? bv bv2)))
   (list (inexact-real-generator))))

;;; Only test this when floats are binary32 or more precise
(test-group "binary32 subnormals"
  (letrec-syntax ((test
                   (syntax-rules ()
                     ((_ expected input)
                      (begin
                        (test-approximate
                         expected
                         (bytevector-binary32-ref input 0 'big)
                         0))))))
    (test 1.40129846432481707092e-45
          #u8(0 0 0 1))
    (test 2.80259692864963414185e-45
          #u8(0 0 0 2))
    (test 4.20389539297445121277e-45
          #u8(0 0 0 3))
    (test 1.17549421069244107549e-38
          #u8(0 #x7F #xFF #xFF))))

;;; ;;;;;;;;;;;;;;
;;; (srfi 281 f64)
;;; ;;;;;;;;;;;;;;

(define (bytevector-reverse bv)
  (do ((new (make-bytevector (bytevector-length bv)))
       (i 0 (+ i 1)))
      ((= i (bytevector-length bv)) new)
    (bytevector-u8-set! new i (bytevector-u8-ref bv
                                                 (- (bytevector-length bv)
                                                    i
                                                    1)))))

(test-group "binary64-set! and binary64-ref"
  (letrec-syntax ((test-endian
                   (syntax-rules ()
                     ((_ name expected input endianness)
                      (begin
                        (test-equal
                         name
                         expected
                         (let ((bv (make-bytevector 8)))
                           (bytevector-binary64-set! bv
                                                     0
                                                     input
                                                     endianness)
                           bv))
                        (test-assert
                         name
                         (= input
                            (bytevector-binary64-ref expected
                                                     0
                                                     endianness)))))))
                  (test
                   (syntax-rules ()
                     ((_ name expected input)
                      (begin
                        (test-endian name expected input 'big)
                        (test-endian name
                                     (bytevector-reverse expected)
                                     input
                                     'little))))))
    (test "1" #u8(#x3F #xF0 #x00 #x00 0 0 0 0) 1.0)
    (test "1.5" #u8(#x3F #xF8 #x00 #x00 0 0 0 0) 1.5)
    (test "-1" #u8(#xBF #xF0 #x00 #x00 0 0 0 0) -1)
    (test "0.5" #u8(#x3f #xE0 #x00 #x00 0 0 0 0) 0.5)
    (test "-0.5" #u8(#xBf #xE0 #x00 #x00 0 0 0 0) -0.5)
    (test "0" #u8(#x00 #x00 #x00 #x00 0 0 0 0) 0.0)
    (test "+inf.0" #u8(#x7F #xF0 #x00 #x00 0 0 0 0) +inf.0)
    (test "-inf.0" #u8(#xFF #xF0 #x00 #x00 0 0 0 0) -inf.0))
  (test-assert "NaN round tripping"
               (let ((bv (make-bytevector 8)))
                 (bytevector-binary64-native-set! bv
                                                  0
                                                  +nan.0)
                 (nan? (bytevector-binary64-native-ref bv 0)))))

;;; Only test this when floats are binary32 or more precise
(test-group "binary64-set! and binary64-ref are inverses"
  (test-property
   (lambda (x)
     (let ((bv (make-bytevector 8)))
       (bytevector-binary64-native-set! bv 0 x)
       (cond
         ((nan? x)
          (nan? (bytevector-binary64-native-ref bv 0)))
         (else
          (= x (bytevector-binary64-native-ref bv 0))))))
   (list (inexact-real-generator))))

(test-group "binary64 subnormals"
  (letrec-syntax ((test
                   (syntax-rules ()
                     ((_ expected input)
                      (begin
                        (test-approximate
                         expected
                         (bytevector-binary64-ref input 0 'big)
                         0))))))
    (test 4.94065645841246544177e-324
          #u8(0 0 0 0 0 0 0 1))
    (test 9.88131291682493088353e-324
          #u8(0 0 0 0 0 0 0 2))
    (test 1.48219693752373963253e-323
          #u8(0 0 0 0 0 0 0 3))
    (test 2.22507385850720088902e-308
          #u8(0 #x0F #xFF #xFF #xFF #xFF #xFF #xFF))))

;;; ;;;;;;;;;;
;;; (srfi 281 unicode)
;;; ;;;;;;;;;;

(test-group "error-handling-mode?"
  (test-assert (error-handling-mode? 'raise))
  (test-assert (error-handling-mode? 'ignore))
  (test-assert (error-handling-mode? 'replace)))

(test-group "error-handling-mode"
  (let ((test
         (lambda (sym)
           (test-equal
            sym
            (eval `(error-handling-mode ,sym)
                  (environment '(srfi 281 error-handling-mode)))))))
    (test 'raise)
    (test 'replace)
    (test 'ignore)))

;;; This test is preferred, but may take very long.
#;(test-group "exhaustive check for round-tripping of UTF-8 for scalar values"
  (do ((i 0 (+ i 1)))
      ((>= i #xD800))
    (let* ((s (string (integer->char i)))
           (n (number->string i 16)))
      (test-assert n (equal? (utf8->string (string->utf8 s))
                             s))))
  (do ((i #xE000 (+ i 1)))
      ((> i #x10FFFF))
    (let ((s (string (integer->char i)))
          (n (number->string i 16)))
      (test-assert n (equal? (utf8->string (string->utf8 s))
                             s)))))

(test-group "extreme values for UTF-8 values"
  (let-syntax ((test2
                (syntax-rules ()
                  ((_ bv s)
                   (begin
                     (test-equal bv (string->utf8 s))
                     (test-equal s (utf8->string bv)))))))
    (test2 #u8(0) "\x0;")
    (test2 #u8(#x7F) "\x7F;")
    (test2 #u8(#xC2 #x80) "\x80;")
    (test2 #u8(#xDF #xBF) "\x7FF;")
    (test2 #u8(#xE0 #xA0 #x80) "\x800;")
    (test2 #u8(#xED #x9F #xBF) "\xD7FF;")
    (test2 #u8(#xEE #x80 #x80) "\xE000;")
    (test2 #u8(#xEF #xBF #xBF) "\xFFFF;")
    (test2 #u8(#xF0 #x90 #x80 #x80) "\x10000;")
    (test2 #u8(#xF4 #x8F #xBF #xBF) "\x10FFFF;")))

(test-group "Truncation examples"
  (let-syntax ((test
                (syntax-rules ()
                  ((_ expected input)
                   (let ((b input))
                     (test-equal expected
                                 (utf8->string
                                  b
                                  0
                                  (bytevector-length b)
                                  'replace)))))))
    (test "\xFFFD;" #u8(#xE0 #xA0))
    (test "\xFFFD;\xFFFD;" #u8(#xC0 #x80))
    (test "\xFFFD;\xFFFD;" #u8(#xC1 #xBF))
    (test "\xFFFD;\xFFFD;" #u8(#xFE #xFF))
    (test "\xFFFD;\x20;" #u8(#xF0 #x90 #x80 #x20))))

(test-group "string->utf8 and utf8->string are inverses"
  (test-property
   (lambda (string+range)
     (let ((string (vector-ref string+range 0))
           (start (vector-ref string+range 1))
           (end (vector-ref string+range 2)))
       (equal? (utf8->string (string->utf8 string
                                           start
                                           end))
               (substring string start end))))
   (list (string+range-generator))))

;; TODO: Test elements of error object
(define-syntax test-raises-unicode-error
  (syntax-rules ()
    ((_ expression)
     (test-assert
      (guard (x (else (unicode-decoding-error? x)))
        expression
        #f)))))

#;(test-group "exhaustive check for two-byte overlong encodings"
  (do ((i #xC0 (+ i 1)))
      ((= i #xC2))
    (test-raises-unicode-error
     (utf8->string (bytevector i)))
    (test-equal (string (integer->char #xFFFD))
                (utf8->string (bytevector i)
                              0
                              1
                              'replace))
    (test-equal ""
                (utf8->string (bytevector i)
                              0
                              1
                              'ignore))
    (do ((j #x80 (+ j 1)))
        ((> j #xBF))
      (test-raises-unicode-error
       (utf8->string (bytevector i j)))
      (test-equal (string (integer->char #xFFFD)
                          (integer->char #xFFFD))
                  (utf8->string (bytevector i j)
                                0
                                2
                                'replace))
      (test-equal ""
                  (utf8->string (bytevector i j) 0 2 'ignore)))))

#;(test-group "exhaustive check for three byte overlong encoding"
  (do ((i #x80 (+ i 1)))
      ((= i #xA0))
    (test-raises-unicode-error
     (utf8->string (bytevector #xE0 i)))
    (test-equal (string (integer->char #xFFFD)
                        (integer->char #xFFFD))
                (utf8->string (bytevector #xE0 i)
                              0
                              2
                              'replace))
    (test-equal ""
                (utf8->string (bytevector #xE0 i)
                              0
                              1
                              'ignore))))

(test-group "check for three byte truncated encoding"
  (do ((i #xA0 (+ i 1)))
      ((> i #xBF))
    (test-raises-unicode-error
     (utf8->string (bytevector #xE0 i)))
    (test-equal (string (integer->char #xFFFD))
                (utf8->string (bytevector #xE0 i)
                              0
                              2
                              'replace))
    (test-equal ""
                (utf8->string (bytevector #xE0 i)
                              0
                              1
                              'ignore))))

#;(test-group "exhaustive check of surrogate encodings"
  (do ((i #xA0 (+ i 1)))
      ((> i #xBF))
    (do ((j #x80 (+ j 1)))
        ((> j #xBF))
      (test-raises-unicode-error
       (utf8->string (bytevector #xED i j)))
      (test-equal (string (integer->char #xFFFD)
                          (integer->char #xFFFD)
                          (integer->char #xFFFD))
                  (utf8->string (bytevector #xED i j)
                                0
                                3
                                'replace))
      (test-equal ""
                  (utf8->string (bytevector #xED i j)
                                0
                                3
                                'ignore)))))

(test-group "encoding above U+10FFFF"
  (test-raises-unicode-error
   (utf8->string #u8(#xF4 #x90 #x80 #x80)))
  (test-equal (string (integer->char #xFFFD)
                      (integer->char #xFFFD)
                      (integer->char #xFFFD)
                      (integer->char #xFFFD))
              (utf8->string #u8(#xF4 #x90 #x80 #x80)
                            0
                            4
                            'replace))
  (test-equal ""
              (utf8->string #u8(#xF4 #x90 #x80 #x80)
                            0
                            4
                            'ignore)))

;;; ;;;;;;
;;; UTF-16
;;; ;;;;;;

#;(test-group "exhaustive check for round-tripping of UTF-16 for scalar values"
  (for-each
   (lambda (endianness)
     (do ((i 0 (+ i 1)))
         ((>= i #xD800))
       (let* ((s (string (integer->char i)))
              (n (number->string i 16)))
         (test-assert n (equal? (utf16->string (string->utf16 s endianness)
                                               endianness
                                               #t)
                                s))))
     (do ((i #xE000 (+ i 1)))
         ((> i #x10FFFF))
       (when (zero? (modulo i #x1000))
         (display (number->string i 16))
         (newline))
       (let ((s (string (integer->char i)))
             (n (number->string i 16)))
         (test-assert n (equal? (utf16->string (string->utf16 s
                                                              endianness)
                                               endianness
                                               #t)
                                s)))))
   '(big little)))

(test-group "UTF-16 behavior on lone surrogates"
  (do ((i #xD800 (+ i 1)))
      ((= i #xDFFF))
    (let ((bv (make-bytevector 2)))
      (bytevector-u16-set! bv 0 i 'big)
      (test-raises-unicode-error (utf16->string bv 'big))
      (test-equal "\xFFFD;"
                  (utf16->string bv 'big #f 0 2 'replace))
      (test-equal "" (utf16->string bv 'big #f 0 2 'ignore))
      (bytevector-u16-set! bv 0 i 'little)
      (test-raises-unicode-error (utf16->string bv 'little))
      (test-equal "\xFFFD;"
                  (utf16->string bv 'little #f 0 2 'replace))
      (test-equal ""
                  (utf16->string bv 'little #f 0 2 'ignore)))))

(test-group "string->utf16 and utf16->string are inverses"
  (test-property
   (lambda (string+range endianness)
     (let* ((string (vector-ref string+range 0))
            (start (vector-ref string+range 1))
            (end (vector-ref string+range 2))
            (string-out
             (utf16->string (string->utf16 string
                                           endianness
                                           start
                                           end)
                            endianness
                            #t)))
       (equal? string-out (substring string start end))))
   (list (string+range-generator)
         (endianness-generator))))

(test-group "utf16: BOM"
  (test-equal "\x20;"
              (utf16->string #u8(#xFE #xFF #x00 #x20) 'little))
  (test-equal "\x20;"
              (utf16->string #u8(#xFF #xFE #x20 #x00) 'big))
  (test-equal "\xFEFF;\x0020;" (utf16->string #u8(#xFE #xFF #x00 #x20)
                                              'big
                                              #t))
  (test-equal "\xFFFE;\x2000;" (utf16->string #u8(#xFE #xFF #x00 #x20)
                                              'little
                                              #t)))

;;; ;;;;;;;;
;;; UTF-32
;;; ;;;;;;;;

#;(test-group "exhaustive check for round-tripping of UTF-32 for scalar values"
    (for-each
     (lambda (endianness)
       (do ((i 0 (+ i 1)))
           ((>= i #xD800))
         (let* ((s (string (integer->char i)))
                (n (number->string i 16)))
           (test-assert n (equal? (utf32->string (string->utf32 s endianness)
                                                 endianness
                                                 #t)
                                  s))))
       (do ((i #xE000 (+ i 1)))
           ((> i #x10FFFF))
         (when (zero? (modulo i #x1000))
           (display (number->string i 16))
           (newline))
         (let ((s (string (integer->char i)))
               (n (number->string i 16)))
           (test-assert n (equal? (utf32->string (string->utf32 s
                                                                endianness)
                                                 endianness
                                                 #t)
                                  s)))))
     '(big little)))

(test-group "string->utf32 and utf32->string are inverses"
  (test-property
   (lambda (string+range endianness)
     (let ((string (vector-ref string+range 0))
           (start (vector-ref string+range 1))
           (end (vector-ref string+range 2)))
       (equal? (utf32->string (string->utf32 string
                                             endianness
                                             start
                                             end)
                              endianness
                              #t)
               (substring string start end))))
   (list (string+range-generator)
         (endianness-generator))))

(test-group "utf32: BOM"
  (test-equal "\x20;"
              (utf32->string #u8(#x00 #x00 #xFE #xFF #x00 #x00 #x00 #x20) 'little))
  (test-equal "\x20;"
              (utf32->string #u8(#xFF #xFE #x00 #x00 #x20 #x00 #x00 #x00) 'big))
  (test-equal "\xFEFF;\x0020;" (utf32->string #u8(#x00 #x00 #xFE #xFF #x00 #x00 #x00 #x20)
                                              'big
                                              #t))
  (test-equal "\xFFFE;\x0020;" (utf32->string #u8(#xFE #xFF #x00 #x00 #x20 #x00 #x00 #x00)
                                            'little
                                            #t)))

(test-end "SRFI 281")
