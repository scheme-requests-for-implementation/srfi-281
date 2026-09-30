; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281 base)
  (import (rename (scheme base)
                  (make-bytevector r7rs:make-bytevector)
                  (bytevector r7rs:bytevector))
          (srfi 143)
          (scheme case-lambda))
  (export endianness? native-endianness
          make-bytevector bytevector
          bytevector=?
          bytevector-fill!
          bytevector? bytevector-length bytevector-copy)
  (cond-expand
    (little-endian
     (begin (define (native-endianness) 'little)))
    (big-endian
     (begin (define (native-endianness) 'big)))
    (else (begin (error "I don't know the native endianness"))))
  (include-library-declarations "internal.scm")
  (include "base.scm"))
