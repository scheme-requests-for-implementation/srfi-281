; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281 serialization)
  (import (scheme base) (scheme case-lambda) (scheme char))
  (export bytevector->hex-string
          hex-string->bytevector
          bytevector->base64
          base64->bytevector)
  (cond-expand
    (chicken
     (import (rename (chicken fixnum)
                     (fxshl fxarithmetic-shift-left)
                     (fxshr fxarithmetic-shift-right))))
    ((library (srfi 143))
     (import (srfi 143))))
  (cond-expand
    ((library (rnrs conditions))
     (import (only (rnrs conditions)
                   condition
                   error?
                   make-error
                   make-message-condition
                   make-who-condition
                   make-irritants-condition)
             (srfi 281 condition))
     (export (rename string-range-condition-string
                     deserialization-error-string)
             (rename string-range-condition-start
                     deserialization-error-start)
             (rename string-range-condition-end
                     deserialization-error-end)
             (rename condition-message deserialization-error-message)
             deserialization-error?)
     (begin
       (define (deserialization-error? obj)
         (and (error? obj) (deserialization-condition? obj)))
       (define (raise-deserialization-error who message bv start end)
         (condition (make-error)
                    (make-deserialization-condition)
                    (make-string-range-condition bv start end)
                    (make-message-condition message)
                    (make-who-condition who)))))
    (else
     (export deserialization-error-bytevector
             deserialization-error-start
             deserialization-error-end
             deserialization-error-message
             deserialization-error?)
     (begin
       (define key "deserialization error")
       (define (deserialization-error? obj)
         (and (error-object? obj)
              (eq? (error-object-message obj) key)))
       (define (deserialization-error-message e)
         (list-ref (error-object-irritants e) 1))
       (define (deserialization-error-string e)
         (list-ref (error-object-irritants e) 2))
       (define (deserialization-error-start e)
         (list-ref (error-object-irritants e) 3))
       (define (deserialization-error-end e)
         (list-ref (error-object-irritants e) 4))
       (define (raise-deserialization-error who message s start end)
         (error key who message s start end)))))
  (include "serialization.scm"))