; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281 unicode)
  (import (scheme base) (scheme case-lambda) (srfi 143)
          (only (srfi 281 base) endianness?)
          (srfi 281 u16))
  (export error-handling-mode?
          unicode-decoding-error?
          string->utf8 string->utf16 string->utf32
          utf8->string utf16->string utf32->string)
  (cond-expand
    ((library (srfi 281 u32))
     (import (srfi 281 u32)))
    (else))
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
     (export (rename bytevector-range-condition-bytevector
                     unicode-decoding-error-bytevector)
             (rename bytevector-range-condition-start
                     unicode-decoding-error-start)
             (rename bytevector-range-condition-end
                     unicode-decoding-error-end)
             (rename condition-message unicode-decoding-error-message)
             unicode-decoding-error?)
     (begin
       (define (unicode-decoding-error? obj)
         (and (error? obj) (unicode-decoding-condition? obj)))
       (define (raise-unicode-decoding-error who message bv start end)
         (condition (make-error)
                    (make-unicode-decoding-condition)
                    (make-bytevector-range-condition bv start end)
                    (make-message-condition message)
                    (make-who-condition who)))))
    (else
     (export unicode-decoding-error-bytevector
             unicode-decoding-error-start
             unicode-decoding-error-end
             unicode-decoding-error-message
             unicode-decoding-error?)
     (begin
       (define key "unicode decoding error")
       (define (unicode-decoding-error? obj)
         (and (error-object? obj)
              (eq? (error-object-message obj) key)))
       (define (unicode-decoding-error-message e)
         (list-ref (error-object-irritants e) 1))
       (define (unicode-decoding-error-bytevector e)
         (list-ref (error-object-irritants e) 2))
       (define (unicode-decoding-error-start e)
         (list-ref (error-object-irritants e) 3))
       (define (unicode-decoding-error-end e)
         (list-ref (error-object-irritants e) 4))
       (define (raise-unicode-decoding-error who message bv start end)
         (error key
                who
                message
                bv
                start
                end)))))
  (include "unicode.scm"))