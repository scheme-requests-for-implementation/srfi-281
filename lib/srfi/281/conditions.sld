(define-library (srfi 281 condition)
  (export &deserialization-condition
          make-deserialization-condition
          deserialization-condition?
          &unicode-decoding-condition
          make-unicode-decoding-condition
          unicode-decoding-condition?
          &string-range-condition
          make-string-range-condition
          string-range-condition?
          string-range-condition-start
          string-range-condition-end
          string-range-condition-string
          &bytevector-range-condition
          bytevector-range-condition?
          bytevector-range-condition-start
          bytevector-range-condition-end
          bytevector-range-condition-bytevector)
  (cond-expand
    ((library (rnrs conditions))
     (import (only (rnrs conditions)
                   define-condition-type))
     (begin
       (define-condition-type &deserialization-condition &condition
         make-deserialization-condition
         deserialization-condition?)
       (define-condition-type &unicode-decoding-condition &condition
         make-unicode-decoding-condition
         unicode-decoding-condition?)
       (define-condition-type &string-range-condition &condition
         make-string-range-condition
         string-range-condition?
         (start string-range-condition-start)
         (end string-range-condition-end)
         (string string-range-condition-string))
       (define-condition-type &bytevector-range-condition &condition
         make-bytevector-range-condition
         bytevector-range-condition?
         (start bytevector-range-condition-start)
         (end bytevector-range-condition-end)
         (bytevector bytevector-range-condition-bytevector))))))
