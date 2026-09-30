; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define (error-handling-mode? obj)
  (case obj
    ((raise ignore replace) #t)
    (else #f)))

(define (continuation-byte? b)
  (fx<=? #x80 b #xBF))

(define utf8->string
  (case-lambda
    ((bv) (utf8->string bv 0))
    ((bv start) (utf8->string bv start (bytevector-length bv)))
    ((bv start end)
     (utf8->string bv start end 'raise))
    ((bv start end error-handling-mode)
     (unless (bytevector? bv)
       (error "not a bytevector" bv))
     (unless (exact-integer? start)
       (error "not an exact integer" start))
     (unless (exact-integer? end)
       (error "not an exact integer" end))
     (unless (<= 0 start end (bytevector-length bv))
       (error "invalid bounds" start end))
     (unless (error-handling-mode? error-handling-mode)
       (error "not an error handling mode" error-handling-mode))
     (call-with-port (open-output-string)
       (lambda (port)
         (define (read-first-byte i)
           (if (= i end)
               (get-output-string port)
               (let ((b (bytevector-u8-ref bv i)))
                 (cond
                   ((fx<=? 0 b #x7F)
                    (write-char (integer->char b) port)
                    (read-first-byte (+ i 1)))
                   ((continuation-byte? b)
                    (handle-error i (+ i 1) "continuation byte")
                    (read-first-byte (+ i 1)))
                   ((fx<=? #xC0 b #xC1)
                    (handle-error i (+ i 1) "overlong start byte")
                    (read-first-byte (+ i 1)))
                   ((fx<=? #xC2 b #xDF)
                    (read-continuation-bytes i (+ i 1)
                                             (fxand b #x1F)
                                             1))
                   ((fx=? b #xE0) (read-after-E0 i))
                   ((fx<=? #xE1 b #xEC)
                    (read-continuation-bytes i (+ i 1)
                                             (fxand b #xF)
                                             2))
                   ((fx=? b #xED) (read-ED i))
                   ((fx<=? #xEE b #xEF)
                    (read-continuation-bytes i (+ i 1)
                                             (fxand b #xF)
                                             2))
                   ((fx=? b #xF0) (read-F0 i))
                   ((fx<=? #xF1 b #xF3)
                    (read-continuation-bytes i (+ i 1)
                                             (fxand b #x7)
                                             3))
                   ((fx=? b #xF4) (read-F4 i))
                   (else
                    (handle-error i (+ i 1) "invalid byte")
                    (read-first-byte (+ i 1)))))))
         (define (handle-error start end emsg)
           (case error-handling-mode
             ((raise)
              (raise-unicode-decoding-error 'utf8->string
                                            emsg
                                            bv
                                            start end))
             ((ignore))
             ((replace) (write-char #\xFFFD port))))
         (define (truncated-at-end start end)
           (handle-error start end "truncated at end")
           (get-output-string port))
         (define (read-after-E0 start)
           (let ((i (+ start 1)))
             (if (= i (bytevector-length bv))
                 (truncated-at-end start i)
                 (let ((b (bytevector-u8-ref bv i)))
                   (cond
                     ((fx<=? #x80 b #x9F)
                      (handle-error start i "overlong second byte of two-byte sequence")
                      (read-first-byte i))
                     (else
                      (read-continuation-bytes start i
                                               (fxand #xE0 #x1F)
                                               2)))))))
         (define (read-continuation-bytes start i acc rest)
           (if (= i (bytevector-length bv))
               (truncated-at-end start i)
               (let ((b (bytevector-u8-ref bv i)))
                 (cond
                   ((continuation-byte? b)
                    (let ((acc (fxior (fxarithmetic-shift-left
                                       acc
                                       6)
                                      (fxand b #x3F))))
                      (if (fx=? rest 1)
                          (begin
                            (write-char (integer->char acc)
                                        port)
                            (read-first-byte (+ i 1)))
                          (read-continuation-bytes start (+ i 1)
                                                   acc
                                                   (fx- rest 1)))))
                   (else
                    (handle-error start i "truncated")
                    (read-first-byte i))))))
         (define (read-ED start)
           (let ((i (+ start 1)))
             (if (= i (bytevector-length bv))
                 (truncated-at-end start i)
                 (let ((b (bytevector-u8-ref bv i)))
                   (cond
                     ((fx<=? #x80 b #x9F)
                      (read-continuation-bytes start i
                                               (fxand #xED #xF)
                                               2))
                     ((continuation-byte? b)
                      (handle-error start i "surrogate codepoint")
                      (read-first-byte i))
                     (else
                      (handle-error start i "not a continuation byte")
                      (read-first-byte i)))))))
         (define (read-F0 start)
           (let ((i (+ start 1)))
             (if (= i (bytevector-length bv))
                 (truncated-at-end start i)
                 (let ((b (bytevector-u8-ref bv i)))
                   (cond
                     ((fx<=? #x80 b #x8F)
                      (handle-error start i "overlong 4-byte encoding")
                      (read-first-byte i))
                     (else
                      (read-continuation-bytes start i
                                               (fxand #xF0 #x7)
                                               3)))))))
         (define (read-F4 start)
           (let ((i (+ start 1)))
             (if (= i (bytevector-length bv))
                 (truncated-at-end start i)
                 (let ((b (bytevector-u8-ref bv i)))
                   (cond
                     ((fx<=? #x80 b #x8F)
                      (read-continuation-bytes start i
                                               (fxand #xF4 #x7)
                                               3))
                     ((continuation-byte? b)
                      (handle-error start i "non-Unicode value")
                      (read-first-byte i))
                     (else (handle-error start i "not a continuation byte")
                           (read-first-byte i)))))))
         (read-first-byte start))))))

(define string->utf8
  (case-lambda
    ((string) (string->utf8 string 0))
    ((string start)
     (unless (string? string)
       (error "not a string" string))
     (string->utf8 string 0 (string-length string)))
    ((string start end)
     (unless (string? string)
       (error "not a string" string))
     (unless (exact-integer? start)
       (error "not an exact integer" start))
     (unless (exact-integer? end)
       (error "not an exact integer" end))
     (unless (<= 0 start end (string-length string))
       (error "invalid bounds" start end))
     (call-with-port (open-output-bytevector)
       (lambda (port)
         (do ((vec (string->vector string start end))
              (i 0 (+ i 1)))
             ((= i (vector-length vec))
              (get-output-bytevector port))
           (let ((i (char->integer (vector-ref vec i))))
             (cond
               ((or (negative? i) (> i #x10FFFF))
                (error "not a Unicode character"
                       (vector-ref vec i)))
               ((fx<=? 0 i #x7F)
                (write-u8 i port))
               ((fx<=? #x80 i #x7FF)
                (write-u8 (fxior #xC0
                                 (fxarithmetic-shift-right i 6))
                          port)
                (write-u8 (fxior #x80 (fxand i #x3F)) port))
               ((fx<=? #xD800 i #xDFFF)
                (error "surrogate codepoint"
                       (vector-ref vec i)))
               ((fx<=? #x800 i #xFFFF)
                (write-u8 (fxior #xE0 (fxarithmetic-shift-right
                                       i 12))
                          port)
                (write-u8 (fxior #x80
                                 (fxand
                                  (fxarithmetic-shift-right
                                   i 6)
                                  #x3F))
                          port)
                (write-u8 (fxior #x80 (fxand i #x3F)) port))
               ((fx<=? #x10000 i #x10FFFF)
                (write-u8 (fxior #xF0
                                 (fxarithmetic-shift-right i 18))
                          port)
                (write-u8 (fxior #x80
                                 (fxand
                                  #x3F
                                  (fxarithmetic-shift-right i 12)))
                          port)
                (write-u8 (fxior #x80
                                 (fxand
                                  #x3F
                                  (fxarithmetic-shift-right i 6)))
                          port)
                (write-u8 (fxior #x80 (fxand i #x3F))
                          port))))))))))

(define (can-read-16? i end)
  (< (+ i 1) end))

(define utf16->string
  (case-lambda
    ((bv endianness)
     (utf16->string bv endianness #f))
    ((bv endianness endianness-mandatory?)
     (utf16->string bv endianness endianness-mandatory? 0))
    ((bv endianness endianness-mandatory? start)
     (unless (bytevector? bv)
       (error "not a bytevector" bv))
     (utf16->string bv endianness endianness-mandatory? start
                    (bytevector-length bv)))
    ((bv endianness endianness-mandatory? start end)
     (utf16->string bv endianness endianness-mandatory? start
                    (bytevector-length bv)
                    'raise))
    ((bv endianness endianness-mandatory? start end error-handling-mode)
     (unless (bytevector? bv)
       (error "not a bytevector" bv))
     (unless (endianness? endianness)
       (error "not an endianness" endianness))
     (unless (exact-integer? start)
       (error "not an exact integer" start))
     (unless (exact-integer? end)
       (error "not an exact integer" end))
     (unless (<= 0 start end (bytevector-length bv))
       (error "invalid bounds" start end))
     (unless (error-handling-mode? error-handling-mode)
       (error "not an error handling mode" error-handling-mode))
     (cond
       ((zero? (- end start)) "")
       ((not (can-read-16? start end))
        (case error-handling-mode
          ((raise)
           (raise-unicode-decoding-error 'utf16->string
                                         "too short"
                                         bv
                                         start
                                         (+ start 1)))
          ((ignore) #u8())
          ((replace) (bytevector #\xFFFD))))
       (else
        (let-values (((endianness start)
                      (if endianness-mandatory?
                          (values endianness start)
                          (let ((w (bytevector-u16-ref bv start 'little)))
                            (cond
                              ((fx=? w #xFFFE)
                               (values 'big (+ start 2)))
                              ((fx=? w #xFEFF)
                               (values 'little (+ start 2)))
                              (else
                               (values endianness start)))))))
          (call-with-port (open-output-string)
            (lambda (port)
              (define (handle-error start end emsg)
                (case error-handling-mode
                  ((raise)
                   (raise-unicode-decoding-error 'utf16->string
                                                 emsg bv start end))
                  ((ignore))
                  ((replace) (write-char #\xFFFD port))))
              (define (read-word i)
                (bytevector-u16-ref bv i endianness))
              (define (read-first-word i)
                (if (= i end)
                    (get-output-string port)
                    (let ((word (read-word i)))
                      (cond
                        ((fx<=? #xD800 word #xDBFF)
                         (read-surrogate-pair word i))
                        ((fx<=? #xDC00 word #xDFFF)
                         (handle-error i (+ i 2) "lone high surrogate")
                         (read-first-word (+ i 2)))
                        (else
                         (write-char (integer->char word) port)
                         (read-first-word (+ i 2)))))))
              (define (read-surrogate-pair low start)
                (let ((i (+ start 2)))
                  (if (not (can-read-16? i end))
                      (begin
                        (handle-error start i "truncated surrogate pair")
                        (get-output-string port))
                      (let ((word (read-word i)))
                        (cond
                          ((fx<=? #xDC00 word #xDFFF)
                           (let* ((low (fxand low #x3FF))
                                  (upper (fxand (fx+ 1
                                                     (fxarithmetic-shift-right
                                                      low
                                                      6))
                                                #x1F))
                                  (low-low (fxand low #x3F)))
                             (write-char (integer->char
                                          (fxior (fxarithmetic-shift-left
                                                  upper
                                                  16)
                                                 (fxarithmetic-shift-left
                                                  low-low
                                                  10)
                                                 (fxand word #x3FF)))
                                         port)
                             (read-first-word (+ i 2))))
                          (else
                           (handle-error start i "lone low surrogate")
                           (read-first-word i)))))))
              (read-first-word start)))))))))

(define string->utf16
  (case-lambda
    ((string) (string->utf16 string 'big))
    ((string endianness)
     (string->utf16 string endianness 0))
    ((string endianness start)
     (unless (string? string)
       (error "not a string" string))
     (string->utf16 string endianness start
                    (string-length string)))
    ((string endianness start end)
     (unless (string? string)
       (error "not a string" string))
     (unless (endianness? endianness)
       (error "not an endianness" endianness))
     (unless (exact-integer? start)
       (error "not an exact integer" start))
     (unless (exact-integer? end)
       (error "not an exact integer" end))
     (unless (<= 0 start end (string-length string))
       (error "invalid bounds" start end))
     (call-with-port (open-output-bytevector)
       (lambda (port)
         (do ((vec (string->vector string start end))
              (scratch (make-bytevector 2))
              (i 0 (+ i 1)))
             ((= i (vector-length vec))
              (get-output-bytevector port))
           (let ((c (char->integer (vector-ref vec i))))
             (cond
               ((or (negative? c) (> c #x10FFFF))
                (error "not a Unicode character" c))
               ((fx<=? #xD800 c #xDFFF)
                (error "lone surrogate" c))
               ((or (fx<=? 0 c #xFFFF))
                (bytevector-u16-set! scratch 0 c endianness)
                (write-u8 (bytevector-u8-ref scratch 0) port)
                (write-u8 (bytevector-u8-ref scratch 1) port))
               (else
                (let* ((hibits (fx- (fxand (fxarithmetic-shift-right
                                            c
                                            16)
                                           #x1F)
                                    1))
                       (c (fxior (fxarithmetic-shift-left hibits
                                                          16)
                                 (fxand c #xFFFF))))
                  (bytevector-u16-set! scratch
                                       0
                                       (fxior
                                        #b1101100000000000
                                        (fxarithmetic-shift-right
                                         c
                                         10))
                                       endianness)
                  (write-u8 (bytevector-u8-ref scratch 0) port)
                  (write-u8 (bytevector-u8-ref scratch 1) port)
                  (bytevector-u16-set! scratch
                                       0
                                       (fxior
                                        #b1101110000000000
                                        (fxand c #x3FF))
                                       endianness)
                  (write-u8 (bytevector-u8-ref scratch 0) port)
                  (write-u8 (bytevector-u8-ref scratch 1) port)))))))))))

(define (read-21-bits bv i endianness)
  ;; Read 21 bits, which will fit in the minimum fixnum width.
  (cond-expand
    ((library (srfi 281 u32))
     (bytevector-u32-ref bv i endianness))
    (else
     (case endianness
       ((little)
        (if (not (zero? (bytevector-u8-ref bv (+ i 3))))
            #x110000
            (let ((high (bytevector-u8-ref bv (+ i 2))))
              (if (fx>=? high #x1F)
                  #x110000
                  (fxior
                   (fxarithmetic-shift-left high 16)
                   (bytevector-u16-ref bv i endianness))))))
       ((big)
        (if (not (zero? (bytevector-u8-ref bv i)))
            #x110000
            (let ((high (bytevector-u8-ref bv (+ i 1))))
              (if (fx>=? high #x1F)
                  #x110000
                  (fxior
                   (fxarithmetic-shift-left high 16)
                   (bytevector-u16-ref bv (+ i 2) endianness))))))))))

(define (can-read-32? i end)
  (< (+ i 3) end))

(define utf32->string
  (case-lambda
    ((bv endianness)
     (utf32->string bv endianness #f))
    ((bv endianness endianness-mandatory?)
     (utf32->string bv endianness endianness-mandatory? 0))
    ((bv endianness endianness-mandatory? start)
     (unless (bytevector? bv)
       (error "not a bytevector" bv))
     (utf32->string bv endianness endianness-mandatory? start
                    (bytevector-length bv)))
    ((bv endianness endianness-mandatory? start end)
     (utf32->string bv endianness endianness-mandatory? start
                    (bytevector-length bv)
                    'raise))
    ((bv endianness endianness-mandatory? start end error-handling-mode)
     (unless (bytevector? bv)
       (error "not a bytevector" bv))
     (unless (endianness? endianness)
       (error "not an endianness" endianness))
     (unless (exact-integer? start)
       (error "not an exact integer" start))
     (unless (exact-integer? end)
       (error "not an exact integer" end))
     (unless (<= 0 start end (bytevector-length bv))
       (error "invalid bounds" start end))
     (unless (error-handling-mode? error-handling-mode)
       (error "not an error handling mode" error-handling-mode))
     (cond
       ((zero? (- end start)) "")
       ((not (can-read-32? start end))
        (case error-handling-mode
          ((raise)
           (raise-unicode-decoding-error 'utf32->string
                                         "too short"
                                         bv
                                         start
                                         end))
          ((ignore) #u8())
          ((replace) (bytevector #\xFFFD))))
       (else
        (let-values (((endianness start)
                      (if endianness-mandatory?
                          (values endianness start)
                          (let ((as-little (read-21-bits bv start 'little))
                                (as-big (read-21-bits bv start 'big)))
                            (cond
                              ((fx=? as-little #xFEFF)
                               (values 'little (+ start 4)))
                              ((fx=? as-big #xFEFF)
                               (values 'big (+ start 4)))
                              (else
                               (values endianness start)))))))
          (call-with-port (open-output-string)
            (lambda (port)
              (define (handle-error start end emsg)
                (case error-handling-mode
                  ((raise)
                   (raise-unicode-decoding-error 'utf32->string
                                                 emsg
                                                 bv start end))
                  ((ignore))
                  ((replace) (write-char #\xFFFD port))))
              (define (read-word i)
                (read-21-bits bv i endianness))
              (let loop ((i start))
                (cond
                  ((= i end) (get-output-string port))
                  ((not (can-read-32? i end))
                   (handle-error start end "truncated at end")
                   (get-output-string port))
                  (else
                   (let ((word (read-word i)))
                     (when (> word #x10FFFF)
                       (handle-error i (+ i 4)
                                     "non-Unicode character"))
                     (when (fx<=? #xD800 word #xDFFF)
                       (handle-error i (+ i 4)
                                     "surrogate codepoint"))
                     (write-char (integer->char word) port)
                     (loop (+ i 4))))))))))))))

(define string->utf32
  (case-lambda
    ((string) (string->utf32 string 'big))
    ((string endianness)
     (string->utf32 string endianness 0))
    ((string endianness start)
     (unless (string? string)
       (error "not a string" string))
     (string->utf32 string endianness start
                    (string-length string)))
    ((string endianness start end)
     (unless (string? string)
       (error "not a string" string))
     (unless (endianness? endianness)
       (error "not an endianness" endianness))
     (unless (exact-integer? start)
       (error "not an exact integer" start))
     (unless (exact-integer? end)
       (error "not an exact integer" end))
     (unless (<= 0 start end (string-length string))
       (error "invalid bounds" start end))
     (call-with-port (open-output-bytevector)
       (lambda (port)
         (do ((vec (string->vector string start end))
              (scratch (make-bytevector 4))
              (i 0 (+ i 1)))
             ((= i (vector-length vec))
              (get-output-bytevector port))
           (let ((c (char->integer (vector-ref vec i))))
             (cond
               ((or (negative? c) (> c #x10FFFF))
                (error "not a Unicode character" c))
               ((fx<=? #xD800 c #xDFFF)
                (error "lone surrogate" c))
               (else
                (cond-expand
                  ((library (srfi 281 u32))
                   (bytevector-u32-set! scratch 0 c endianness)
                   (write-u8 (bytevector-u8-ref scratch 0) port)
                   (write-u8 (bytevector-u8-ref scratch 1) port)
                   (write-u8 (bytevector-u8-ref scratch 2) port)
                   (write-u8 (bytevector-u8-ref scratch 3) port))
                  (else
                   (case endianness
                     ((little)
                      (do ((j 0 (fx+ j 1))
                           (c c (fxarithmetic-shift-right c 8)))
                          ((fx=? j 4))
                        (write-u8 (fxand c #xFF) port)))
                     ((big)
                      (write-u8 0 port)
                      (write-u8 (fxarithmetic-shift-right c 16) port)
                      (write-u8 (fxand (fxarithmetic-shift c 8) #xFF) port)
                      (write-u8 (fxand c #xFF) port))))))))))))))