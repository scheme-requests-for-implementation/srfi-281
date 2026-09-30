; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

;;; TODO: This has to handle whitespace characters.

(define (value->hex-digit value)
  (case value
    ((0) #\0) ((1) #\1) ((2) #\2) ((3) #\3) ((4) #\4)
    ((5) #\5) ((6) #\6) ((7) #\7) ((8) #\8) ((9) #\9)
    ((#xA) #\A) ((#xB) #\B) ((#xC) #\C) ((#xD) #\D)
    ((#xE) #\E) ((#xF) #\F)))

(define bytevector->hex-string
  (case-lambda
    ((bv) (bytevector->hex-string bv 0))
    ((bv start) (bytevector->hex-string bv
                                        start
                                        (bytevector-length bv)))
    ((bv start end)
     (call-with-port (open-output-string)
       (lambda (port)
         (do ((i start (+ i 1)))
             ((= i end) (get-output-string port))
           (let* ((byte (bytevector-u8-ref bv i))
                  (hi (fxarithmetic-shift-right byte 4))
                  (lo (fxand byte #xF)))
             (write-char (value->hex-digit hi) port)
             (write-char (value->hex-digit lo) port))))))))

(define (hex-digit->value char)
  (case char
    ((#\0) 0)
    ((#\1) 1)
    ((#\2) 2)
    ((#\3) 3)
    ((#\4) 4)
    ((#\5) 5)
    ((#\6) 6)
    ((#\7) 7)
    ((#\8) 8)
    ((#\9) 9)
    ((#\A #\a) #xA)
    ((#\B #\b) #xB)
    ((#\C #\c) #xC)
    ((#\D #\d) #xD)
    ((#\E #\e) #xE)
    ((#\F #\f) #xF)
    (else #f)))

(define hex-string->bytevector
  (case-lambda
    ((string) (hex-string->bytevector string 0))
    ((string start) (hex-string->bytevector string
                                            start
                                            (string-length string)))
    ((string start end)
     (unless (even? (- end start))
       (error "not a valid length for string" string start end))
     (call-with-port (open-output-bytevector)
       (lambda (port)
         (define vec (string->vector string start end))
         (define (find-next-hex-digit i)
           (do ((i i (+ i 1)))
               ((or (= i (vector-length vec))
                    (not (char-whitespace? (vector-ref vec i))))
                i)))
         (define (convert-digit i)
           (let ((i (find-next-hex-digit i)))
             (if (= i (vector-length vec))
                 (get-output-bytevector port)
                 (let ((j (find-next-hex-digit (+ i 1))))
                   (when (= j (vector-length vec))
                     (raise-deserializerion-error
                      'hex-string->bytevector
                      "truncated at end"
                      i
                      j))
                   (let ((b1 (hex-digit->value (vector-ref vec i)))
                         (b2 (hex-digit->value (vector-ref vec j))))
                     (unless (and b1 b2)
                       (raise-deserialization-error
                        'hex-string->bytevector
                        "invalid hex double"
                        i
                        (+ j 1)))
                     (write-u8 (fxior (fxarithmetic-shift-left b1 4)
                                      b2)
                               port)
                     (convert-digit (+ j 1)))))))
         (convert-digit 0))))))

(define bytevector->base64
  (case-lambda
    ((bv) (bytevector->base64 bv #f))
    ((bv digits) (bytevector->base64 bv digits 0))
    ((bv digits start)
     (bytevector->base64 bv
                         digits
                         0
                         (bytevector-length bv)))
    ((bv digits start end)
     ;; Base64 takes 6 byte portions of the bytevector at a time.
     ;; This means 3 bytes are scanned at any given time to make
     ;; 4 base64 digits.
     (let ((digits (or digits "+/")))
       (call-with-port (open-output-string)
         (lambda (port)
           (define (ref* i)
             (if (>= i end)
                 0
                 (bytevector-u8-ref bv i)))
           (define (factor-bytes b1 b2 b3)
             ;; [012345][670123][456701][234567]
             (values (fxarithmetic-shift-right b1 2)
                     (fxior (fxarithmetic-shift-left
                             (fxand b1 #b11)
                             4)
                            (fxarithmetic-shift-right
                             b2
                             4))
                     (fxior (fxarithmetic-shift-left
                             (fxand b2 #xF)
                             2)
                            (fxarithmetic-shift-right
                             b3
                             6))
                     (fxand b3 #x3F)))
           (define for62 (string-ref digits 0))
           (define for63 (string-ref digits 1))
           (define (convert digit)
             (case digit
               ((0) #\A) ((17) #\R) ((34) #\i) ((51) #\z)
               ((1) #\B) ((18) #\S) ((35) #\j) ((52) #\0)
               ((2) #\C) ((19) #\T) ((36) #\k) ((53) #\1)
               ((3) #\D) ((20) #\U) ((37) #\l) ((54) #\2)
               ((4) #\E) ((21) #\V) ((38) #\m) ((55) #\3)
               ((5) #\F) ((22) #\W) ((39) #\n) ((56) #\4)
               ((6) #\G) ((23) #\X) ((40) #\o) ((57) #\5)
               ((7) #\H) ((24) #\Y) ((41) #\p) ((58) #\6)
               ((8) #\I) ((25) #\Z) ((42) #\q) ((59) #\7)
               ((9) #\J) ((26) #\a) ((43) #\r) ((60) #\8)
               ((10) #\K) ((27) #\b) ((44) #\s) ((61) #\9)
               ((11) #\L) ((28) #\c) ((45) #\t) ((62) for62)
               ((12) #\M) ((29) #\d) ((46) #\u) ((63) for63)
               ((13) #\N) ((30) #\e) ((47) #\v)
               ((14) #\O) ((31) #\f) ((48) #\w)
               ((15) #\P) ((32) #\g) ((49) #\x)
               ((16) #\Q) ((33) #\h) ((50) #\y)))
           (define (write-bytes i)
             (let-values (((d1 d2 d3 d4)
                           (factor-bytes (ref* i)
                                         (ref* (+ i 1))
                                         (ref* (+ i 2)))))
               (write-char (convert d1) port)
               (write-char (convert d2) port)
               (cond
                 ((= (+ i 1) end)
                  ;; Two padding characters
                  (write-string "==" port))
                 ((= (+ i 2) end)
                  (write-char (convert d3) port)
                  (write-char #\= port))
                 (else
                  (write-char (convert d3) port)
                  (write-char (convert d4) port)))
               (loop (+ i 3))))
           (define (loop i)
             (if (>= i end)
                 (get-output-string port)
                 (write-bytes i)))
           (loop start)))))))

(define standard-base64-char?
  (let ((l (string->list
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ\
             abcdefghijklmnopqrstuvwxyz\
             0123456789=")))
    (lambda (char)
      (memv char l))))

(define base64->bytevector
  (case-lambda
    ((string) (base64->bytevector string #f))
    ((string digits)
     (base64->bytevector string digits 0))
    ((string digits start)
     (unless (string? string)
       (error "not a string" string))
     (base64->bytevector string digits start
                         (string-length string)))
    ((string digits start end)
     ;; A base64 encoded string encodes 3 bytes in 4 ASCII characters.
     (unless (string? string)
       (error "not a string" string))
     (unless (or (not digits)
                 (and (string? digits)
                      (= (string-length digits) 2)
                      (not (standard-base64-char? (string-ref digits 0)))
                      (not (standard-base64-char? (string-ref digits 1)))))
       (error "invalid base64 digits" digits))
     (unless (<= 0 start end (string-length string))
       (error "invalid start and end" string start end))
     (let ((digits (or digits "+/")))
       (call-with-port (open-output-bytevector)
         (lambda (port)
           (define vec (string->vector string start end))
           (define len (vector-length vec))
           (define for62 (string-ref digits 0))
           (define for63 (string-ref digits 1))
           (define (convert char)
             (cond
               ((char=? char for62) 62)
               ((char=? char for63) 63)
               (else
                (case char
                  ((#\A) 0) ((#\R) 17) ((#\i) 34) ((#\z) 51)
                  ((#\B) 1) ((#\S) 18) ((#\j) 35) ((#\0) 52)
                  ((#\C) 2) ((#\T) 19) ((#\k) 36) ((#\1) 53)
                  ((#\D) 3) ((#\U) 20) ((#\l) 37) ((#\2) 54)
                  ((#\E) 4) ((#\V) 21) ((#\m) 38) ((#\3) 55)
                  ((#\F) 5) ((#\W) 22) ((#\n) 39) ((#\4) 56)
                  ((#\G) 6) ((#\X) 23) ((#\o) 40) ((#\5) 57)
                  ((#\H) 7) ((#\Y) 24) ((#\p) 41) ((#\6) 58)
                  ((#\I) 8) ((#\Z) 25) ((#\q) 42) ((#\7) 59)
                  ((#\J) 9) ((#\a) 26) ((#\r) 43) ((#\8) 60)
                  ((#\K) 10) ((#\b) 27) ((#\s) 44) ((#\9) 61)
                  ((#\L) 11) ((#\c) 28) ((#\t) 45)
                  ((#\M) 12) ((#\d) 29) ((#\u) 46)
                  ((#\N) 13) ((#\e) 30) ((#\v) 47)
                  ((#\O) 14) ((#\f) 31) ((#\w) 48)
                  ((#\P) 15) ((#\g) 32) ((#\x) 49)
                  ((#\Q) 16) ((#\h) 33) ((#\y) 50)
                  ((#\=) #f)
                  (else 'bad)))))
           (define (skip-to i)
             (do ((i i (+ i 1)))
                 ((or (>= i len)
                      (not (char-whitespace? (vector-ref vec i))))
                  i)))
           (define (convert-block i)
             ;; [012345] [670123] [456701] [234567]
             (let* ((i2 (skip-to (+ i 1)))
                    (i3 (skip-to (+ i2 1)))
                    (i4 (skip-to (+ i3 1))))
               (when (or (>= i2 len)
                         (>= i3 len)
                         (>= i4 len))
                 (raise-deserialization-error
                  'base64->bytevector
                  "truncated"
                  s
                  i
                  (min i4 len)))
               (let* ((d1 (convert (vector-ref vec i)))
                      (d2 (convert (vector-ref vec i2)))
                      (d3 (convert (vector-ref vec i3)))
                      (d4 (convert (vector-ref vec i4))))
                 (when (or (eq? d1 'bad)
                           (eq? d2 'bad)
                           (eq? d3 'bad)
                           (eq? d4 'bad))
                   (raise-deserialization-error
                    'base64->bytevector
                    "invalid base64 characters"
                    s
                    i
                    (+ i4 1)))
                 (write-u8 (fxior (fxarithmetic-shift-left d1 2)
                                  (fxarithmetic-shift-right d2 4))
                           port)
                 (when d3
                   (write-u8 (fxior (fxarithmetic-shift-left
                                     (fxand d2 #xF)
                                     4)
                                    (fxarithmetic-shift-right d3 2))
                             port))
                 (when d4
                   (write-u8 (fxior (fxarithmetic-shift-left
                                     (fxand d3 #x3)
                                     6)
                                    d4)
                             port))
                 (if (or (not d3) (not d4))
                     (ensure-end (+ i4 1))
                     (loop (+ i4 1))))))
           (define (ensure-end i)
             (let ((j (skip-to i)))
               (unless (>= j len)
                 (raise-deserialization-error
                  'base64->bytevector
                  "data after padding characters"
                  string
                  i
                  j))
               (get-output-bytevector port)))
           (define (loop i)
             (let ((i (skip-to i)))
               (if (>= i len)
                   (get-output-bytevector port)
                   (convert-block i))))
           (loop 0)))))))
