; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define (endianness? obj)
  (case obj
    ((little big) #t)
    (else #f)))

(define s8->u8 (signed->unsigned-factory 1))

(define (check-fill fill)
  (cond
    ((not (exact-integer? fill))
     (error "not an exact integer" fill))
    ((<= -128 fill -1)
     (s8->u8 fill))
    ((<= 0 fill 255) fill)
    (else (error "fill not a signed or unsigned byte" fill))))

(define make-bytevector
  (case-lambda
    ((k) (make-bytevector k 0))
    ((k fill)
     (unless (and (exact-integer? k)
                  (not (negative? k)))
       (error "not an exact, non-negative integer" k))
     (let ((fill (check-fill fill)))
       (r7rs:make-bytevector k fill)))))

(define (bytevector . rest)
  (do ((bv (make-bytevector (length rest)))
       (rest rest (cdr rest))
       (i 0 (+ i 1)))
      ((= i (bytevector-length bv)) bv)
    (bytevector-u8-set! bv i (check-fill (car rest)))))

(define (bytevector=? x y . rest)
  (unless (bytevector? x)
    (error "not a bytevector" x))
  (let loop ((x x) (y y) (rest rest))
    (cond
      ((not (bytevector? y))
       (error "not a bytevector" y))
      (else (and (equal? x y)
                 (or (null? rest)
                     (loop y (car rest) (cdr rest))))))))

(define bytevector-fill!
  (case-lambda
    ((bv fill) (bytevector-fill! bv fill 0))
    ((bv fill start)
     (bytevector-fill! bv fill start (bytevector-length bv)))
    ((bv fill start end)
     (unless (bytevector? bv)
       (error "not a bytevector" bv))
     (do ((i start (+ i 1))
          (fill (check-fill fill)))
         ((= i end))
       (bytevector-u8-set! bv i fill)))))
