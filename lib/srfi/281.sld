; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281)
  (import (srfi 281 base) (srfi 281 u8) (srfi 281 serialization))
  (export endianness? native-endianness
          bytevector bytevector? make-bytevector bytevector-fill!
          bytevector-length
          bytevector=?
          bytevector-copy
          bytevector-u8-ref bytevector-u8-set!
          u8-list->bytevector bytevector->u8-list
          deserialization-error?
          deserialization-error-string
          deserialization-error-start
          deserialization-error-end
          deserialization-error-message
          bytevector->hex-string hex-string->bytevector
          bytevector->base64 base64->bytevector)
  (cond-expand
    ((or gauche chicken chibi (library (srfi 281 endianness)))
     (import (srfi 281 endianness))
     (export endianness))
    (else))
  (cond-expand
    ((or tr7 gauche chicken chibi (library (srfi 281 s8)))
     (import (srfi 281 s8))
     (export bytevector-s8-ref bytevector-s8-set!))
    (else))
  (cond-expand
    ((or chibi gauche chicken (library (srfi 281 int)))
     (import (srfi 281 int))
     (export bytevector-uint-ref bytevector-uint-set!
             bytevector-sint-ref bytevector-sint-set!
             uint-list->bytevector bytevector->uint-list
             sint-list->bytevector bytevector->sint-list))
    (else))
  (cond-expand
    ((or tr7 gauche chicken chibi)
     (import (srfi 281 u16))
     (export bytevector-u16-ref bytevector-u16-set!
             bytevector-u16-native-ref bytevector-u16-native-set!))
    (else))
  (cond-expand
    ((or tr7 gauche chicken chibi)
     (import (srfi 281 s16))
     (export bytevector-s16-ref bytevector-s16-set!
             bytevector-s16-native-ref bytevector-s16-native-set!))
    (else))
  (cond-expand
    ((or (and |64bit| tr7) gauche chicken chibi)
     (import (srfi 281 u32))
     (export bytevector-u32-ref bytevector-u32-set!
             bytevector-u32-native-ref bytevector-u32-native-set!))
    (else))
  (cond-expand
    ((or (and |64bit| tr7) gauche chicken chibi)
     (import (srfi 281 s32))
     (export bytevector-s32-ref bytevector-s32-set!
             bytevector-s32-native-ref bytevector-s32-native-set!))
    (else))
  (cond-expand
    ((or gauche chicken chibi)
     (import (srfi 281 s64))
     (export bytevector-s64-ref bytevector-s64-set!
             bytevector-s64-native-ref bytevector-s64-native-set!))
    (else))
  (cond-expand
    ((or gauche chicken chibi)
     (import (srfi 281 u64))
     (export bytevector-u64-ref bytevector-u64-set!
             bytevector-u64-native-ref bytevector-u64-native-set!))
    (else))
  (cond-expand
    ((or gauche chicken tr7 chibi)
     (import (srfi 281 f32))
     (export bytevector-binary32-set!
             bytevector-ieee-single-set!
             bytevector-binary32-ref
             bytevector-ieee-single-ref
             bytevector-binary32-native-set!
             bytevector-ieee-single-native-set!
             bytevector-binary32-native-ref
             bytevector-ieee-single-native-ref))
    (else))
  (cond-expand
    ((or gauche chicken tr7 chibi)
     (import (srfi 281 f64))
     (export bytevector-binary64-set!
             bytevector-ieee-single-set!
             bytevector-binary64-ref
             bytevector-ieee-single-ref
             bytevector-binary64-native-set!
             bytevector-ieee-single-native-set!
             bytevector-binary64-native-ref
             bytevector-ieee-single-native-ref))
    (else))
  (cond-expand
    ((or gauche chibi chicken tr7)
     (import (srfi 281 unicode))
     (export error-handling-mode?
             unicode-decoding-error?
             unicode-decoding-error-bytevector
             unicode-decoding-error-start
             unicode-decoding-error-end
             unicode-decoding-error-message
             string->utf8 string->utf16 string->utf32
             utf8->string utf16->string utf32->string))
    (else))
  (cond-expand
    ((library (rnrs conditions))
     (import (srfi 281 condition))
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
             bytevector-range-condition-bytevector))
    (else))
  (cond-expand
    ((or gauche chicken (library (srfi 281 error-handling-mode)))
     (import (srfi 281 error-handling-mode))
     (export error-handling-mode))
    (else)))