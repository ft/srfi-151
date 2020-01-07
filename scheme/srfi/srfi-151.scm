;; SRFI 151: Bitwise Operations
;; https://srfi.schemers.org/srfi-151/

(define-module (srfi srfi-151)
  #:use-module ((srfi srfi-60) #:prefix srfi60:))

;; Basic Operations

(define-public bitwise-not lognot)
(define-public bitwise-and logand)
(define-public bitwise-ior logior)
(define-public bitwise-xor logxor)

(define-public bitwise-eqv
  (case-lambda
    (() -1)
    ((a) a)
    ((a b) (bitwise-not (bitwise-xor a b)))
    ((a b . rest) (apply bitwise-eqv (cons (bitwise-eqv a b) rest)))))

(define-public (bitwise-nand . lst)
  (bitwise-not (apply bitwise-and lst )))

(define-public (bitwise-nor . lst)
  (bitwise-not (apply bitwise-ior lst )))

(define (complement-a fnc a b)
  (fnc (bitwise-not a) b))

(define (complement-b fnc a b)
  (fnc a (bitwise-not b)))

(define-public (bitwise-andc1 a b)
  (complement-a bitwise-and a b))

(define-public (bitwise-andc2 a b)
  (complement-b bitwise-and a b))

(define-public (bitwise-orc1 a b)
  (complement-a bitwise-ior a b))

(define-public (bitwise-orc2 a b)
  (complement-b bitwise-ior a b))

;; Integer Operations

(define-public arithmetic-shift ash)
(define-public bit-count logcount)

;; Guile has integer-length implemented in C in the default environment.

;;(define (integer-length x)
;;  (inexact->exact (ceiling (/ (log (if (negative? x)
;;                                       (- x)
;;                                       (+ 1 x)))
;;                              (log 2)))))

(define-public bitwise-if srfi60:bitwise-if)

;; Single-Bit Operations

(define-public bit-set? logbit?)
(define-public copy-bit srfi60:copy-bit)

(define-public (bit-swap n1 n2 i)
  (copy-bit n2
            (copy-bit n1 i (bit-set? n2 i))
            (bit-set? n1 i)))

(define-public any-bit-set? logtest)

(define-public (every-bit-set? test b)
  (= test (logand test b)))

(define-public first-set-bit srfi60:first-set-bit)

;; Bit Field Operations

(define-public bit-field bit-extract)

(define-public (bit-field-any? x start end)
  (not (zero? (bit-field x start end))))

(define-public (bit-field-every? x start end)
  (= (bit-field x start end)
     (bit-field-set 0 0 (- end start))))

(define-public (bit-field-set x start end)
  (let ((n (- end start)))
    (bitwise-ior x (arithmetic-shift (1- (ash 1 n)) start))))

(define-public (bit-field-clear x start end)
  (bitwise-and x (bitwise-not (bit-field-set 0 start end))))

(define-public (bit-field-replace dst src start end)
  (bitwise-ior (bit-field-clear dst start end)
               (arithmetic-shift (bit-field src 0 (- end start)) start)))

(define-public (bit-field-replace-same dst src start end)
  (bitwise-ior (bit-field-clear dst start end)
               (bitwise-and src (bit-field-set 0 start end))))

(define-public bit-field-rotate srfi60:rotate-bit-field)
(define-public bit-field-reverse srfi60:reverse-bit-field)

;; Bits Conversion

(define-public (bits->list . args)
  (reverse (apply srfi60:integer->list args)))

(define-public bits->vector
  (case-lambda
    ((x) (list->vector (bits->list x)))
    ((x len) (list->vector (bits->list x len)))))

(define-public (list->bits lst)
  (srfi60:list->integer (reverse lst)))

(define-public (vector->bits v)
  (list->bits (vector->list v)))

(define-public (bits . lst)
  (list->bits lst))

;; Fold, Unfold, and Generate

(define-public (bitwise-fold proc seed x)
  (let loop ((count-down (integer-length x)) (rest x) (seed seed))
    (if (zero? count-down)
        seed
        (loop (1- count-down) (arithmetic-shift rest -1)
              (proc (not (zero? (bitwise-and rest 1))) seed)))))

(define-public (bitwise-for-each proc x)
  (let loop ((x x) (n (integer-length x)))
    (when (not (zero? n))
      (proc (= 1 (logand 1 x)))
      (loop (arithmetic-shift x -1) (1- x)))))

(define-inlinable (get-bit x)
  (if (boolean? x)
      (if x 1 0)
      x))

(define-public (bitwise-unfold stop? mapper successor seed)
  (let loop ((acc 0) (bit 0) (state seed))
    (cond ((stop? state) acc)
          (else (loop (logior acc
                              (arithmetic-shift (get-bit (mapper bit))
                                                bit))
                      (1+ bit)
                      (successor state))))))
