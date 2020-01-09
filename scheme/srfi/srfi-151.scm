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
    "Produces the complement of the bitwise-xor procedure.

When applied to three (or more) arguments, it does not produce a 1 bit
everywhere that a, b and c all agree! For three arguments, it
produces (bitwise-eqv a (bitwise-eqv b c)) or the equivalent
(bitwise-eqv (bitwise-eqv a b) c):

  (bitwise-eqv 37 12 52) => 29

With no arguments it returns -1, with one argument it returns that argument
unchanged."
    (() -1)
    ((a) a)
    ((a b) (bitwise-not (bitwise-xor a b)))
    ((a b . rest) (apply bitwise-eqv (cons (bitwise-eqv a b) rest)))))

(define-public (bitwise-nand . lst)
  "Produce inverted result of bitwise-and."
  (bitwise-not (apply bitwise-and lst)))

(define-public (bitwise-nor . lst)
  "Produce inverted result of bitwise-or."
  (bitwise-not (apply bitwise-ior lst)))

(define (complement-a fnc a b)
  (fnc (bitwise-not a) b))

(define (complement-b fnc a b)
  (fnc a (bitwise-not b)))

(define-public (bitwise-andc1 a b)
  "Binary AND operation with first argument inverted."
  (complement-a bitwise-and a b))

(define-public (bitwise-andc2 a b)
  "Binary AND operation with second argument inverted."
  (complement-b bitwise-and a b))

(define-public (bitwise-orc1 a b)
  "Binary inclusive-OR operation with first argument inverted."
  (complement-a bitwise-ior a b))

(define-public (bitwise-orc2 a b)
  "Binary inclusive-OR operation with second argument inverted."
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

(define-public (bitwise-if mask n0 n1)
  "Merge the bitstrings n0 and n1.

The bitstring mask determines from which string to take each bit. That is, if
the k-th bit of mask is 1, then the k-th bit of the result is the k-th bit of
n0, otherwise the k-th bit of n1:

  (bitwise-if #b111100 #b11110000 #b1111) => #b110011"
  (srfi60:bitwise-if mask n0 n1))

;; Single-Bit Operations

(define-public bit-set? logbit?)
(define-public copy-bit srfi60:copy-bit)

(define-public (bit-swap n0 n1 i)
  "Returns an integer the same as i except that the n0-th bit and the n1-th
bit have been exchanged.

  (bit-swap 0 2 4) => #b1"
  (copy-bit n1
            (copy-bit n0 i (bit-set? n1 i))
            (bit-set? n0 i)))

(define-public any-bit-set? logtest)

(define-public (every-bit-set? test b)
  "Test whether all bits from test are set in b.

  (every-bit-set? 4 6) => #t
  (every-bit-set? 7 6) => #f"
  (= test (logand test b)))

(define-public first-set-bit srfi60:first-set-bit)

;; Bit Field Operations

(define-public bit-field bit-extract)

(define-public (bit-field-any? x start end)
  "Returns true if any of the field's bits are set in bitstring x, and false
otherwise.

  (bit-field-any? #b1001001 1 6) => #t
  (bit-field-any? #b1000001 1 6) => #f"
  (not (zero? (bit-field x start end))))

(define-public (bit-field-every? x start end)
  "Returns false if any of the field's bits are not set in bitstring x, and
true otherwise.

  (bit-field-every? #b1011110 1 5) => #t
  (bit-field-every? #b1011010 1 5) => #f"
  (= (bit-field x start end)
     (bit-field-set 0 0 (- end start))))

(define-public (bit-field-set x start end)
  "Returns x with all of the field's bits set to 1."
  (let ((n (- end start)))
    (bitwise-ior x (arithmetic-shift (1- (ash 1 n)) start))))

(define-public (bit-field-clear x start end)
  "Returns x with all of the field's bits set to 0."
  (bitwise-and x (bitwise-not (bit-field-set 0 start end))))

(define-public (bit-field-replace dst src start end)
  "Returns dest with the field replaced by the least-significant end-start
bits in source.

  (bit-field-replace #b101010 #b010 1 4) => #b100100
  (bit-field-replace #b110 1 0 1) => #b111
  (bit-field-replace #b110 1 1 2) => #b110"
  (bitwise-ior (bit-field-clear dst start end)
               (arithmetic-shift (bit-field src 0 (- end start)) start)))

(define-public (bit-field-replace-same dst src start end)
  "Returns dest with its field replaced by the corresponding field in source.

  (bit-field-replace-same #b1111 #b0000 1 3) => #b1001"
  (bitwise-ior (bit-field-clear dst start end)
               (bitwise-and src (bit-field-set 0 start end))))

(define-public bit-field-rotate srfi60:rotate-bit-field)
(define-public bit-field-reverse srfi60:reverse-bit-field)

;; Bits Conversion

(define-public (bits->list . args)
  "Returns a list of n booleans corresponding to each bit of the non-negative
integer i, returning bit no.0 as the first element, bit no.1 as the second, and
so on. #t is returned for each 1; #f for 0."
  (reverse (apply srfi60:integer->list args)))

(define-public bits->vector
  (case-lambda
    "Returns a vector of n booleans corresponding to each bit of the non-negative
integer i, returning bit no.0 as the first element, bit no.1 as the second, and
so on. #t is returned for each 1; #f for 0."
    ((x) (list->vector (bits->list x)))
    ((x len) (list->vector (bits->list x len)))))

(define-public (list->bits lst)
  "Returns an integer formed from the booleans in list, using the first element
as bit no.0, the second element as bit no.1, and so on. It is an error if list
contains non-booleans. A 1 bit is coded for each #t; a 0 bit for #f."
  (srfi60:list->integer (reverse lst)))

(define-public (vector->bits v)
  "Returns an integer formed from the booleans in vector, using the first element
as bit no.0, the second element as bit no.1, and so on. It is an error if vector
contains non-booleans. A 1 bit is coded for each #t; a 0 bit for #f."
  (list->bits (vector->list v)))

(define-public (bits . lst)
  "Returns the integer coded by the bool arguments. The first argument is bit
no.0, the second argument is bit no.1, and so on."
  (list->bits lst))

;; Fold, Unfold, and Generate

(define-public (bitwise-fold proc seed x)
  "Fold-operation for bitwise integer values.

For each bit b of x from bit no.0 (inclusive) to bit (integer-length x)
(exclusive), proc is called as (proc b r), where r is the current
accumulated result. The initial value of r is seed, and the value returned
by proc becomes the next accumulated result. When the last bit has been
processed,the final accumulated result becomes the result of bitwise-fold.

  (bitwise-fold cons '() #b1010111) => (#t #f #t #f #t #t #t)"
  (let loop ((count-down (integer-length x)) (rest x) (seed seed))
    (if (zero? count-down)
        seed
        (loop (1- count-down) (arithmetic-shift rest -1)
              (proc (not (zero? (bitwise-and rest 1))) seed)))))

(define-public (bitwise-for-each proc x)
  "Similar to for-each bit for bitwise interger values.

Repeatedly applies proc to the bits of x starting with bit no.0 (inclusive) and
ending with bit (integer-length x) (exclusive). The values returned by proc are
discarded. Returns an unspecified value.

      (let ((count 0))
        (bitwise-for-each (lambda (b) (if b (set! count (+ count 1))))
                          #b1010111)
       count) => 5"
  (let loop ((x x) (n (integer-length x)))
    (when (not (zero? n))
      (proc (= 1 (logand 1 x)))
      (loop (arithmetic-shift x -1) (1- x)))))

(define-inlinable (get-bit x)
  (if (boolean? x)
      (if x 1 0)
      (bitwise-and 1 x)))

(define-public (bitwise-unfold stop? mapper successor seed)
  "Unfold for bitwise integer values.

Generates a non-negative integer bit by bit, starting with bit 0. If the result
of applying stop? to the current state (whose initial value is seed) is true,
return the currently accumulated bits as an integer. Otherwise, apply mapper to
the current state to obtain the next bit of the result by interpreting a true
value as a 1 bit and a false value as a 0 bit. Then get a new state by applying
successor to the current state, and repeat this algorithm.

  (bitwise-unfold (lambda (i) (= i 10))
                  even?
                  (lambda (i) (+ i 1))
                  0)) => #b101010101"
  (let loop ((acc 0) (bit 0) (state seed))
    (cond ((stop? state) acc)
          (else (loop (bitwise-ior acc
                                   (arithmetic-shift (get-bit (mapper bit))
                                                     bit))
                      (1+ bit)
                      (successor state))))))

(define-public (make-bitwise-generator value)
  "Returns generator (in terms of SRFI 121) that generates all the bits of
value starting with the least significant bit.

A generator is just a procedure that will produce a new value each time it
is called.

Note that the generator is infinite."
  (lambda ()
    (let ((least-significant-bit (bit-set? 0 value)))
      (set! value (arithmetic-shift value -1))
      least-significant-bit)))
