;; -*- scheme -*-

(use-modules (srfi srfi-151)
             (test tap)
             (ice-9 match))

(define (title/shift v s r)
  (format #f "(arithmetic-shift ~a ~a) => ~a" v s r))

(define *shift-tests* '(((value  .  8)
                         (shift  .  2)
                         (result . 32))
                        ((value  .  4)
                         (shift  .  0)
                         (result .  4))
                        ((value  .  8)
                         (shift  . -1)
                         (result .  4))
                        ((value  . -100000000000000000000000000000000)
                         (shift  . -100)
                         (result . -79))))

(define (title/count v r c)
  (format #f "(bit-count ~a) => ~a~a~a" v r
          (if c " ;; " "")
          (or c "")))

(define *bit-count-tests* `(((value   .   0) (result . 0))
                            ((value   .  -1) (result . 0))
                            ((value   .  13) (result . 3)
                             (comment . "Two's-complement binary: ...0001101"))
                            ((value   . -13) (result . 2)
                             (comment . "Two's-complement binary: ...1110011"))
                            ((value   .  30) (result . 4)
                             (comment . "Two's-complement binary: ...0011110"))
                            ((value   . -30) (result . 4)
                             (comment . "Two's-complement binary: ...1100010"))
                            ((value   . ,(expt 2 100))
                             (result  . 1))
                            ((value   . ,(- (expt 2 100)))
                             (result  . 100))
                            ((value   . ,(- (1+ (expt 2 100))))
                             (result  . 1))))

(define (title/length v r)
  (format #f "(integer-length ~a) => ~a" v r))

(define *integer-length-tests* '(((value .  0) (result . 0))
                                 ((value .  1) (result . 1))
                                 ((value . -1) (result . 0))
                                 ((value .  7) (result . 3))
                                 ((value . -7) (result . 3))
                                 ((value .  8) (result . 4))
                                 ((value . -8) (result . 3))))

(define (title/bw-if m a b r)
  (format #f "(bitwise-if ~a ~a ~a) => ~a" m a b r))

(define *bitwise-if-tests*
  '(((mask . 3) (n0 . 1) (n1 . 8)
     (result . 9))
    ((mask . 3) (n0 . 8) (n1 . 1)
     (result . 0))
    ((mask . 1) (n0 . 1) (n1 . 2)
     (result . 3))
    ((mask . #b00111100) (n0 . #b11110000) (n1 . #b00001111)
     (result . #b00110011))))

(with-test-bundle (srfi-151 integer)
  (plan (apply + (map length (list *shift-tests*
                                   *bit-count-tests*
                                   *integer-length-tests*
                                   *bitwise-if-tests*))))

  (for-each-test (*shift-tests* => this)
    (match this
      ((('value . v) ('shift . s) ('result . r))
       (define-test (title/shift v s r)
         (pass-if-= (arithmetic-shift v s) r)))))

  (for-each-test (*bit-count-tests* => this)
    (match this
      ((('value . v) ('result . r) ('comment . c))
       (define-test (title/count v r c)
         (pass-if-= (bit-count v) r)))
      ((('value . v) ('result . r))
       (define-test (title/count v r #f)
         (pass-if-= (bit-count v) r)))))

  (for-each-test (*integer-length-tests* => this)
    (match this
      ((('value . v) ('result . r))
       (define-test (title/length v r)
         (pass-if-= (integer-length v) r)))))

  (for-each-test (*bitwise-if-tests* => this)
    (match this
      ((('mask . m) ('n0 . n0) ('n1 . n1) ('result . r))
       (define-test (title/bw-if m n0 n1 r)
         (pass-if-= (bitwise-if m n0 n1) r))))))
