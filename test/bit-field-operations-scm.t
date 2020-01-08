;; -*- scheme -*-

(use-modules (srfi srfi-151)
             (test tap)
             (ice-9 format)
             (ice-9 match))

(define (title/bit-field a b c r)
  (format #f "(bit-field #x~x ~a ~a) => #x~x" a b c r))

(define *field-tests*
  '(((int . #b1101101010) (start . 0) (end .   4) (result . #b001010))
    ((int . #b1101101010) (start . 3) (end .   9) (result . #b101101))
    ((int . #b1101101010) (start . 4) (end .   9) (result . #b010110))
    ((int . #b1101101010) (start . 4) (end .  10) (result . #b110110))
    ((int .            6) (start . 0) (end .   1) (result . 0))
    ((int .            6) (start . 1) (end .   3) (result . 3))
    ((int .            6) (start . 2) (end . 999) (result . 1))
    ((int . #x100000000000000000000000000000000) (start . 128) (end . 129)
     (result . 1))))

(define (title/bit-field-any? a b c r)
  (format #f "(bit-field-any? #b~b ~a ~a) => ~a" a b c r))

(define *any-tests*
  '(((int . #b1001001) (start . 1) (end . 6) (result . #t))
    ((int . #b1000001) (start . 1) (end . 6) (result . #f))))

(define (title/replace dst src s e r)
  (format #f "(bit-field-replace #b~b #b~b ~a ~a) => #b~b" dst src s e r))

(define *replace-tests*
  '(((dst . #b101010) (src . #b010) (start . 1) (end . 4) (result . #b100100))
    ((dst . #b000110) (src . #b001) (start . 0) (end . 1) (result . #b000111))
    ((dst . #b000110) (src . #b001) (start . 1) (end . 2) (result . #b000110))))

(define (title/rotate int count s e r)
  (format #f "(bit-field-rotate #x~x ~a ~a ~a) => #x~x" int count s e r))

(define *rotate-tests*
  '(((int . #b0110) (count .  0) (start . 0) (end .  10) (result . #b0110))
    ((int . #b0110) (count .  0) (start . 0) (end . 256) (result . #b0110))
    ((int . #x100000000000000000000000000000000)
                    (count .  1) (start . 0) (end . 129) (result .      1))
    ((int . #b0110) (count .  1) (start . 1) (end .   2) (result . #b0110))
    ((int . #b0110) (count .  1) (start . 2) (end .   4) (result . #b1010))
    ((int . #b0111) (count . -1) (start . 1) (end .   4) (result . #b1011))))

(define (title/reverse int s e r)
  (format #f "(bit-field-reverse #x~x ~a ~a) => #x~x" int s e r))

(define *reverse-tests*
  '(((int . 6) (start . 1) (end .  3) (result .  6))
    ((int . 6) (start . 1) (end .  4) (result . 12))
    ((int . 1) (start . 0) (end . 32) (result . #x80000000))
    ((int . 1) (start . 0) (end . 31) (result . #x40000000))
    ((int . 1) (start . 0) (end . 30) (result . #x20000000))
    ((int . #x140000000000000000000000000000000)
     (start . 0) (end . 129) (result . 5))))

(with-test-bundle (srfi-151 bit-field)
  (plan (+ (apply + (map length (list *field-tests*
                                      *any-tests*
                                      *replace-tests*
                                      *rotate-tests*
                                      *reverse-tests*)))
           3))

  (for-each-test (*field-tests* => this)
    (match this
      ((('int . int) ('start . s) ('end . e) ('result . r))
       (define-test (title/bit-field int s e r)
         (pass-if-= (bit-field int s e) r)))))

  (for-each-test (*any-tests* => this)
    (match this
      ((('int . int) ('start . s) ('end . e) ('result . r))
       (define-test (title/bit-field-any? int s e r)
         (pass-if-equal? (bit-field-any? int s e) r)))))

  (define-test "(bit-field-clear #b101010 1 4) => #b100000"
    (pass-if-= (bit-field-clear #b101010 1 4) #b100000))
  (define-test "(bit-field-set #b101010 1 4) => #b101110"
    (pass-if-= (bit-field-set #b101010 1 4) #b101110))

  (for-each-test (*replace-tests* => this)
    (match this
      ((('dst . dst) ('src . src) ('start . s) ('end . e) ('result . r))
       (define-test (title/replace dst src s e r)
         (pass-if-= (bit-field-replace dst src s e) r)))))

  (define-test "(bit-field-replace-same #b1111 #b0000 1 3) => #b1001"
    (pass-if-= (bit-field-replace-same #b1111 #b0000 1 3) #b1001))

  (for-each-test (*rotate-tests* => this)
    (match this
      ((('int . int) ('count . count) ('start . s) ('end . e) ('result . r))
       (define-test (title/rotate int count s e r)
         (pass-if-= (bit-field-rotate int count s e) r)))))

  (for-each-test (*reverse-tests* => this)
    (match this
      ((('int . int) ('start . s) ('end . e) ('result . r))
       (define-test (title/reverse int s e r)
         (pass-if-= (bit-field-reverse int s e) r))))))
