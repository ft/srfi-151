;; -*- scheme -*-

(use-modules (srfi srfi-151)
             (test tap)
             (ice-9 format))

(define (title/binary n a b r)
  (format #f "(~a ~a ~a) => ~a" n a b r))

(define (title/copy idx int bool r)
  (format #f "(copy-bit ~a #b~b ~a) => #b~b" idx int bool r))

(define (title/unary n v r)
  (format #f "(~a ~a) => ~a" n v r))

(define (title/set? a b r)
  (title/binary 'bit-set? a b r))

(define *not-tests* '(((value .  10) (result . -11))
                      ((value . -37) (result .  36))))

(define *set-tests* `(((a .       1) (b .  1) (result . #f))
                      ((a .       0) (b .  1) (result . #t))
                      ((a .       3) (b . 10) (result . #t))
                      ((a . 1000000) (b . -1) (result . #t))
                      ((a .       2) (b .  6) (result . #t))
                      ((a .       0) (b .  6) (result . #f))))

(define *copy-tests* '(((index  . 0)
                        (int    . 0)
                        (bool   . #t)
                        (result . #b1))
                       ((index  . 2)
                        (int    . 0)
                        (bool   . #t)
                        (result . #b100))
                       ((index  . 2)
                        (int    . #b1111)
                        (bool   . #f)
                        (result . #b1011))))

(define *any-tests* '(((mask . 3) (int .  6) (result . #t))
                      ((mask . 3) (int . 12) (result . #f))))

(define *every-tests* '(((mask . 4) (int . 6) (result . #t))
                        ((mask . 7) (int . 6) (result . #f))))

(define *first-tests* `(((int .             1) (result .  0))
                        ((int .             2) (result .  1))
                        ((int .             0) (result . -1))
                        ((int .            40) (result .  3))
                        ((int .           -28) (result .  2))
                        ((int . ,(expt  2 99)) (result . 99))
                        ((int . ,(expt -2 99)) (result . 99))))

(with-test-bundle (srfi-151 single-bit)
  (plan (+ (apply + (map length (list *set-tests*
                                      *copy-tests*
                                      *any-tests*
                                      *every-tests*
                                      *first-tests*)))
           1))

  (for-each-test (*set-tests* => this)
    (let ((a (assq-ref this 'a))
          (b (assq-ref this 'b))
          (r (assq-ref this 'result)))
      (define-test (title/set? a b r)
        (pass-if-equal? (bit-set? a b) r))))

  (for-each-test (*copy-tests* => this)
    (let ((idx (assq-ref this 'index))
          (int (assq-ref this 'int))
          (bool (assq-ref this 'bool))
          (r (assq-ref this 'result)))
      (define-test (title/copy idx int bool r)
        (pass-if-= (copy-bit idx int bool) r))))

  (define-test "(bit-swap 0 2 4) => #b1"
    (pass-if-= (bit-swap 0 2 4) #b1))

  (for-each-test (*any-tests* => this)
    (let ((mask (assq-ref this 'mask))
          (int (assq-ref this 'int))
          (r (assq-ref this 'result)))
      (define-test (title/binary 'any-bit-set? mask int r)
        (pass-if-equal? (any-bit-set? mask int) r))))

  (for-each-test (*every-tests* => this)
    (let ((mask (assq-ref this 'mask))
          (int (assq-ref this 'int))
          (r (assq-ref this 'result)))
      (define-test (title/binary 'every-bit-set? mask int r)
        (pass-if-equal? (every-bit-set? mask int) r))))

  (for-each-test (*first-tests* => this)
    (let ((int (assq-ref this 'int))
          (r (assq-ref this 'result)))
      (define-test (title/unary 'first-set-bit int r)
        (pass-if-equal? (first-set-bit int) r)))))
