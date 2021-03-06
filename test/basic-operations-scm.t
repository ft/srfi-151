;; -*- scheme -*-

(use-modules (srfi srfi-151)
             (test tap)
             (ice-9 match))

(define (title/binary n a b r)
  (format #f "(~a ~a ~a) => ~a" n a b r))

(define (title/unary n v r)
  (format #f "(~a ~a) => ~a" n v r))

(define (title/not v r)
  (title/unary 'bitwise-not v r))

(define *not-tests* '(((value .  10) (result . -11))
                      ((value . -37) (result .  36))))

(define *log-tests* `(((name . bitwise-ior)
                       (proc . ,bitwise-ior)
                       (a . 3) (b . 10) (result . 11))
                      ((name . bitwise-and)
                       (proc . ,bitwise-and)
                       (a . 11) (b . 26) (result . 10))
                      ((name . bitwise-xor)
                       (proc . ,bitwise-xor)
                       (a . 3) (b . 10) (result . 9))
                      ((name . bitwise-eqv)
                       (proc . ,bitwise-eqv)
                       (a . 37) (b . 12) (result . -42))
                      ((name . bitwise-and)
                       (proc . ,bitwise-and)
                       (a . 37) (b . 12) (result . 4))
                      ((name . bitwise-nand)
                       (proc . ,bitwise-nand)
                       (a . 11) (b . 26) (result . -11))
                      ((name . bitwise-nor)
                       (proc . ,bitwise-nor)
                       (a . 11) (b . 26) (result . -28))
                      ((name . bitwise-andc1)
                       (proc . ,bitwise-andc1)
                       (a . 11) (b . 26) (result . 16))
                      ((name . bitwise-andc2)
                       (proc . ,bitwise-andc2)
                       (a . 11) (b . 26) (result . 1))
                      ((name . bitwise-orc1)
                       (proc . ,bitwise-orc1)
                       (a . 11) (b . 26) (result . -2))
                      ((name . bitwise-orc2)
                       (proc . ,bitwise-orc2)
                       (a . 11) (b . 26) (result . -17))))

(with-test-bundle (srfi-151 basic)
  (plan (+ (apply + (map length (list *not-tests*
                                      *log-tests*)))
           5))

  (for-each-test (*not-tests* => this)
    (match this
      ((('value . v) ('result . r))
       (define-test (title/not v r)
         (pass-if-= (bitwise-not v) r)))))

  (for-each-test (*log-tests* => this)
    (match this
      ((('name . n) ('proc . p) ('a . a) ('b . b) ('result . r))
       (define-test (title/binary n a b r)
         (pass-if-= (p a b) r)))))

  (define-test "(bitwise-eqv) => -1"
    (pass-if-= (bitwise-eqv) -1))
  (define-test "(bitwise-eqv 123) => 123"
    (pass-if-= (bitwise-eqv 123) 123))
  (define-test "(bitwise-eqv 37 12 52) == (bitwise-eqv (bitwise-eqv 37 12) 52)"
    (pass-if-= (bitwise-eqv 37 12 52)
               (bitwise-eqv (bitwise-eqv 37 12) 52)))
  (define-test "(bitwise-eqv 37 12 52) == (bitwise-eqv 37 (bitwise-eqv 12 52))"
    (pass-if-= (bitwise-eqv 37 12 52)
               (bitwise-eqv 37 (bitwise-eqv 12 52))))
  (define-test "(bitwise-eqv 37 12 52) != (bitwise-ior ...)"
    (let ((a 37)
          (b 12)
          (c 52))
      (pass-if-not-= (bitwise-eqv a b c)
                     (bitwise-ior (bitwise-and a b c)
                                  (bitwise-and (bitwise-not a)
                                               (bitwise-not b)
                                               (bitwise-not c)))))))
