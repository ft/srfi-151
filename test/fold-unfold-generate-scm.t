;; -*- scheme -*-

(use-modules (srfi srfi-151)
             (test tap)
             (ice-9 format))

(with-test-bundle (srfi-151 single-bit)
  (plan 3)

  (define-test "(bitwise-fold cons '() #b1010111) => (#t #f #t #f #t #t #t)"
    (pass-if-equal? (bitwise-fold cons '() #b1010111)
                    '(#t #f #t #f #t #t #t)))
  (let ((count 0))
    (bitwise-for-each (lambda (b) (if b (set! count (+ count 1))))
                      #b1010111)
    (define-test "bitwise-for-each: Counting bits works"
      (pass-if-= 5 count)))

  (define-test "bitwise-unfold works as specified"
    (pass-if-= (bitwise-unfold (lambda (i) (= i 10))
                               even?
                               (lambda (i) (+ i 1))
                               0)
               #b101010101)))
