;; -*- scheme -*-

(use-modules (srfi srfi-151)
             (test tap)
             (ice-9 format))

(define (title/apply proc args r)
  (if (integer? r)
      (format #f "(~a~{ ~a~}) => #b~b" proc args r)
      (format #f "(~a~{ ~a~}) => ~a" proc args r)))

(define *b2l-tests*
  '(((args #b1110101) (result #t #f #t #f #t #t #t))
    ((args 3 5)       (result #t #t #f #f #f))
    ((args 6 4)       (result #f #t #t #f))))

(define *b2v-tests*
  '(((args #b1110101) (result . #(#t #f #t #f #t #t #t)))))

(define (title/x2b n v r)
  (format #f "(~a ~a) => #b~b" n v r))

(define *l2b-tests*
  '(((args #t #f #t #f #t #t #t)       (result . #b001110101))
    ((args #f #f #t #f #t #f #t #t #t) (result . #b111010100))
    ((args #f #t #t)    (result .  6))
    ((args #f #t #t #f) (result .  6))
    ((args #f #f #t #t) (result . 12))))

(define *v2b-tests*
  '(((args . #(#t #f #t #f #t #t #t))       (result . #b001110101))
    ((args . #(#f #f #t #f #t #f #t #t #t)) (result . #b111010100))
    ((args . #(#f #t #t))                   (result .  6))
    ((args . #(#f #t #t #f))                (result .  6))
    ((args . #(#f #f #t #t))                (result . 12))))

(define *bits-tests*
  '(((args #t #f #t #f #t #t #t)       (result . #b001110101))
    ((args #f #f #t #f #t #f #t #t #t) (result . #b111010100))))

(with-test-bundle (srfi-151 single-bit)
  (plan (apply + (map length (list *b2l-tests*
                                   *b2v-tests*
                                   *l2b-tests*
                                   *v2b-tests*
                                   *bits-tests*))))

  (for-each-test (*b2l-tests* => this)
    (let ((args (assq-ref this 'args))
          (r (assq-ref this 'result)))
      (define-test (title/apply 'bits->list args r)
        (pass-if-equal? (apply bits->list args) r))))

  (for-each-test (*b2v-tests* => this)
    (let ((args (assq-ref this 'args))
          (r (assq-ref this 'result)))
      (define-test (title/apply 'bits->vector args r)
        (pass-if-equal? (apply bits->vector args) r))))

  (for-each-test (*l2b-tests* => this)
    (let ((args (assq-ref this 'args))
          (r (assq-ref this 'result)))
      (define-test (title/x2b 'list->bits args r)
        (pass-if-equal? (list->bits args) r))))

  (for-each-test (*v2b-tests* => this)
    (let ((args (assq-ref this 'args))
          (r (assq-ref this 'result)))
      (define-test (title/x2b 'vector->bits args r)
        (pass-if-equal? (vector->bits args) r))))

  (for-each-test (*bits-tests* => this)
    (let ((args (assq-ref this 'args))
          (r (assq-ref this 'result)))
      (define-test (title/apply 'bits args r)
        (pass-if-equal? (apply bits args) r)))))
