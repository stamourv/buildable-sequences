#lang racket

(require racket/generic)

;; Attempt at implementing buildable sequences.
;; i.e. sequences that also know how to build themselves
;; That's all we need for a generic map, filter, etc.

;; TODO take a closer look at Christophe Rhodes's proposal for CL
;;  (www.doc.gold.ac.uk/~mas01cr/papers/ilc2007/sequences-20070301.pdf‎)
;;  In particular, his `make-sequence-like' method may be nicer than
;;  `get-builder' + `finalize-builder' + `get-size-hint'. OTOH, it doesn't
;;  look like his proposal allows for incremental building (initial contents
;;  need to all be passed in), and seems to alwasy require allocating an
;;  intermediate data structure to hold the initial contents. His interface
;;  is simple, though.

(define-generics buildable
  #:defined-table buildable-defined-table
  ;; either you define these
  [get-cons buildable]
  [get-null buildable]
  ;; or you define these two, and maybe get-size-hint too
  [get-builder buildable [size-hint]]
  ;; TODO actually, finalize-builder is not defined in the original struct,
  ;;  only on the external builder
  ;; TODO maybe add it to the 1st set of methods, and call it all the time
  ;;  (if defined) for any kind of post-processing, external builder or not?
  [finalize-builder buildable] ; external-builder -> original-thing
  [get-size-hint buildable]) ; may be called by map, etc., but not, say, filter

;; TODO if that doesn't end up being `map' in racket/base, can still be
;;  `map' in racket/something-else, that would shadow racket/base
(define (map/g f s) ;; TODO work with multiple sequences. which one wins?
  (define defined-table (buildable-defined-table s))
  (define directly-buildable?
    (and (dict-ref defined-table 'get-cons)
         (dict-ref defined-table 'get-null)))
  (define builder
    (if directly-buildable?
        ;; directly buildable, its own builder
        s
        ;; has an external builder struct
        (if (dict-ref defined-table 'get-size-hint)
            (get-builder s (get-size-hint s))
            (get-builder s))))
  (define cons (get-cons builder))
  (define null (get-null builder))
  (define res
    (let loop ([s (sequence->stream s)])
      (if (stream-empty? s)
          null
          ;; TODO stream-first can return multiple values (hashes), bleh
          (cons (f (stream-first s)) (loop (stream-rest s))))))
  (if directly-buildable? res (finalize-builder res)))

(define (filter/g f s)
  (define defined-table (buildable-defined-table s)) ;; TODO abstract w/ map
  (define directly-buildable?
    (and (dict-ref defined-table 'get-cons)
         (dict-ref defined-table 'get-null)))
  (define builder
    (if directly-buildable?
        ;; directly buildable, its own builder
        s
        ;; has an external builder struct
        ;; no size hint, ever
        (get-builder s)))
  (define cons (get-cons builder))
  (define null (get-null builder))
  (define res
    (let loop ([s (sequence->stream s)])
      (cond [(stream-empty? s)    null]
            [(f (stream-first s)) (cons (stream-first s)
                                        (loop (stream-rest s)))]
            [else                 (loop (stream-rest s))])))
  (if directly-buildable? res (finalize-builder res)))

;; TODO need append too. what else? stream-rest already returns the right
;;  type (i.e. not an opaque stream), rest (for-each, fold, andmap, ormap,
;;  etc.) are consumers, do not need the buildable part
;;   -> then maybe whatever library provides this should just reprovide
;;      sequence-fold, etc. under the naming convention we pick
;; TODO build-list / unfold? reverse? remove? sort? member? findf? x-ref?
;;  take? drop? add-between? remove-duplicates? filter-map? filter-not?
;;  shuffle? argmin/argmax? make-list?

(module+ test
  (require rackunit))

(struct kons (kar kdr) #:transparent
        #:methods gen:buildable
        [(define (get-cons x) (lambda (a d) (kons a d)))
         (define (get-null x) '())]
        #:methods gen:stream
        [(define (stream-empty? x) (empty? x))
         (define (stream-first  x) (kons-kar x))
         (define (stream-rest   x) (kons-kdr x))])

(module+ test
  (check-equal? (map/g add1 (kons 1 (kons 2 '())))
                (kons 2 (kons 3 '())))
  (check-equal? (filter/g even? (kons 1 (kons 2 '())))
                (kons 2 '())))

(struct vektor (v) #:transparent
        #:methods gen:buildable
        [(define (get-builder x [size-hint #f])
           (if size-hint
               (vektor-builder (make-vector size-hint) (sub1 size-hint))
               ;; no size hint, fall back to lists
               (vektor-list-builder 'useless))) ;; TODO maybe not the best interface
         (define (get-size-hint x)
           (vector-length (vektor-v x)))]
        #:property prop:sequence (lambda (x) (vektor-v x)))

(struct vektor-builder (v [i #:mutable]) #:transparent
        #:methods gen:buildable
        [(define (get-cons x)
           (lambda (a d)
             (define i (vektor-builder-i x))
             (vector-set! (vektor-builder-v x) i a)
             (set-vektor-builder-i! x (sub1 i))
             x))
         (define (get-null x)
           x)
         (define (finalize-builder x)
           (vektor (vektor-builder-v x)))])
(struct vektor-list-builder ([l #:mutable]) #:transparent
        #:methods gen:buildable
        [(define (get-cons x)
           (lambda (a d)
             (set-vektor-list-builder-l!
              d (cons a (vektor-list-builder-l d)))
             d))
         (define (get-null x) (vektor-list-builder '()))
         (define (finalize-builder x)
           (vektor (list->vector (vektor-list-builder-l x))))])


(module+ test
  (check-equal? (map/g add1 (vektor '#(1 2 3)))
                (vektor '#(2 3 4)))
  (check-equal? (filter/g even? (vektor '#(1 2 3)))
                (vektor '#(2))))
