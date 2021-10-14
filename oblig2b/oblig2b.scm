;; 1 Innkapsling, lokal tilstand og omgivelsesmodellen

;; (a)

;; Denne er så si stjålet fra forelesningsfoilene:
(define (make-counter)
  (let ((x 0))
    (lambda ()
      (set! x (+ x 1))
      x)))

(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))
(c1)
(c1)
(c1)
count
(c2)


;; (b)

;; [Tegning av diagrammer. Blergh]


;; 2 Innkapsling, lokal tilstand, og *message passing*

;; (a)

(define (make-stack elements)
  (define (push! args)
    (if (null? args)
      elements
      (begin (set! elements (cons (car args) elements))
             (push! (cdr args)))))
  (define (pop!)
    (if (null? elements)
      '()
      (let ((popped-element (car elements)))
        (set! elements (cdr elements))
        popped-element)))
  (define (stack) elements)
  (lambda (message . args)
    (cond ((eq? message 'push!) (push! args))
          ((eq? message 'pop!) (pop!))
          ((eq? message 'stack) (stack)))))

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'pop!)
(s1 'stack)
(s2 'pop!)
(s2 'push! 1 2 3 4)
(s2 'stack)
(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack)

;; (b)

(define (push! s . args)
  (define (pusher list)
    (if (null? list)
      (stack s)
      (begin (s 'push! (car list))
             (pusher (cdr list)))))
  (pusher args))
(define (pop! s)
  (s 'pop!))
(define (stack s)
  (s 'stack))

(pop! s1)
(stack s1)
(push! s1 'foo 'faa)
(stack s1)

;; 3 Strukturdeling og sirkulære lister

;; (a)
