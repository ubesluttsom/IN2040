;; Oppgave 1 --- Innkapsling, lokal tilstand og omgivelsesmodellen
;;
;; (a) Denne er så si stjålet fra forelesningsfoilene.

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


;; (b)      +-------------------------------------------------+
;;          | make-counter: ...                               |
;;          | counter: 42                                     |
;; global ->| c2: --------------------+                       |
;; env      | c1: --+                 |                       |
;;          +-------|-----------------|-----------------------+
;;                  |          ^      |          ^
;;                  |          |      |          |
;;                  |       +------+  |       +------+
;;                  |  E1 ->| x: 3 |  |  E2 ->| x: 1 |
;;                  |       +------+  |       +-------
;;                  V          ^      V          ^
;;              .---.---.      |  .---.---.      |
;;              | O | O-+------+  | O | O-+------+
;;              `-|-^---'         `-|-^---'
;;                | +---------------+
;;                V V
;;         body: (lambda ()
;;                 (set! x (+ x 1))
;;                 x)))


;; Oppgave 2 --- Innkapsling, lokal tilstand, og *message passing*
;;
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


;; Oppgave 3 --- Strukturdeling og sirkulære lister
;;
;; (a)

(define bar (list 'a 'b 'c 'd 'e))

;;      +---+---+     +---+---+     +---+---+     +---+---+     +---+--/+
;; ---->| * | *-+---->| * | *-+---->| * | *-+---->| * | *-+---->| * | / |
;;      +-|-+---+     +-|-+---+     +-|-+---+     +-|-+---+     +-|-+/--+
;;        |             |             |             |             |
;;        V             V             V             V             V
;;      +---+         +---+         +---+         +---+         +---+
;;      | a |         | b |         | c |         | d |         | e |
;;      +---+         +---+         +---+         +---+         +---+

(set-cdr! (cdddr bar) (cdr bar))

;;                      +-------------------------------+
;;                      |                               |
;;                      V                               |
;;      +---+---+     +---+---+     +---+---+     +---+-|-+
;; ---->| * | *-+---->| * | *-+---->| * | *-+---->| * | * |
;;      +-|-+---+     +-|-+---+     +-|-+---+     +-|-+---+
;;        |             |             |             |  
;;        V             V             V             V  
;;      +---+         +---+         +---+         +---+
;;      | a |         | b |         | c |         | d |
;;      +---+         +---+         +---+         +---+

(list-ref bar 0) ;; →  a
(list-ref bar 3) ;; →  d
(list-ref bar 4) ;; →  b
(list-ref bar 5) ;; →  c

;; Vi får disse verdiene ettersom listen har blitt syklisk; etter tredje
;; element, forsetter vi å telle fra andre element i løkke.


;; (b)

(define bah (list 'bring 'a 'towel))

;;      +---+---+     +---+---+     +---+--/+
;; ---->| * | *-+---->| * | *-+---->| * | / |
;;      +-|-+---+     +-|-+---+     +-|-+/--+
;;        |             |             |
;;        V             V             V
;;      +-------+     +---+         +-------+
;;      | bring |     | a |         | towel |
;;      +-------+     +---+         +-------+

(set-car! bah (cdr bah))

;;        +-------------+
;;        |             |
;;        |             V
;;      +-|-+---+     +---+---+     +---+--/+
;; ---->| * | *-+---->| * | *-+---->| * | / |
;;      +---+---+     +-|-+---+     +-|-+/--+
;;                      |             |
;;                      V             V
;;                    +---+         +-------+
;;                    | a |         | towel |
;;                    +---+         +-------+

bah                     ;; →  ((a towel) a towel)
(set-car! (car bah) 42) ;;
bah                     ;; →  ((42 towel) 42 towel)

;; `caar` til `bah` refererer til akkurat samme `a` som `cadr` til `bah`, de er
;; aliaser for hverandre. Så når det siste kallet på `set-car!` endrer verdien
;; til `(caar bah)` til 42, er dette ekvivalent med å endre verdien til `(cadr
;; bah)`, og vi får resultetet over.


;; (c)

;; Bruker skilpadde-og-hare alogritmen:
;; <https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_tortoise_and_hare>

(define (cycle? list)
  (define (find-repetition tortoise hare)
      (cond ((null? hare) #f)
            ((null? (cdr hare)) #f)
            ((eq? tortoise hare) #t)
            (else (find-repetition (cdr tortoise) (cddr hare)))))
  (if (pair? list)
      (find-repetition list (cdr list))
      #f))

(cycle? '(hey ho))   ;; →  #f
(cycle? '(la la la)) ;; →  #f
(cycle? bah)         ;; →  #f
(cycle? bar)         ;; →  #t


;; (d)

(list? bar) ;; →  #t
(list? bah) ;; →  #f

;; Fra R5RS spesifikasjonen er en liste definert slik:
;;
;; > Pairs are used primarily to represent lists. A list can be defined
;; > recursively as either the empty list or a pair whose cdr is a list. More
;; > precisely, the set of lists is defined as the smallest set X such that
;; >  - The empty list is in X.
;; >  - If *list* is in X, then any pair whose cdr field contains *list* is
;; >    also in X.
;;
;; Altså, for at noe skal være en liste må den innholde et par som har den
;; tomme lista i sitt cdr. Dette er ikke tilfellet for `bah`, eller andre
;; sykliske “lister”.
