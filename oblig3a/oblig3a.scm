(load "prekode3a.scm")

;; 1. PROSEDYRER FOR BEDRE PROSEDYRER

;; (a) – (b) {{{

(define mem
  (let ((unmemoized-procs (make-table)))
    (lambda (action proc)
        (cond ((eq? action 'memoize)
                (let ((memoized (let ((cache (make-table)))
                                  (lambda args
                                    (or (lookup args cache)
                                        (begin (insert! args (apply proc args) cache)
                                               (lookup args cache)))))))
                  (insert! memoized proc unmemoized-procs)
                  memoized))
                ((eq? action 'unmemoize)
                 (lookup proc unmemoized-procs))))))

;; Test

(fib 5)
(set! fib (mem 'memoize fib))
(fib 5)
(fib 5)
(set! fib (mem 'unmemoize fib))
(fib 5)
(test-proc 1 2 3)
(set! test-proc (mem 'memoize test-proc))
(test-proc 1 2 3)
(test-proc 1 2 3)

;; }}}

;; (c) {{{

(display "? (define mem-fib (mem 'memoize fib))\n")
(define mem-fib (mem 'memoize fib))
(display "? (mem-fib 3)\n")
(mem-fib 3)
(display "? (mem-fib 3)\n")
(mem-fib 3)
(display "? (mem-fib 2)\n")
(mem-fib 2)

;; I motsettning til hva jeg først trodde, forskjellen ligger *ikke* i at det
;; brukes `define` for å binde `mem-fib` prosedyren, og ikke `set!` som
;; tidligere. Forskjellen er heller (tror jeg) at vi lager en ny binding
;; `mem-fib`, i stedet for å endre på den gamle; da vil ikke `fib` bli
;; rekursivt omdefinert, og det gjøres kall på den «originale» `fib`
;; prossedyren innad i den memoiserte prosedyrekroppen, og ikke på
;; `memoized-proc` (som jeg har kallt den). Bruke
       (define fib (mem 'memoize fib))
       (fib 3)
       (fib 3)
       (fib 2)
;; vil fungere helt greit.

;; }}}

;; 2 STRØMMER

;; (a) {{{

(define (list-to-stream list)
  (if (null? list)
    '()                                           ; Basistillfellet.
    (cons-stream (car list)                       ; Ellers, cons sammen strøm
                 (list-to-stream (cdr list)))))   ; med rekkursjon

(define (stream-to-list stream . n)
  (define (iter stream n)
    (if (or (= n 0) (stream-null? stream))
      '()                                           ; Basistilfeller.
      (cons (stream-car stream)                     ; Ellers, lag liste med å
            (iter (stream-cdr stream) (- n 1)))))   ; rekkursivt conse bortover
  (if (null? n)
      (iter stream -1)          ; Hvis kun `stream` argument, forkast teller,
      (iter stream (car n))))   ; ellers, tolk ekstra argument `n` som teller

(list-to-stream '(1 2 3 4 5))
;; →  (1 . #<promise>)
(stream-to-list (stream-interval 10 20))
;; →  (10 11 12 13 14 15 16 17 18 19 20)
(show-stream nats 15)
;; →  1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ...

;; }}}

;; (b) {{{

(define (stream-take n stream)
  (list-to-stream (stream-to-list stream n)))   ; Modularitet <3

(define foo (stream-take 10 nats))
foo
;; →  (1 . #<promise>)
(show-stream foo 5)
;; →  1 2 3 4 5 ...
(show-stream foo 20)
;; →  1 2 3 4 5 6 7 8 9 10
(show-stream (stream-take 15 nats) 10)
;; →  1 2 3 4 5 6 7 8 9 10 ...

;; }}}


;; (c) {{{

;; Altså, de ville jo blitt ganske meningsløst, siden `memq` ville itterert seg
;; igjennom hele stømmen for hvert element i strømmen. Hvis du like vel må
;; evaluere hele strømmen for (nesten) hvert steg, hva er da poenget med å bruke
;; en strøm?

;; }}}


;; (d) {{{

(define (remove-duplicates stream)
  (define in-set?                             ; ←  Predikatet vi filterer etter
    (let ((set-of-elements (make-table)))     ; ←  Mengde som husker elementene
      (lambda (element)                       ; som et let-over-labda uttrykk.
        (if (lookup element set-of-elements)  ; Om elementet finnes i mengden,
          #f                                  ; er det et duplikat (filtrer),
          (begin (insert! element '() set-of-elements) ; ellers, legg til
                 element)))))                 ; og returner det (ikke filtrer).
  (if (stream-null? stream)
    the-empty-stream
    (cons-stream (in-set? (stream-car stream))
                 (remove-duplicates
                   (stream-filter in-set? (stream-cdr stream))))))

;; Test
(show-stream (remove-duplicates (list-to-stream '(1 52 7 3 2 2 78 7 9 10 11 66 66 52 3 4 2 2 3 1 1 1))))

;; }}}
