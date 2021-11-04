(load "prekode3a.scm")

;; 1. PROSEDYRER FOR BEDRE PROSEDYRER

;; (a), (b)

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

(display "--- 1 (a), (b) ---\n")
(set! fib (mem 'memoize fib))
(fib 3)
;; computing fib of 3
;; computing fib of 2
;; computing fib of 1
;; computing fib of 0
;; →   2
(fib 3)
;; →   2
(fib 2)
;; →   1
(fib 4)
;; computing fib of 4
;; →   3
(set! fib (mem 'unmemoize fib))
(fib 3)
;; computing fib of 3
;; computing fib of 2
;; computing fib of 1
;; computing fib of 0
;; computing fib of 1
;; →   2

(set! test-proc (mem 'memoize test-proc))
(test-proc)
;; computing test-proc of ()
;; →   0
(test-proc)
;; →   0
(test-proc 40 41 42 43 44)
;; computing test-proc of (40 41 42 43 44)
;; computing test-proc of (41 42 43 44)
;; computing test-proc of (42 43 44)
;; computing test-proc of (43 44)
;; computing test-proc of (44)
;; →   10
(test-proc 40 41 42 43 44)
;; →   10
(test-proc 42 43 44)
;; →   5


;; (c)

(display "--- 1 (c) ---\n")
(define mem-fib (mem 'memoize fib))
(mem-fib 3)
;; computing fib of 3
;; computing fib of 2
;; computing fib of 1
;; computing fib of 0
;; computing fib of 1
;; →  2
(mem-fib 3)
;; →  2
(mem-fib 2)
;; computing fib of 2
;; computing fib of 1
;; computing fib of 0
;; →  1

;; I motsetning til hva jeg først trodde, forskjellen ligger *ikke* i at det
;; brukes `define` for å binde `mem-fib` prosedyren, og ikke `set!` som
;; tidligere. Forskjellen er heller (tror jeg) at vi lager en ny binding
;; `mem-fib`, i stedet for å endre på den gamle; da vil ikke `fib` bli
;; rekursivt omdefinert, og det gjøres kall på den «originale» `fib`
;; prosedyren innad i den memoiserte prosedyrekroppen, og ikke på
;; `memoized-proc` (som jeg har kalt den). Bruke
       (define fib (mem 'memoize fib))
       (fib 3)
       (fib 3)
       (fib 2)
;; vil fungere helt greit.


;; 2. STRØMMER

;; (a)

(define (list-to-stream list)
  (if (null? list)
    '()                                           ; Basistilfellet.
    (cons-stream (car list)                       ; Ellers, cons sammen strøm
                 (list-to-stream (cdr list)))))   ; med rekursjon

(define (stream-to-list stream . n)
  (define (iter stream n)
    (if (or (= n 0) (stream-null? stream))
      '()                                           ; Basistilfeller.
      (cons (stream-car stream)                     ; Ellers, lag liste med å
            (iter (stream-cdr stream) (- n 1)))))   ; rekursivt conse bortover
  (if (null? n)
      (iter stream -1)          ; Hvis kun `stream` argument, forkast teller,
      (iter stream (car n))))   ; ellers, tolk ekstra argument `n` som teller

(display "--- 2 (a) ---\n")
(list-to-stream '(1 2 3 4 5))
;; →  (1 . #<promise>)
(stream-to-list (stream-interval 10 20))
;; →  (10 11 12 13 14 15 16 17 18 19 20)
(show-stream nats 15)
;; →  1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ...


;; (b)

(define (stream-take n stream)
  (list-to-stream (stream-to-list stream n)))   ; Modularitet <3

(display "--- 2 (b) ---\n")
(define foo (stream-take 10 nats))
foo
;; →  (1 . #<promise>)
(show-stream foo 5)
;; →  1 2 3 4 5 ...
(show-stream foo 20)
;; →  1 2 3 4 5 6 7 8 9 10
(show-stream (stream-take 15 nats) 10)
;; →  1 2 3 4 5 6 7 8 9 10 ...


;; (c)

;; Altså, de ville jo blitt ganske meningsløst, siden `memq` ville iterert seg
;; igjennom hele strømmen for hvert element i strømmen. Hvis du like vel må
;; evaluere hele strømmen for (nesten) hvert steg, hva er da poenget med å bruke
;; en strøm? Dessuten ville det ikke fungere på uendelige strømmer.


;; (d)

;; Idéen er å heller sjekke «bakover» i strømmen; sjekk elementene i strømmen
;; som allerede har blitt evaluert, og kun aksepter nye elementer som ikke
;; evaluerer til noen av de tidligere.

(define (remove-duplicates stream)
  (if (stream-null? stream)
    the-empty-stream
    (cons-stream (stream-car stream)
                 (remove-duplicates
                   (stream-filter
                       (lambda (element)
                         (not (memq element (list (stream-car stream)))))
                     (stream-cdr stream))))))

(display "--- 2 (c) ---\n")
(show-stream (remove-duplicates (list-to-stream '(2 1 3 115 3 1 1))))
(show-stream (remove-duplicates (list-to-stream '(1 1 1 1 1 1 1 1 1 1 1 1 1
                                                  1 1 1 1 1 1 1 1 1 1 1 1 1))))
(show-stream (remove-duplicates nats))
