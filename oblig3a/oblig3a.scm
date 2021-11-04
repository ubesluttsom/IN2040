(load "prekode3a.scm")

;; 1. PROSEDYRER FOR BEDRE PROSEDYRER

;; (a) – (b)

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


;; (c)

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


;; 2 STRØMMER

;; (a)

(define (list-to-stream list)
  (cons-stream (car list) (list-to-stream (cdr list))))

(define (stream-to-list stream . n)
  (define (iter stream n)
    (cond ((= n 0) '())
          ((null? (cdr stream)) '())
          (else (begin 
                       (display "!!!\n")
                       (display (stream-car stream))
                       (newline)
                       (display (stream-cdr stream))
                       (newline)
                       (display (null? (stream-cdr stream)))
                       (newline)
                       (cons (stream-car stream) (iter (stream-cdr stream) (- n 1)))))))
  (if (null? n)
      (iter stream -1)
      (iter stream (car n))))

(show-stream (list-to-stream '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)))
(stream-to-list (list-to-stream '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)))
