; 1. PAR OG LISTER
  
; (a)
; 
;      +---+---+     +----+
; ---->| * | *-+---->| 11 |
;      +-|-+---+     +----+
;        |
;        V
;      +----+
;      | 42 |
;      +----+
  
; (b)
; 
;      +---+---+     +---+
; ---->| * | *-+---->| / |
;      +-|-+---+     +---+
;        |
;        V
;      +----+
;      | 42 |
;      +----+
  
; (c)
; 
;      +---+---+     +---+---+     +---+
; ---->| * | *-+---->| * | *-+---->| / |
;      +-|-+---+     +-|-+---+     +---+
;        |             |
;        V             V
;      +----+        +----+
;      | 42 |        | 11 |
;      +----+        +----+
  
; (d)
; 
;      +---+---+     +---+---+     +---+
; ---->| * | *-+---->| * | *-+---->| / |
;      +-|-+---+     +-|-+---+     +---+
;        |             |
;        V             V
;      +----+        +---+---+     +---+---+     +---+
;      | 42 |        | * | *-+---->| * | *-+---->| / |
;      +----+        +-|-+---+     +-|-+---+     +---+
;                      |             |
;                      V             V
;                    +----+        +----+
;                    | 11 |        | 12 |
;                    +----+        +----+
  
; (e)
; 
;      +---+---+     +---+---+     +---+---+     +---+---+    
; ---->| * | *-+---->| * | *-+---->| * | *-+---->| * | / |   
;      +-|-+---+     +-|-+---+     +-|-+---+     +-|-+---+   
;        |             |             |             |
;        |             V             V             V
;        |           +---+         +---+         +---+
;        |           | 1 |         | 2 |         | 3 |
;        |           +---+         +---+         +---+
;        V
;      +---+---+     +---+---+     +---+---+
;      | * | *-+---->| * | *-+---->| * | / |
;      +-|-+---+     +-|-+---+     +-|-+---+
;        |             |             |
;        V             V             V
;      +---+         +---+         +---+
;      | 1 |         | 2 |         | 3 |
;      +---+         +---+         +---+

; (f)

(car (cdr '(0 42 #t bar)))

; (g)

(car (cdr (car '((0 42) (#t bar)))))

; (h)

(car (car (cdr '((0) (42 #t) (bar)))))

; (i)

(define bar 'bar) ; for at REPLet ikke skal klage

(cons (cons 0 (cons 42 '())) (cons (cons #t (cons bar '())) '()))

(list (list 0 42) (list #t bar))


; 2. REKURSJON OVER LISTER OG HØYREORDENSPROSEDYRER

; (a)

(define (take n elements)
  (cond ((null? elements) '())
        ((= n 0) '())
        (else (cons (car elements) (take (- n 1) (cdr elements))))))

(take 3 '(a b c d e f))
(take 1 '(a b c d e f))
(take 4 '(a b))
(take 4 '())

; (b)


(define (take n elements)
  (define (take-iter count taken rest)
    (cond ((null? rest) taken)
          ((= count 0) taken)
          (else (take-iter (- count 1)
                           (append taken (list (car rest))) ; Stygt. Fiks!
                           (cdr rest)))))
  (take-iter n '() elements))

(take 3 '(a b c d e f))
(take 1 '(a b c d e f))
(take 4 '(a b))
(take 4 '())
