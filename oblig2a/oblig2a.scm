;; 1 DIVERSE

;; (a)

(define (p-cons x y)
  (lambda (proc) (proc x y)))

;; Jeg skjønte ikke helt om det var meningen at vi ikke skulle bruke de
;; innebyggde `cons`, `car` eller `cdr` i det hele tatt? Må innrømme at det
;; hadde jeg problemer med (foruten å bruke `list` eller no', men det er jo
;; egentlig det samme).

(define (p-car x)
  (car (x cons)))

(define (p-cdr y)
  (cdr (y cons)))

(p-cons "foo" "bar")
(p-car (p-cons "foo" "bar"))
(p-cdr (p-cons "foo" "bar"))
(p-car (p-cdr (p-cons "zoo" (p-cons "foo" "bar"))))

;; (b)

(define foo 42)

;;  ? (let ((foo 5)
;;          (x foo))
;;      (if (= x foo)
;;        'same
;;        'different))
;; →  different

((lambda (foo x)
   (if (= x foo)
    'same
    'different)) 5 foo)

;;  ? (let ((bar foo)
;;          (baz 'towel))
;;      (let ((bar (list bar baz))
;;            (foo baz))
;;        (list foo bar)))
;; →  (towel (42 towel))

((lambda (bar baz)
   ((lambda (bar foo)
     (list foo bar))
    (list bar baz) baz))
 foo 'towel)

;; (c)

(define (infix-eval x-op-y)
  ((cadr x-op-y) (car x-op-y) (caddr x-op-y)))

(define foo (list 21 + 21))
(define baz (list 21 list 21))
(define bar (list 84 / 2))
(infix-eval foo)
(infix-eval baz)
(infix-eval bar)

;; (d)

;;  ? (define bah '(84 / 2))
;;  ? (infix-eval bah)
;; →  error!
;;
;; Vi får en error, siden `/` blir tolket bokstavelig, og ikke som et
;; funksjonskall.


;; 2 HUFFMAN-KODING

(load "huffman.scm")

;; (a)

;; Gjorde dette også med en `append` konstruksjon, men ser penere ut (men
;; kanskje ikke like effektivt?) med å bare bruke `reverse` på
;; `akk`(umulatoren) på slutten.

(define (decode bits tree)
  (define (decode-1 bits current-branch akk)
    (if (null? bits)
      (reverse akk)
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch) 
          (decode-1 (cdr bits) tree (cons (symbol-leaf next-branch) akk))
          (decode-1 (cdr bits) next-branch akk)))))
  (decode-1 bits tree '()))

;; (b)

(decode sample-code sample-tree)
;; →  (samurais fight ninjas by night)

;; (c)

(define (encode symbols tree)
  (define (encode-1 symbols current-branch code bits)
    (cond ((null? symbols) code)
          ((leaf? current-branch)
           (if (eq? (symbol-leaf current-branch) (car symbols))
             (encode-1 (cdr symbols) tree (append code bits) '())
             #false))
          (else (or (encode-1 symbols (left-branch current-branch) code
                              (append bits (list '0)))
                    (encode-1 symbols (right-branch current-branch) code
                              (append bits (list '1)))))))
  (encode-1 symbols tree '() '()))

(encode (decode sample-code sample-tree) sample-tree)
(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)

;; (d)

;;  ? (define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
;;  ? (define codebook (grow-huffman-tree freqs))
;;  ? (decode (encode '(a b c) codebook) codebook)
;; →  (a b c)

;; Flau over hvor lang tid det tokk meg å finne ut av denne ...

(define (grow-huffman-tree freqs)
  (define (merge leafs)
    (if (null? (cdr leafs)) leafs
      (if (<= (weight (car leafs)) (weight (cadr leafs)))
        (merge (append (list (make-code-tree (car leafs)
                                             (cadr leafs)))
                       (cddr leafs)))
        (merge (adjoin-set (car leafs) (cdr leafs))))))
  (merge (make-leaf-set freqs)))

(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(decode (encode '(a b c) codebook) codebook)

;; (e)

(define freqs '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3) (in 2)
                (ambush 2) (defeat 1) (the 5) (sword 4) (by 12) (assassin 1)
                (river 2) (forest 1) (wait 1) (poison 1)))
(define codebook (grow-huffman-tree freqs))

;; →  (((((leaf night 12) (((leaf hide 3) ((leaf defeat 1) (leaf river 2)
;;    (defeat river) 3) (hide defe at river) 6) ((leaf sword 4) ((leaf ambush
;;    2) (leaf in 2) (ambush in) 4) (sword ambush in) 8) (h ide defeat river
;;    sword ambush in) 14) (night hide defeat river sword ambush in) 26) ((leaf
;;    ninja s 20) (((((leaf poison 1) (leaf wait 1) (poison wait) 2) ((leaf
;;    forest 1) (leaf assassin 1) (for est assassin) 2) (poison wait forest
;;    assassin) 4) (leaf the 5) (poison wait forest assassin the) 9) (leaf by
;;    12) (poison wait forest assassin the by) 21) (ninjas poison wait forest
;;    assassin th e by) 41) (night hide defeat river sword ambush in ninjas
;;    poison wait forest assassin the by) 67) ((leaf fight 45) (leaf samurais
;;    57) (fight samurais) 102) (night hide defeat river sword ambus h in
;;    ninjas poison wait forest assassin the by fight samurais) 169))

(define message '(ninjas fight ninjas fight ninjas ninjas fight samurais
                  samurais fight samurais fight ninjas ninjas fight by night))

(encode message codebook)

;; →  (0 0 1 0 0 1 0 0 0 1
;;     0 0 1 0 0 0 1 0 0 0
;;     1 0 0 1 0 0 1 1 0 1
;;     1 0 1 0 0 1 1 0 1 0
;;     0 0 1 0 0 0 1 0 0 1
;;     0 0 0 1 1 1 0 0 0 0)

;; Altså, totalt 60 bit.

;; Koden under gir oss gjennomsnittslengden på hvert kodeord i besjeden: ~1.77.

(define (encode-with-codebook message) (encode (list message) codebook))
(define code-as-list (map encode-with-codebook message))
(define symbol-lengths (map length code-as-list))
(/ (apply + symbol-lengths) (length symbol-lengths))

;; →  30/17

;; Jeg lager en ny frekvensliste, hvor alle symboler har samme frekvens, da
;; vil Huffman-treet bli et jevnt fordelt binærtre. Resultatet fra kjøringen
;; gjør svaret på minste antall bits ved fast lengde opplagt: 5.

(define freqs '((samurais 1) (ninjas 1) (fight 1) (night 1) (hide 1) (in 1)
                (ambush 1) (defeat 1) (the 1) (sword 1) (by 1) (assassin 1)
                (river 1) (forest 1) (wait 1) (poison 1)))
(define codebook (grow-huffman-tree freqs))
(define code-as-list (map encode-with-codebook message))
(define symbol-lengths (map length code-as-list))
symbol-lengths

;; →  (5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5)

;; (f)

;; BLANK
