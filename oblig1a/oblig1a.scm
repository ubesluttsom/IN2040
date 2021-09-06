;; OPPGAVE 1

;; (a)

(* (+ 4 2) 5)
;; Evalueres til 30.  `(+ 4 2)` blir 6, som vi setter inn og får `(* 6 5)`,
;; som blir 30.

;; (b)

(* (+ 4 2) (5))
;; Her blir `(5)` tolket som en prosedyre som heter 5, med ingen
;; argumenter. Dette gir en feilmelding, siden `5` ikke er definert som en
;; prosedyre.

;; (c)
(* (4 + 2) 5)

;; Samme problem som forrige deloppgave, her blir `4` forsøkt kallt som en
;; prosedyre med argmenter `+` (som er en prosedyre) og `5` (et tall), men
;; `4` er ikke definert slik, og gir dermed en feilmelding.

;; (d)

(define bar (/ 44 2))
bar
;; Het definerer vi en prosedyre `bar`, som deler 44 på 2. Deretter kalles
;; `bar` på neste linje og prosedyren utføres og evalueres til 22.

;; (e)

(- bar 11)
;; Prosedyren `-` kalles og trekker 11 fra `bar`. Siden `bar` blir evaluert
;; til 22, returneres 11. (Dette gitt at vi har definert `bar`
;; som i forrige deloppgave! Ellers, ville vi fått en feilmelding om at
;; `bar` ikke er definert.)

;; (f)

(/ (* bar 3 4 1) bar)
;; Antar `bar` er definert som i deloppgave (d). `bar`-ene evalueres til
;; `11`. Prosedyren `*` blir kallt med argumentene `11`, `3`, `4` og `1`,
;; som ganges sammen til `264`. Prosedyren `/` deler deretter `264` på
;; `11`, og returnerer `12`.


;; OPPGAVE 2

;; (a)

(or (= 1 2)
    "paff!"
    "piff!"
    (zero? (1 - 1)))
;; `or` returnerer den første sanne verdien i listen med uttrykk, som her
;; blir `"paff!"` (siden 1≠2). Alt som ikke eksplisitt er definert som
;; usant `#f`, er sant `#t`; strenger evalueres derfor som sant. I siste
;; kondisjonale utrykk blir det ikke brukt prefiksnotasjon, som ville gitt
;; en feilmelding (om at `1` ikke er en prosedyre).

(and (= 1 2)
     "paff!"
     "piff!"
     (zero? (1 - 1)))
;; `and` returnerer `#t` om argumentene evalueres til `#t`, men `#f`
;; ellers. Første uttrykk er usant (siden 1≠2), og dermed returneres `#f`
;; umiddelbart.

(if (positive? 42)
    "poff!"
    (i-am-undefined))
;; `if` tester om første argument er sant, hvis det er tilfellet evalueres
;; andre argument, ellers (hvis usant) tredje. I dette tilfellet,
;; `(positive? 42)` evalueres til `#t` (siden 42 er et positivt tall), og
;; andre argument, `"poff!"`, evalueres (og returneres).

;; `or`, `and` og `if` er *special forms*, siden alle argumentene ikke
;; nødvendigvis blir evaluert.

;; (b)

;; > Definer en prosedyre som heter `sign` som tar et tall som argument og
;; > returnerer `-1` dersom tallet er negativt, `1` hvis det er positivt,
;; > og `0` hvis tallet er null. Skriv to versjoner; én som bruker `if` og
;; > en som bruker `cond`.

(define (sign x)
  (if (> x 0) 1 
      (if (< x 0) -1 0)))

(define (sign x)
  (cond ((> x 0) 1)
        ((< x 0) -1)
        (else 0)))

;; (c)

;; > Prøv å skrive en versjon av sign fra oppgave (b) over som hverken
;; > bruker cond eller if men istedet bruker de logiske predikatene and og
;; > or.

(define (sign x)
  (or (and (> x 0) 1)
      (and (< x 0) -1)
      0))


;; OPPGAVE 3

;; (a)

;; > Skriv to prosedyrer `add1` og `sub1` som hver tar et tall som argument
;; > og returnerer resultatet av å henholdsvis legge til og trekke fra én.

(define (add1 x)
  (+ x 1))

(define (sub1 x)
  (- x 1))

;; (b)

;; > Basert på prosedyrene fra oppgaven over skal du nå definere en
;; > rekursiv prosedyre `plus` som tar to positive heltall som argumenter
;; > og returnerer resultatet av å legge dem sammen. Resultatet skal altså
;; > være det samme som om vi kalte den primitive prosedyren `+` på de to
;; > argumentene direkte, men her skal vi altså klare oss med kun `sub1` og
;; > `add1`. Husk at prosedyren din skal være rekursiv, dvs. at prosedyren
;; > kaller på seg selv i sin egen definisjon.

(define (pluss x y)
  (cond ((> x 0) (pluss (sub1 x) (add1 y)))
        ((< x 0) (pluss (add1 x) (sub1 y)))
        (else y)))

;; (c)

;; > I denne oppgaven skal vi forholde oss til skillet mellom prosedyre og
;; > prosess, og forskjellen mellom rekursive og iterative prosesser. Prøv
;; > å gi en analyse av den rekursive prosedyren du definerte i oppgave (b)
;; > over: Gir den opphav til en *iterativ* eller en *rekursiv* prosess?
;; > Forklar svaret ditt. Avhengig av hva slags type prosess den
;; > opprinnelige prosedyren din fører til, prøv å definere en ny variant
;; > som fører til den andre typen prosess.
;;
;; *Prosedyren* jeg definerte i forrige oppgave er *rekursiv* siden den
;; kaller på seg selv for å evaluere «neste steg» i prosedyren. *Prosessen* 
;; er også lineært iterativ, siden det ikke er noen andre, ventende, kall i
;; prosedyren etter det rekursive kallet returnerer (`cond` kjører kun den
;; første <klausen> hvor <testen> er sann, resten kjøres ikke).

(define (pluss x y)
  (cond ((> x 0) (add1 (pluss (sub1 x) y)))
        ((< x 0) (sub1 (pluss (add1 x) y)))
        (else y)))

;; I alternativet over vil vi ha et ventende kall på `add1` eller `sub1`
;; for hvert rekursive kall på `pluss`. Prossessen blir derfor lineært
;; rekursiv.

;; (d)

(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
        e
        (power-iter (+ 1 e))))
  (power-iter 1))

;; Kan fjerne `n` og `b` fra signaturen til `power-iter` siden de likevel
;; er innenfor samme skop når vi endrer til blokkstruktur, og de forblir
;; uendret ved hvert rekursive prosedyrekall.

;; (e)

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;; Vi kan ikke endre signaturen til `fib-iter` prosedyren uten videre,
;; siden alle argumentene enderer seg for hvert nye rekursive kall av
;; prosedyren.
