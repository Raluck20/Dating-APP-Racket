#lang racket

(provide (all-defined-out))

; ATENȚIE - Veți avea de reimplementat mai multe funcții
; din etapa 1, însă cu un alt mod de rezolvare (folosind
; funcționale sau alte funcții solicitate în enunț).
; Enunțul acestor exerciții va debuta prin "Ca în etapa 1,
; dar este interzisă recursivitatea explicită".


; TODO 1
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosiți orice funcțională exceptând foldl/foldr.
(define (get-men mpref)
  (map (append (lambda (x) (car x))) mpref))  ; fct anonima care intoarce primu el al fiecarei subliste din mpref si creeaza o lista din acestea adica lista barbatilor
; am fol map ca sa aplicam asta pe fiecare el din mpref


; TODO 2
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți foldl sau foldr, astfel încât să nu fie necesare
; operații de tip append sau reverse.
(define (get-women wpref)
   (foldr (lambda (x y) (cons (car x) y)) '() wpref)) ; y - lista goala, punem in y primu el de pe fiecare subsir din wpref, prelucrand elem de la dreapta la stanga
  


; TODO 3
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Folosiți minim o funcțională și minim o funcție anonimă.
(define (get-pref-list pref person)
  (foldr (lambda (p acc)                   ; cu foldr vom evalua fiecare el din pref
           ; fct lambda primeste primu subsir din pref si verifica daca primu el din subsir e egal cu person. daca da, va pune restul subsirului in acumulator
           (if (equal? person (car p))
               (append (cdr p) acc)
               acc))
         '() pref))
; TODO 4
; Ca în etapa 1, dar este interzisă recursivitatea explicită
; și sunt permiși operatorii condiționali:
; Implementați o funcție care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Folosiți funcția member.
(define (preferable? pref-list x y)
  (if (member x (member y pref-list))
      #f
      #t))


; TODO 5
; Implementați recursiv funcționala find-first, care primește
; un predicat și o listă și întoarce primul element al listei
; care satisface predicatul, sau false dacă un asemenea element
; nu există.
; Implementarea trebuie să fie eficientă în sensul că nu trebuie
; să continue explorarea listei odată ce s-a găsit elementul.
(define (find-first p L)
  (cond ((null? L) #f)
        ((p (car L)) (car L))  ; daca primu el din losta satisface p, intoarce el
        (else (find-first p (cdr L))))) ; recursivitate

; TODO 6
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți find-first, fără să îl apelați de 2 ori (hint: define în define).
(define (get-partner engagements person)
  ((lambda (partner-pair)
     (if partner-pair
         (if (equal? (car partner-pair) person) ; cat tp avem el in partner-pair ver daca primu el din part-pair = person.daca da intoarce restu listei, daca nu intoarce primu el
             (cdr partner-pair)
             (car partner-pair))
         #f))
   (find-first                     ; lambda cauta prima pereche din eng care respecta conditia data in if
    (lambda (p)
      (if (equal? person (car p))
          p
          #f))
    engagements)))


; TODO 7
; Implementați recursiv funcționala change-first care primește
; un predicat p, o listă L și o valoare val, și întoarce o nouă 
; listă în care primul element din L care satisface predicatul p
; a fost înlocuit cu valoarea val, celelalte rămânând la fel.
; Dacă niciun element din L nu satisface predicatul, lista L
; rămâne neschimbată.
(define (change-first p L val)
  (cond ((null? L) '())
        ((p (car L)) (cons val (cdr L)))   ; verifică dacă primul element al listei L satisface condiția dată de funcția p. Dacă da, înlocuiește primul element cu val și returnează lista modificată.
        (else (cons (car L) (change-first p (cdr L) val)))))    ;recursivitate trece la urm el


; TODO 8
; Implementați funcția update-engagements care primește o listă de
; logodne engagements și două persoane p1 și p2, și întoarce lista
; actualizată de logodne, în care vechiul partener al lui p1 este
; înlocuit cu p2.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - p1 era logodită în prealabil
; - fiecare cuplu din lista engagements are pe prima poziție
;   persoanele de același gen cu p1
; Folosiți change-first.
(define (update-engagements engagements p1 p2) 
  (change-first (lambda (p) (equal? p1 (car p))) ;lambda verifica daca p1 este egal cu primu el din logodna p
                engagements 
                (cons p1 p2)))


; TODO
; Copiați implementarea funcției better-match-exists? din etapa 1.
; Funcția nu este repunctată de checker, dar este necesară pentru
; implementarea funcției stable-match? de mai jos.
; Dacă nu ați implementat better-match-exists? în etapa 1, solicitați 
; o rezolvare de la asistent, astfel încât să puteți continua.
(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (cond
    ((or (null? p1-list) (equal? (car p1-list) p2)) #f)
    ((and (preferable? p1-list(car p1-list) p2) (preferable? (get-pref-list pref2 (car p1-list)) p1 (get-partner engagements (car p1-list))))#t)
    (else(better-match-exists? p1 p2 (cdr p1-list) pref2 engagements)))
    )


; TODO 9
; Implementați funcția stable-match? care primește o listă 
; completă de logodne engagements, o listă de preferințe masculine 
; mpref și o listă de preferințe feminine wpref, și întoarce true 
; dacă toate cuplurile din engagements sunt stabile.
; Un cuplu este stabil dacă pentru niciunul din membrii cuplului
; nu există un alt partener mai potrivit (conform definiției de
; la funcția better-match-exists?).
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
(define (stable-match? engagements mpref wpref)
  (not (find-first( lambda (pair)    ;functia find-first e utilizata pt a gasi prima pereche din eng care satisface conditia , ne trb not pt ca stable match e inversul lui better-match
                     ; 1- facem better match de femeie barbat lista pref femeii mpref si engagementurile inversate (aplicam pe toata lista de eng inversarea partenerilor intre ei)
                     ; 2- facem better match de barbat femeie lista pref barbati wpref si in cazul acesta lasam en asa cum sunt
                     ;-daca nu sunt indeplinite cond #f
                     ;mergem recursiv pe toate eng
       (cond
         ((better-match-exists? (car pair) (cdr pair) (get-pref-list wpref (car pair)) mpref (map (lambda (x) (cons (cdr x) (car x))) engagements))#t)
         ((better-match-exists? (cdr pair) (car pair) (get-pref-list mpref (cdr pair)) wpref engagements)#t)
         (else #f))) engagements))
       )