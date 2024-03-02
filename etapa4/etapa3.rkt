#lang racket

(require "etapa2.rkt")


(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.
(define (get-unstable-couples engagements mpref wpref)
  (let ((unstable-couples '()))
    (map (lambda (pair) (let* ((a (car pair)) (d (cdr pair)) (topman (get-pref-list wpref a)) (topwoman (get-pref-list mpref d))) ; topman lista de pref a lui a, topwoman lista de pref a lui d
           (cond ((better-match-exists? a d topman mpref (map (lambda (x) (let* ((xd (cdr x)) (xa (car x))) (cons xd xa))) engagements)) ;daca exista un better match pentru a si d atunci acestia sunt adaugati in ustable couples
               (set! unstable-couples (cons pair unstable-couples))
               )
           ((better-match-exists? d a topwoman wpref engagements)
               (set! unstable-couples (cons (cons a d) unstable-couples))
               ))))
         engagements)
    unstable-couples))


; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  (let merge ((burlaci free-men) (logodne engagements))
    (if (null? burlaci) logodne
         (let* ((m (car burlaci)) (rest-burlaci (cdr burlaci)) (pref-burlaci (get-pref-list mpref m)))
         (let cauta ((topw pref-burlaci))
              (if (null? topw) logodne
                    (let* ((w (car topw)) (topw-pref (get-pref-list wpref w)) (logodnic (get-partner logodne w)) (pair (cons w m)) (add-pair (cons pair logodne))
                                          (add-logodnic (cons logodnic rest-burlaci)) (update (update-engagements logodne w m)) (rest-topw (cdr  topw)))
                      (if (eq? logodnic #f) (merge rest-burlaci add-pair)
                            (if (preferable? topw-pref m logodnic)
                                (merge add-logodnic update)
                                (cauta rest-topw))))))))))



; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (map car mpref) '() mpref wpref))



; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (apply append (map (lambda (pair) (list (car pair) (cdr pair))) pair-list)))

