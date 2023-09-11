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
  (let ((l-schimbata (reverse (foldl (lambda (pereche acc)
                                      (cons (cons (cdr pereche) (car pereche)) acc))
                                    '()
                                    engagements))))
    (let parcurgere ((l1 engagements)
                     (l2 l-schimbata)
                     (ac null))
       (if (null? l1)
            ac
            (let* ((pereche (car l1))
                   (pereche-schimbata (car l2))
                   (p-f (car pereche))
                   (d-b (cdr pereche))
                   (p-b-2 (car pereche-schimbata))
                   (d-f-2 (cdr pereche-schimbata))
                   (rest_l1 (cdr l1))
                   (rest_l2 (cdr l2))
                   )
       
              (if (or (better-match-exists? d-b p-f (get-partner mpref d-b) wpref engagements)
                    (better-match-exists? d-f-2 p-b-2 (get-partner wpref d-f-2) mpref l-schimbata))
                  (parcurgere rest_l1 rest_l2 (append (list pereche) ac))
                  (parcurgere rest_l1 rest_l2 ac)))))))

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
  (let loop-man ((list-man free-men)
                 (acc engagements))
    (if (null? list-man)
        acc
        (let* ((man (car list-man))
               (pref-man (get-pref-list mpref man))
               (rest_list_m (cdr list-man))
               )
               (let loop-woman ((women-fav pref-man)
                                (ac acc)
                                )
                 (if (null? women-fav)
                     (loop-man (cdr list-man) ac)
                     (let* ((woman (car women-fav)))
                       (if (get-partner ac woman)
                           (if (preferable? (get-pref-list wpref woman) man (get-partner ac woman))
                               (loop-man (append rest_list_m (list  (get-partner ac woman) )) (update-engagements ac woman man))
                               (loop-woman (cdr women-fav) ac)
                               )
                           (loop-man rest_list_m (append (list (cons woman man)) ac))
                   
                    )
                )
            )
          )
        )
      )
    )
  )
  
  
  
              
        
; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (let ((lista_b (get-men mpref))
        (engagements null))
  (engage lista_b engagements mpref wpref)))
           
  
  


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldl (λ (a acc)
           (append (list (car a) (cdr a)) acc)
           ) null pair-list )
  )

