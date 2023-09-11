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
  (map first mpref)
  )


; TODO 2
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți foldl sau foldr, astfel încât să nu fie necesare
; operații de tip append sau reverse.
(define (get-women wpref)
  (reverse(foldl (λ (x acc)
           (append (list (car x)) acc)
           ) null wpref))
  )


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
  (foldl (λ (x acc)
           (if (equal? (car x) person)
                       (append (cdr x) acc)
                       acc
                       )
           )
           null pref)
)         


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
  (ormap (λ (a)
           (if (or (equal? x #f) (equal? y #f))
               #t
               (if (equal? a x)
                   (if (member y (list-tail pref-list (add1 (index-of pref-list a))))
                       #t
                       #f
                       )
                   #f
                   )
           ) pref-list
          )
  )
  )


 






  
  
  
                           
 

; TODO 5
; Implementați recursiv funcționala find-first, care primește
; un predicat și o listă și întoarce primul element al listei
; care satisface predicatul, sau false dacă un asemenea element
; nu există.
; Implementarea trebuie să fie eficientă în sensul că nu trebuie
; să continue explorarea listei odată ce s-a găsit elementul.
(define (find-first p L)
  (if (null? L)
      #f
      (if (p (car L))
          (car L)
          (find-first p (cdr L))
          )
      )
  )


; TODO 6
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți find-first, fără să îl apelați de 2 ori (hint: define în define).
(define (get-partner engagements person)
  (define result (find-first (λ (a)
                (if (equal? (car a) person)
                    (cdr a)
                    #f
                    )
                ) engagements))
  (if result
      (cdr result)
      #f
      )
  )
  

; TODO 7
; Implementați recursiv funcționala change-first care primește
; un predicat p, o listă L și o valoare val, și întoarce o nouă 
; listă în care primul element din L care satisface predicatul p
; a fost înlocuit cu valoarea val, celelalte rămânând la fel.
; Dacă niciun element din L nu satisface predicatul, lista L
; rămâne neschimbată.

(define (replace lst idx new-elem)
      (if (empty? lst)
          lst
          (if (= idx 0)
              (cons new-elem (cdr lst))
              (cons (car lst) (replace (cdr lst) (- idx 1) new-elem)))))
  

(define (change-first p L val)
  (define rezultat (find-first p L))
  (if  rezultat
       (replace L (index-of L rezultat) val)
       L
       )
      
)

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
   (define R(change-first  (λ (a)
                   (if (equal? (car a) p1)
                       #t
                       #f
                       )
                  ) engagements p2))

  (reverse
   (foldl (λ (elem R-final)
            (if (pair? elem)
                (cons elem R-final)
                (cons (cons p1 elem) R-final)))
          '()
          R))
  )

  
  
  
  
  
   
             


; TODO
; Copiați implementarea funcției better-match-exists? din etapa 1.
; Funcția nu este repunctată de checker, dar este necesară pentru
; implementarea funcției stable-match? de mai jos.
; Dacă nu ați implementat better-match-exists? în etapa 1, solicitați 
; o rezolvare de la asistent, astfel încât să puteți continua.
(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (if (null? p1-list)
      #f
  (if (preferable? (get-pref-list pref2 (car p1-list)) (get-partner engagements (car p1-list)) p1)
      (better-match-exists? p1 p2 (cdr p1-list) pref2 engagements)
       (if (equal? (get-partner engagements (car p1-list)) p1)
           #f
           #t
    
      ))))


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

(define (f_ajutatoare lista)
  (if (null? lista)
      #t
      (if (not(car lista))
          (f_ajutatoare (cdr lista))
          #f
          )
      )
  )
                        

  
(define (stable-match? engagements mpref wpref)
 (define r (map (λ (x)
           (better-match-exists? (cdr x) (car x) (get-pref-list mpref (cdr x)) wpref engagements)
           ) engagements))
  (f_ajutatoare r)
  )
   
                       

  
  
  
               
               
  
  

