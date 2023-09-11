#lang racket

(require "etapa2.rkt")
(require "etapa3.rkt")

(provide (all-defined-out))

;; Preferințele bărbaților și femeilor din problemă se pot schimba
;; în timp, dar de obicei ele nu se schimbă radical de la un moment
;; la altul. De aceea, în loc să rulăm de la zero algoritmul
;; Gale-Shapley de fiecare dată când se schimbă ceva, preferăm să
;; pornim de la lista de logodne stabile obținută în pasul anterior
;; și să o actualizăm, conform algoritmului următor:
;; - eliminăm din engagements cuplurile care au devenit instabile
;;   în urma modificărilor de preferințe
;;   - cuplurile rămase sunt stabile între ele și considerăm că
;;     se găsesc împreună într-o cameră, în timp ce membrii cuplurilor
;;     destrămate stau la coadă la intrarea în cameră
;; - cât timp coada nu este goală
;;   - prima persoană p din coadă intră în cameră și încearcă să se
;;     cupleze cu cineva care este deja acolo, astfel:
;;     - p-list = lista de preferințe a lui p
;;     - determină prima persoană p' din p-list care este în cameră
;;     - dacă p' nu e logodită, logodește p' cu p
;;     - dacă p' e logodită
;;       - dacă p' îl preferă pe p partenerului actual p''
;;         - logodește p' cu p
;;         - încearcă să îl cuplezi pe p'' cu altcineva din cameră
;;           (folosind același algoritm)
;;       - altfel, treci la următoarea persoană din p-list (dacă
;;         aceasta există, altfel p rămâne temporar fără partener)


; TODO 1
; Implementați funcția match care primește o persoană person care
; intră în cameră, lista engagements a cuplurilor din cameră
; (cuplurile având pe prima poziție persoanele de gen opus lui 
; person), o listă pref1 care conține preferințele celor de același 
; gen cu person, o listă pref2 cu preferințele celor de gen diferit, 
; respectiv o coadă queue a persoanelor din afara camerei,
; și întoarce lista de cupluri actualizată astfel încât noile
; cupluri să fie stabile între ele.
; Această listă se obține ca rezultat al încercării de a cupla pe
; person cu cineva din cameră (person va încerca în ordine persoanele 
; din lista sa de preferințe), care poate duce la destrămarea
; unui cuplu și necesitatea de a cupla noua persoană rămasă singură
; cu altcineva din cameră, etc. Procesul continuă până când:
; - ori avem numai cupluri stabile între ele în cameră, nimeni
;   nefiind singur
; - ori toate persoanele rămase singure nu ar fi preferate de nimeni
;   altcineva din cameră, și în acest caz convenim să "logodim"
;   aceste persoane cu valoarea #f, astfel încât funcția să
;   întoarcă în aceeași listă atât informația despre cine din
;   cameră este logodit, cât și despre cine este singur
(define (match person engagements pref1 pref2 queue)
        (let for-pref ((pref-person (get-pref-list pref1 person))
                       (persoana person)
                       (acc engagements)
                       )
          (if (null? pref-person)
             acc
               (let* ((preferat (car pref-person))
                     (partener-pt-pref (get-partner acc preferat))
                             )
                 (if (member preferat queue)
                      (if (null? (cdr pref-person))
                             (for-pref (cdr pref-person) persoana (append acc (list (cons #f persoana))))
                             (for-pref (cdr pref-person) persoana acc)
                             )
                      (if (preferable? (get-pref-list pref2 preferat) persoana partener-pt-pref)
                         (for-pref  (get-pref-list pref1 partener-pt-pref) partener-pt-pref (update-engagements acc preferat persoana))
                         (if (null? (cdr pref-person))
                             (for-pref (cdr pref-person) persoana (append acc (list (cons #f persoana))))
                             (for-pref (cdr pref-person) persoana acc)
                             )
                         )
                      )
                 )
               )
          )
  )
  
  
              


; TODO 2
; Implementați funcția path-to-stability care primește lista
; engagements a cuplurilor din cameră, o listă de preferințe 
; masculine mpref, o listă de preferințe feminine wpref, respectiv
; coada queue a persoanelor din afara camerei, și întoarce lista
; completă de logodne stabile, obținută după ce fiecare persoană
; din queue este introdusă pe rând în cameră și supusă procesului
; descris de funcția match.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; - persoanele nelogodite din cameră apar în engagements sub forma
;   (#f . nume-bărbat) sau (nume-femeie . #f)



;; situatie in care am o persoana si logodne care au pe prima pozitie persoane de acelasi gen cu person
;; (la 1 stiam ca am pe prima pozitie a logodnelor persoane de gen opus cu person)
;; pentru asta am folosit o functie de inversare a membrilor din fiecare pereche a listei (functia este folosita si in etapa 3 ex1)
(define (match2 person engagements pref1 pref2 queue)
  (let ((engagements2 (reverse (foldl (lambda (pereche acc)
                                      (cons (cons (cdr pereche) (car pereche)) acc))
                                    '()
                                    engagements))))
        (let for-pref ((pref-person (get-pref-list pref1 person))
                       (persoana person)
                       (acc engagements2)
                       )
          (if (null? pref-person)
              (reverse (foldl (lambda (pereche acumulator)                                      ;; lista rezultata trebuie inversata de asemenea
                                                                                                ;; pt a avea pe prima poz din pereche tot femeie
                                      (cons (cons (cdr pereche) (car pereche)) acumulator))
                                    '()
                                    acc))
               (let* ((preferat (car pref-person))
                     (partener-pt-pref (get-partner acc preferat))
                             )
                 (if (member preferat queue)
                      (if (null? (cdr pref-person))
                             (for-pref (cdr pref-person) persoana (append acc (list (cons #f persoana))))
                             (for-pref (cdr pref-person) persoana acc)
                             )
                      (if (preferable? (get-pref-list pref2 preferat) persoana partener-pt-pref)
                         (for-pref  (get-pref-list pref1 partener-pt-pref) partener-pt-pref (update-engagements acc preferat persoana))
                         (if (null? (cdr pref-person))
                             (for-pref (cdr pref-person) persoana (append acc (list (cons #f persoana))))
                             (for-pref (cdr pref-person) persoana acc)
                             )
                         )
                      )
                 )
               )
          )
    )
  )
  
  



(define (path-to-stability engagements mpref wpref queue)
  (let for-queue ((rez engagements)
                  (lista queue)
                  )
    (if (null? lista)
        rez
        (let ((persoana (car lista)))
          (if (member persoana (get-men mpref))                        ;; daca persoana este barbat se aplica primul match, altfel al doilea
            (for-queue  (match persoana rez mpref wpref lista) (cdr lista))
            (for-queue  (match2 persoana rez wpref mpref lista) (cdr lista))
            )
          )
        )
    )
  )


; TODO 3
; Implementați funcția update-stable-match care primește o listă 
; completă de logodne engagements (soluția anterioară), o listă de 
; preferințe masculine mpref și o listă de preferințe feminine wpref 
; (adică preferințele modificate față de cele pe baza cărora s-a 
; obținut soluția engagements), și calculează o nouă listă de logodne 
; stabile - conform cu noile preferințe, astfel:
; - unstable = cuplurile instabile din engagements
; - room-engagements = engagements - unstable
; - queue = persoanele din unstable
; - aplică algoritmul path-to-stability
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
(define (update-stable-match engagements mpref wpref)
  (let ((lista_c_i  (get-unstable-couples engagements mpref wpref))
    )
    (define COADA (foldr (λ (pereche coada)                                ;; formez coada cu persoanele care formeaza cupluri instabile
                (append (list (car pereche) (cdr pereche)) coada)
             ) '() lista_c_i))
      (let ((engagements-nou (filter (λ (pair)                             ;; scot cuplurile instabile din engagments
                                       (not (member pair lista_c_i))
                                     ) engagements))
            )
        (path-to-stability engagements-nou mpref wpref COADA)              ;; aplic task2 pentru a stabiliza cuplusrile
      )
    )
  )
  


; TODO 4
; Implementați funcția build-stable-matches-stream care primește
; un flux pref-stream de instanțe SMP și întoarce fluxul de 
; soluții SMP corespunzător acestor instanțe.
; O instanță SMP este o pereche cu punct între o listă de preferințe
; masculine și o listă de preferințe feminine.
; Fluxul rezultat se va obține în felul următor:
; - primul element se calculează prin aplicarea algoritmului
;   Gale-Shapley asupra primei instanțe
; - următoarele elemente se obțin prin actualizarea soluției
;   anterioare conform algoritmului implementat în etapa 4 a temei
; Trebuie să lucrați cu interfața pentru fluxuri. Dacă rezolvați
; problema folosind liste și doar convertiți în/din fluxuri,
; punctajul pe acest exercițiu se anulează în totalitate.

(define (build-stable-matches-stream pref-stream)
  (if (stream-empty? pref-stream)
      empty-stream
      (stream-cons (gale-shapley (first (stream-first pref-stream)) (rest (stream-first pref-stream)))
               (build-stable-matches-stream (stream-rest pref-stream))
               )
      )
  )


       
      


