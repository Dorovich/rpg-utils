;;; DATOS DE MOUSE GUARD

(define mg-data (make-hash-table 2))

;; Definiciones

(define (make-animal name nature aspects desc)
  (methods
    ((get-name)
     name)
    ((get-nature)
     nature)
    ((get-aspects)
     aspects)
    ((get-desc)
     desc)
    ((print)
     (format #t "- ~a -~%~a~%~%Naturaleza ~a ~d~%~{~a~^, ~}~%"
	     name desc name nature aspects))))

(define (add-animal name nature aspects desc)
  (hash-set! mg-data name (make-animal name nature aspects desc)))

;; Instancias en la tabla

(add-animal "Coyote" 8
	    '("Inteligente" "Adaptable" "Impredecible" "Tenaz")
	    "Más allá de las Fronteras Olfativas acecha un gran múmero de animales peligrosos. Motivados por la escasez durente el largo invierno, a veces los coyotes hacen incursiones hasta los Territorios en busca de comida.
	    Los coyotes son más grandes que los zorros, pero no tan grandes como los lobos. Son omnívoros muy astutos, que lo mismo devorarán una ciudad llena de ratones que se contentarán con agarrar uno o dos ratones extraviados.")

(add-animal "Abeja" 2
	    '("Zumbar" "Recoger Polen" "Moverse en Enjambre")
	    "Las abejas habitan dentro de sus colmenas, de cuyo interior los apicultores recogen miel y cera.")

;; Final

(set! last-loaded-data mg-data)
