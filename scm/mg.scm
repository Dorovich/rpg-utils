;;; DATOS DE MOUSE GUARD

(define mg-data (make-hash-table 2))

;; Definiciones

(define (make-animal name nature aspects desc)
  (methods
    ((get-name)
     name)
    ((get-nature)
     nature)
    ((set-nature nat)
     (format #t "Naturaleza modificada: ~a -> ~a~%" nature nat)
     ;; esto modifica el valor de naturaleza global del animal :(
     (set! nature (string->number nat)))
    ((describe)
     (format #t "[ ~a ]~%~a~%"
	     name desc)
     (newline)
     (stats))
    ((stats)
     (format #t "Naturaleza (~a) ~d~%~{~a~^, ~}~%"
	     name nature aspects))))

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

(add-animal "Liebre" 6
	    '("Correr" "Esconderse" "Forrajear")
	    "Las liebres son un tipo de conejo grande. Durante el verano su pelaje es marrón oscuro, pero en invierno lo muda por otro de un brillante blanco, con sólo la punta de las orejas de colo negro. Las liebres están dotadas de inteligencia y son pacíficas, pero no llevan prácticamente nada de ropa y no usan herramientas o armas.
De vez en cuando las liebres y los ratones se alían. Las liebres permiten a los ratones usarlas como monturas, y a cambio éstos les proporcionan comida o productos manufacturados.

No todos los ratones entienden a las liebres. Estas criaturas hablan suavemente y con pocas palabras. De hecho, muchos ratones ni siquiera se dan cuenta de que las liebres hablan. Aquellos que se toman el tiempo de escucharlas descubren que las liebres son espartanas, estoicas y reservadas. Algunos guardianes han recibido el entrenamiento necesario para trabajar con liebres.

Estos ratones han fabricado arreos y arneses que fijan a los cuerpos de las liebres para montar sobre ellas o usarlas para remolar objetos. No es fácil montar en liebre; a menos que un ratón haya sido entrenado en cómo dirigirse a ellas, las liebres no le escucharán y mucho menos dejarán que las monte.")

;; Final

(set! last-loaded-data mg-data)
