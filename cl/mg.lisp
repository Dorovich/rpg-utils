;;; DATOS DE MOUSE GUARD

(defparameter *mg-data* (make-hash-table :test 'equalp))

;; Definiciones

(defclass animal ()
  ((name
     :initarg :name
     :type string
     :accessor mg/animal.name)
   (nature
     :initarg :nature
     :type fixnum
     :accessor mg/animal.nature)
   (aspects
     :initarg :aspects
     :type list
     :accessor mg/animal.aspects)
   (description
     :initarg :desc
     :type string
     :accessor mg/animal.desc)))

(defun add-animal (name nature aspects description)
  (setf (gethash name *mg-data*)
	(make-instance 'animal
		       :name name
		       :nature nature
		       :aspects aspects
		       :desc description)))

(defmethod print-thing ((thing animal))
  (format t "- ~A -~%~A~%~%Naturaleza ~A ~D~%~{~A~^, ~}~%"
	  (mg/animal.name thing)
	  (mg/animal.desc thing)
	  (mg/animal.name thing)
	  (mg/animal.nature thing)
	  (mg/animal.aspects thing)))

;; Instancias en la tabla

(add-animal "Coyote" 8 '("Inteligente" "Adaptable" "Impredecible" "Tenaz")
	    "Más allá de las Fronteras Olfativas acecha un gran múmero de animales peligrosos. Motivados por la escasez durente el largo invierno, a veces los coyotes hacen incursiones hasta los Territorios en busca de comida.
	    Los coyotes son más grandes que los zorros, pero no tan grandes como los lobos. Son omnívoros muy astutos, que lo mismo devorarán una ciudad llena de ratones que se contentarán con agarrar uno o dos ratones extraviados.")

(add-animal "Abeja" 2 '("Zumbar" "Recoger Polen" "Moverse en Enjambre")
	    "Las abejas habitan dentro de sus colmenas, de cuyo interior los apicultores recogen miel y cera.")

;; Final

(setf *last-loaded-data* *mg-data*)
