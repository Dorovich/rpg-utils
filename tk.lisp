;; DATOS DE TROIKA!

(defparameter *tk-data* (make-hash-table :test 'equalp))

;; Definiciones

(defclass background ()
  ((name
     :initarg :name
     :type string
     :accessor tk/bg.name)
   (description
     :initarg :desc
     :type string
     :accessor tk/bg.desc)
   (posessions
     :initarg :posessions
     :type list
     :accessor tk/bg.posessions)
   (skills
     :initarg :skills
     :type list
     :accessor tk/bg.skills)
   (special
     :initarg :special
     :type string
     :accessor tk/bg.special)))

(defun add-background (name description posessions skills special)
  (setf (gethash name *tk-data*)
	(make-instance 'background
		       :name name
		       :desc description
		       :posessions posessions
		       :skills skills
		       :special special)))


(defmethod print-thing ((thing background))
  (format t "[ ~A ]~%~A~%~%Posessions~%~{- ~A~%~}~%Skills~%~{- ~A~%~}~%Special~%- ~A~%"
	  (tk/bg.name thing)
	  (tk/bg.desc thing)
	  (loop for i in (tk/bg.posessions thing)
		collect (concatenate 'string
				     (if (numberp (car i))
				       (write-to-string (car i))
				       (car i))
				     " "
				     (cdr i)))
	  (loop for i in (tk/bg.skills thing)
		collect (concatenate 'string
				     (write-to-string (car i))
				     " "
				     (cdr i)))
	  (or (tk/bg.special thing) "None")))


;; Instancias en la tabla

(add-background "Necromancer"
		"The least popular of magical practitioners, Necromancers are shunned by the major centres of learning, left to their own devices on the edges of society, passing on knowledge in the time honoured master–student dynamic. This loneliness encourages students to make their own friends."
		'((1 . "Dusty robes")
		  (1 . "The skull of your master OR Zombie Servant OR ghost with whom you have developed a codependent relationship")
		  ("2d6" . "Silver pence"))
		'((2 . "Healing")
		  (2 . "Mortuary Science")
		  (2 . "Relationship Counseling")
		  (1 . "Spell - Posthumous Vitality")
		  (1 . "Spell - Posthumous Vitality")
		  (1 . "Spell - Torpor")
		  (1 . "Sneak"))
		nil)

;; Final

(setf *last-loaded-data* *tk-data*)
