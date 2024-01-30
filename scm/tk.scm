;;; DATOS DE TROIKA!

(define tk-data (make-hash-table 1))

;; Definiciones

(define default-posessions '(("2d6" "Silver Pence")
			     (1 . "Knife (DMG 2, 2, 2, 2, 4, 8, 10)")
			     (1 . "Lantern & flask of oil")
			     (1 . "Rucksack")
			     (6 . "Provisions")))

(define (make-background name desc posessions skills special)
  (methods
    ((get-name)
     name)
    ((describe)
     (format #t "[ ~a ]~%~a~%~%Posesiones~%~{~{- ~a ~a~%~}~}~%Habilidades~%~{~{- ~a ~a~%~}~}~:[~;~%Especial~%- ~a~%~]"
	     name desc posessions skills special))))

(define* (add-background name desc posessions skills #:optional special)
  (hash-set! tk-data name (make-background name desc posessions skills special)))

;; Instancias en la tabla

(add-background "Claviger"
		"The Key Masters wander the universe fathoming the workings of all entryways. Though they’re quite fascinated with simple chests and doors they are most excited by metaphysical and metaphorical barriers. You might find small conclaves of Clavigers camped around the feet of Demon Gates, debating appropriate methods of attack, or building obscure machines of entry."
		'((1 "Festooned with Keys (2 armor, 4 slots)")
		  (1 "Distinguished Sledgehammer (DMG 1, 2, 3, 6, 12, 13, 14). Ignores 1 point of Armour. Requires at least two hands to use.")
		  (1 "Lock picking tools"))
		'((4 "Locks")
		  (3 "Strength")
		  (3 "Trapping")
		  (2 "Spell - Open")
		  (1 "Spell - See Through")
		  (1 "Maul Fighting")
		  (1 "Spell - Lock")))

;; Final

(set! last-loaded-data tk-data)
