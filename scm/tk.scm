;;; DATOS DE TROIKA!

(define tk-data (make-hash-table 1))

;; Definiciones

(define* (make-background name desc posessions skills #:optional special)
  (methods
    ((get-name)
     name)
    ((describe)
     (format #t "[ ~a ]~%~a~%~%Posesiones~%~{- ~a~%~}~%Habilidades%~{- ~a~%~}~%Especial~%- ~a~%"
	     name
	     desc
	     (let ((l '()))
	       )
	     )
     )))

;; Instancias en la tabla

;; Final

(set! last-loaded-data tk-data)
