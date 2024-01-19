;; Funciones principales

(defparameter *valid-commands* '(("repl" . repl)
                                 ("roll" . roll)
                                 ("r" . roll)
                                 ("coin" . coin)
                                 ("c" . coin)
                                 ("info" . info)
                                 ("i" . info)
                                 ))

(defun main (args)
  (when (null args) (return-from main))
  (let ((f (cdr (assoc (car args) *valid-commands* :test #'string=))))
    (if f
        (apply f (cdr args))
      (format t "Ese comando no existe.~%"))))

;; Funciones de utilidad

(require :uiop)

(defun delimiter-split (delimiter sequence
                                  &key (keep-delimiters nil)
                                  &aux (end (length sequence)))
  (loop for start = 0 then (1+ pos)
        for pos   = (position delimiter sequence :start start)
        
        when (and (null pos) (not (= start end))) ; no more delimiter found
        collect (subseq sequence start)
        while pos ; while delimiter found
        when (> pos start) collect (subseq sequence start pos) ; some content found
        when keep-delimiters collect (subseq sequence pos (1+ pos)))) ; optionally keep delimiter

(defun prompt-read (&optional prompt)
  (when prompt
    (format *query-io* "~A " prompt)
    (force-output *query-io*))
  (read-line *query-io*))

;; Funciones de interfaz

(defun repl ()
  (let ((cmd (prompt-read ">")))
    (unless (string= cmd "exit")
      (main (delimiter-split #\Space cmd))
      (repl))))

(defun roll (&optional expr)
  (when (null expr)
    (format t "Esta función necesita un argumento: expresión de dados.~%")
    (return-from roll))
  (let* ((nums (delimiter-split #\d expr))
         (times (parse-integer (car nums)))
         (faces (parse-integer (cadr nums)))
         (result 0))
    (setf *random-state* (make-random-state t))
    (loop repeat times
          do (incf result (+ 1 (random faces))))
    (format t "~Dd~D => ~D~%" times faces result)))

(defun coin (&optional times)
  (when (null times)
    (format t "Esta función necesita un argumento: número de monedas.~%")
    (return-from coin))
  (let ((heads 0)
        (tails 0))
    (setf times (parse-integer times))
    (setf *random-state* (make-random-state t))
    (loop repeat times
          do (if (= (random 2) 0)
                 (incf tails)
               (incf heads)))
    (format t "~D monedas => ~D caras, ~D cruces~%" times heads tails)))

(defun info (&optional game type name)
  (when (or (null game) (null type) (null name))
    (format t "Esta función necesita tres argumentos: nombre del juego, clase de información, nombre concreto del tipo.~%")
    (return-from info))
  (format t "Buscando informacion sobre ~A ~A del juego ~A...~%" name type game))

;; Inicio del programa

(main uiop:*command-line-arguments*)
