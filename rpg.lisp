;; Funciones principales

(defparameter *valid-commands* '((("repl") . rpg/repl-startup)
                                 (("roll" "r") . rpg/roll)
                                 (("coin" "c") . rpg/coin)
                                 (("info" "i") . rpg/info)
                                 (("new" "n") . rpg/new)
                                 (("do" "d") . rpg/do)))

(defun main (args)
  (when (null args)
    (rpg/repl-startup)
    (return-from main))
  (let ((f (cdr (assoc (car args) *valid-commands*
                       :test (lambda (key item)
                               (member key item :test 'string=))))))
    (if f
        (apply f (cdr args))
      (format t "[!] El comando ~A no existe.~%" (car args)))))

;; Funciones de utilidad

(require :uiop)
(require :sb-posix)

(defparameter *loaded-games* (make-hash-table :test 'equal))
(defparameter *last-loaded-data* nil)

(defparameter *active-things* nil)
(defparameter *in-repl* nil)

(defgeneric print-thing (thing))

(defun require-game (game)
  "Carga el contenido del juego si no lo estaba ya."
  (let ((%f (concatenate 'string game ".lisp")))
    (if (null (gethash game *loaded-games*))
        (if (probe-file %f)
            (progn
              (load %f)
              (setf (gethash game *loaded-games*) *last-loaded-data*))
          (progn
            (format t "[!] No se han podido cargar los datos del juego ~A: Achivo ~A no encontrado.~%" game %f)
            nil))
      t)))

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

(defun rpg/repl ()
  "Invoca un REPL. Por si se quieren poner muchos comandos a la vez o trabajar con instancias."
  (let ((cmd (prompt-read ">")))
    (unless (string= cmd "exit")
      (main (delimiter-split #\Space cmd))
      (rpg/repl))))

(defun rpg/repl-startup (&rest rest)
  "Prepara el entorno del REPL y captura cualquier error o interrupción que haya mientras se ejecuta."
  ;; errores?
  (when rest
    (format t "[!] Demasiados argumentos.~%")
    (return-from rpg/repl-startup))
  ;; codigo
  (setf *in-repl* t)
  (setf *active-things* (make-hash-table :test 'equal))
  (handler-case
      (rpg/repl)
    (sb-sys:interactive-interrupt () (sb-ext:exit :code 1))
    (error (c)
           (format t "[!] Ha ocurrido un error:~%~A~%~%Saliendo.~%" c)
           (sb-ext:exit :code 1))))

(defun rpg/roll (&optional expr &rest rest)
  "Tirar dados según la expresión XdY (X = cantidad, Y = caras) y calcular la suma de los resultados."
  ;; errores?
  (when (null expr)
    (format t "[?] Esta función necesita un argumento: expresión de dados.~%")
    (return-from rpg/roll))
  (when rest
    (format t "[!] Demasiados argumentos.~%")
    (return-from rpg/roll))
  ;; codigo
  (let* ((nums (delimiter-split #\d expr))
         (times (parse-integer (car nums)))
         (faces (parse-integer (cadr nums)))
         (result 0))
    (setf *random-state* (make-random-state t))
    (loop repeat times
          do (incf result (+ 1 (random faces))))
    (format t "~Dd~D => ~D~%" times faces result)))

(defun rpg/coin (&optional times &rest rest)
  "Tirar X cantidad de monedas y hacer recuendo de la cantidad de caras y cruces."
  ;; errores?
  (when (null times)
    (format t "[?] Esta función necesita un argumento: número de monedas.~%")
    (return-from rpg/coin))
  (when rest
    (format t "[!] Demasiados argumentos.~%")
    (return-from rpg/coin))
  ;; codigo
  (let ((heads 0)
        (tails 0))
    (setf times (parse-integer times))
    (setf *random-state* (make-random-state t))
    (loop repeat times
          do (if (= (random 2) 0)
                 (incf tails)
               (incf heads)))
    (format t "~D monedas => ~D caras, ~D cruces~%" times heads tails)))

(defun rpg/info (&optional game topic &rest rest)
  "Buscar información sobre un tema concreto de un juego en concreto."
  ;; errores?
  (when (or (null game) (null topic))
    (format t "[?] Esta función necesita dos argumentos: nombre del juego, nombre del tema en concreto.~%")
    (return-from rpg/info))
  (unless (require-game game)
    (return-from rpg/info))
  ;; codigo
  (let* ((game-data (gethash game *loaded-games*))
         (thing (gethash topic game-data)))
    (print-thing thing)))

(defun rpg/new (&optional game topic identifier &rest rest)
  "Crear una instancia (con nuevo identificador) para usar en otros comandos. Para uso en el REPL."
  ;; errores?
  (unless *in-repl*
    (format t "[!] Las instancias se eliminan cuando el programa finaliza. Para trabajar con ellas usa el REPL: ./rpg repl~%")
    (return-from rpg/new))
  (when (or (null game) (null topic) (null identifier))
    (format t "[?] Esta función necesita tres argumentos: nombre del juego, nombre de la cosa en concreto, nombre identificativo exclusivo.~%")
    (return-from rpg/new))
  (when rest
    (format t "[!] Demasiados argumentos.~%")
    (return-from rpg/new))
  (unless (require-game game)
    (return-from rpg/new))
  ;; codigo
  (let* ((game-data (gethash game *loaded-games*))
         (thing (gethash topic game-data)))
    (setf (gethash identifier *active-things*) thing))
  (format t "Nueva intancia (~A) de ~A creada." identifier topic))

(defun rpg/do (&optional action identifier &rest rest)
  "Hacer una acción sobre una instancia. Para uso en el REPL."
  ;; errores?
  (unless *in-repl*
    (format t "[!] Las instancias se eliminan cuando el programa finaliza. Para trabajar con ellas usa el REPL: ./rpg repl~%")
    (return-from rpg/do))
  (when (or (null action) (null identifier))
    (format t "[?] Esta función necesita dos argumentos: acción a realizar, nombre identificativo exclusivo.~%")
    (return-from rpg/do))
  (when rest
    (format t "[!] Demasiados argumentos.~%")
    (return-from rpg/do))
  ;; codigo
  (if (string= action "print")
      (print-thing (gethash identifier *active-things*))))

;; Inicio del programa

(main uiop:*command-line-arguments*)
