(use-modules (ice-9 format)
	     (ice-9 readline))

(set! *random-state* (random-state-from-platform))

;; Funciones de utilidad

(define in-repl #f)
(define loaded-games (make-hash-table 3))
(define last-loaded-data #f)
(define active-things (make-hash-table))

(define (string-split char-delimiter? string)
  (define (maybe-add a b parts)
    (if (= a b)
      parts
      (cons (substring string a b) parts)))
  (let ((n (string-length string)))
    (let loop ((a 0) (b 0) (parts '()))
      (if (< b n)
	(if (not (char-delimiter? (string-ref string b)))
	  (loop a (+ b 1) parts)
	  (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
	(reverse (maybe-add a b parts))))))

(define (is-d? char)
  (if (eq? char #\d) #t #f))

(define (require-game game)
  (let ((file (string-append game ".scm")))
    (if (not (hash-ref loaded-games game))
      (if (file-exists? file)
	(begin
	  (load file)
	  (hash-set! loaded-games game last-loaded-data)
	  #t)
	(begin
	  (format #t "[!] No se han podido cargar los datos del juego ~a: Achivo \"~a\" no encontrado.~%" game file)
	  #f))
      #t)))

(define-syntax-rule (methods ((method-id method-args ...)
			      body ...) ...)
		    (lambda (method . args)
		      (letrec ((method-id
				 (lambda (method-args ...)
				   body ...)) ...)
			(cond
			  ((eq? method (quote method-id))
			   (apply method-id args)) ...
			  (else
			    (error "No such method:" method))))))

;; Funciones de interfaz

(define (rpg/repl)
  (let ((cmd (readline "> ")))
    (when (and (not (eof-object? cmd))
	       (string<> "exit" cmd))
      (main (string-split char-whitespace? cmd))
      (newline)
      (rpg/repl))))

(define (rpg/repl-startup)
  (if (not in-repl)
    (begin
      (set! in-repl #t)
      (with-exception-handler
	(lambda (e) (exit 1))
	(rpg/repl)))
    (format #t "[!] El REPL ya está activo.~%")))

(define (rpg/roll expr)
  (let* ((nums (string-split is-d? expr))
	 (times (string->number (car nums)))
	 (faces (string->number (cadr nums)))
	 (result 0))
    (if (and times faces)
      (begin
	(do ((i 1 (1+ i)))
	  ((> i times))
	  (set! result (+ result (random faces))))
	(format #t "~dd~d => ~d~%" times faces result))
      (format #t "[!] Expresión de los dados malformada.~%"))))

(define (rpg/coin times)
  (let ((times (string->number times))
	(heads 0)
	(tails 0))
    (if times
      (begin
	(do ((i 1 (1+ i)))
	  ((> i times))
	  (if (= 0 (random 2))
	    (set! tails (1+ tails))
	    (set! heads (1+ heads))))
	(format #t "~d monedas => ~d caras, ~d cruces~%" times heads tails))
      (format #t "[!] Expresión cantidad malformada.~%"))))

(define (rpg/info game topic)
  (when (require-game game)
    (let* ((game-data (hash-ref loaded-games game))
	   (thing (hash-ref game-data topic)))
      (thing 'describe))))

(define (rpg/new game topic identifier)
  (if (and in-repl (require-game game))
    (begin
      (let* ((game-data (hash-ref loaded-games game))
	     (thing (hash-ref game-data topic)))
	(hash-set! active-things identifier thing))
      (format #t "Nueva instancia de ~a creada (~a).~%" topic identifier))
    (format #t "[!] Las instancias se eliminan cuando el programa finaliza. Para trabajar con ellas usa el REPL: ./rpg repl~%")))

(define (rpg/do action identifier)
  (if in-repl
    (let ((thing (hash-ref active-things identifier)))
      (thing (string->symbol action)))
    (format #t "[!] Las instancias se eliminan cuando el programa finaliza. Para trabajar con ellas usa el REPL: ./rpg repl~%")))

;; Funciones principales

(define valid-commands `(("repl" ,rpg/repl-startup . 0)
			 ("roll" ,rpg/roll . 1)
			 ("coin" ,rpg/coin . 1)
			 ("info" ,rpg/info . 2)
			 ("new" ,rpg/new . 3)
			 ("do" ,rpg/do . 2)))

(define (main args)
  (if (not in-repl)
    (set! args (cdr args)))
  (if (null? args)
    (rpg/repl-startup)
    (let* ((cmd (car args))
	   (entry (assoc-ref valid-commands cmd)))
      (if entry
	(begin
	  (let ((func (car entry))
		(nargs (cdr entry)))
	    (if (= nargs (length (cdr args)))
	      (apply func (cdr args))
	      (format #t "[!] Número de argumentos incorrecto.~%"))))
	(format #t "[!] El comando ~a no existe.~%" cmd)))))
