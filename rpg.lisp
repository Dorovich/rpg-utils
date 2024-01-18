(require :uiop)

(defun delimiter-split (delimiter sequence
                                 &key (keep-delimiters nil)
                                 &aux (end (length sequence)))
  (loop for start = 0 then (1+ pos)
        for pos   = (position delimiter sequence :start start)
        ; no more delimiter found
        when (and (null pos) (not (= start end)))
        collect (subseq sequence start)
        ; while delimiter found
        while pos
        ;  some content found
        when (> pos start) collect (subseq sequence start pos)
        ;  optionally keep delimiter
        when keep-delimiters collect (subseq sequence pos (1+ pos))))

;; Funciones de interfaz

(defun roll (expr)
  (let* ((nums (delimiter-split #\d expr))
         (times (parse-integer (car nums)))
         (faces (parse-integer (cadr nums)))
         (result 0))
    (setf *random-state* (make-random-state t))
    (loop repeat times
          do (incf result (+ 1 (random faces))))
    (format t "~Dd~D => ~D~%" times faces result)))

(defparameter *valid-commands* '(("roll" . roll)
                                ("r" . roll)))

;; Funciones principales

(defun main (args)
  (when (null args) (return-from main))
  (let ((f (cdr (assoc (car args) *valid-commands* :test #'string=))))
    (if f
        (apply f (cdr args))
      (format t "Ese comando no existe.~%"))))

(main uiop:*command-line-arguments*)
