;;;; lysys.lisp

(in-package #:lysys)

(defun deg-angle (deg) (* (/ pi 180.0) deg))

;; Database of L-systems using symbol plists.

(defmacro define-l-system (sym &key axiom rules angle)
  `(setf (symbol-plist ',sym)
	 (list :axiom ',axiom
	  :rules ',rules
	  :angle (deg-angle ,angle))))

(define-l-system rings
  :axiom (F + F + F + F)
  :rules ((F -> F F + F + F + F + F + F - F))
  :angle 90.0)

(define-l-system quadratic-koch-1
  :axiom (F + F + F + F)
  :rules ((F -> F + F - F - F F + F + F - F))
  :angle 90.0)

(define-l-system tatami
  :axiom (F + F + F + F)
  :rules ((F -> F F + F - F + F + F F))
  :angle 90.0)

(define-l-system square-sierpinski
  :axiom (F + X F + F + X F)
  :rules ((X -> X F - F + F - X F + F + X F - F + F - X))
  :angle 90.0)

(define-l-system hilbert
  :axiom (X)
  :rules ((X -> - Y F + X F X + F Y -)
	  (Y -> + X F - Y F Y - F X +))
  :angle 90.0)

(define-l-system sierpinski-triangle
  :axiom (Y F)
  :rules ((X -> Y F + X F + Y)
	  (Y -> X F - Y F - X))
  :angle 60.0)

(define-l-system hex-gosper
  :axiom (X F)
  :rules ((X -> X + Y F + + Y F - F X - - F X F X - Y F +)
	  (Y -> - F X + Y F Y F + + Y F + F X - - F X - Y))
  :angle 60.0)

(define-l-system levy-curve
  :axiom (F)
  :rules ((F -> - F + + F -))
  :angle 45.0)

(define-l-system anklet
  :axiom (- X - - X)
  :rules ((X -> X F X - - X F X))
  :angle 45.0)

(define-l-system fern
  :axiom (X)
  :rules ((F -> F F)
	  (X -> F - pu pu X po + X po  + F pu + F X po - X))
  :angle 22.5)

(define-l-system fern2
  :axiom (F)
  :rules ((F -> F F + pu + F - F - F po - pu - F + F + F po))
  :angle 22.5)



(defun apply-substitution (sentence rules)
  (alexandria:flatten 
   (loop for u in sentence
	 collecting (if (assoc u rules)
			(cddr (assoc u rules)) 
			u))))

(defun compute-sentence-level (axiom rules n)
  (let ((sentence axiom))
    (loop for k from 1 to n
	  do (setf sentence
		   (apply-substitution sentence rules)))
    sentence))

(defun sentence-to-string (sentence)
  (format nil "~{~A~}" sentence))

(defun level-n-string (l-system n)
  (sentence-to-string
   (compute-sentence-level (get l-system :axiom)
			   (get l-system :rules) n)))

;; No need to keep the path vertices in memory.
;; Just write them out one by one.

(defun write-out (out state)
  (format out "~a ~a~%" (first state) (second state)))

(defun parse-sentence-and-write-path (sentence angle filename)
  (let ((current-state (list 0.0 0.0 0.0))
	(stack nil))
    (with-open-file
	(out filename :direction :output :if-exists :supersede)
      (write-out out current-state)
      (loop for u in sentence
	    do (progn
		 (case u
		   (F (progn
			(incf (first current-state)
			      (cos (third current-state)))
			(incf (second current-state)
			      (sin (third current-state)))
			(write-out out current-state)))
		   (+ (incf (third current-state) angle))
		   (- (decf (third current-state) angle))
		   (pu (push (copy-seq current-state) stack))
		   (po (setf current-state (pop stack)))))))))

(defun run-l-system (l-system n &optional (filename "path-vertices.dat"))
  (destructuring-bind (&key axiom rules angle)
      (symbol-plist l-system)
    (let ((sentence (compute-sentence-level axiom rules n)))
      (parse-sentence-and-write-path sentence angle filename))))
