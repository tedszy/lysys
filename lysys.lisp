;;;; lysys.lisp

(in-package #:lysys)

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

;; Current state needs to store pen-up/pen-down.
;; When stack is popped we dont draw a line, so
;; pop to pen-up. This should resolve stack problem.

(defun write-out (out state joined)
  (format out "~a ~a ~a ~%" (first state) (second state) joined))

(defun parse-sentence-and-write-path (sentence angle filename)
  (let ((current-state (list 0.0 0.0 0.0))
	(stack nil))
    (with-open-file
	(out filename :direction :output :if-exists :supersede)
      (write-out out current-state 1)
      (loop for u in sentence
	    with k = 0
	    do (progn
		 (case u
		   (F (progn
			(incf (first current-state)
			      (cos (third current-state)))
			(incf (second current-state)
			      (sin (third current-state)))
			(write-out out current-state 1)
			(incf k)))
		   (F* (progn
			(incf (first current-state)
			      (cos (third current-state)))
			(incf (second current-state)
			      (sin (third current-state)))
			(write-out out current-state 0)
			(incf k)))
		   (+ (incf (third current-state) angle))
		   (- (decf (third current-state) angle))
		   (s< (push (copy-seq current-state) stack))
		   (s> (progn (setf current-state (pop stack))
			      (write-out out current-state 0)))
		   (r~ (incf (third current-state) 180.0))))
	    finally (return k)))))

(defun run-l-system (l-system n &optional (filename "path-vertices.dat"))
  (destructuring-bind (&key axiom rules angle)
      (symbol-plist l-system)
    (let ((sentence (compute-sentence-level axiom rules n)))
       (parse-sentence-and-write-path sentence angle filename))))
