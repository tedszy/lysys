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

(defun write-out (out state)
  (format out "~a ~a~%" (first state) (second state)))

(defun parse-sentence-and-write-path (sentence angle filename)
  (let ((current-state (list 0.0 0.0 0.0))
	(stack nil))
    (with-open-file
	(out filename :direction :output :if-exists :supersede)
      (write-out out current-state)
      (loop for u in sentence
	    with k = 0
	    do (progn
		 (case u
		   (F (progn
			(incf (first current-state)
			      (cos (third current-state)))
			(incf (second current-state)
			      (sin (third current-state)))
			(write-out out current-state)
			(incf k)))
		   (+ (incf (third current-state) angle))
		   (- (decf (third current-state) angle))
		   (< (push (copy-seq current-state) stack))
		   (> (setf current-state (pop stack)))
		   (r (incf (third current-state) 180.0))))
	    finally (return k)))))

(defun run-l-system (l-system n &optional (filename "path-vertices.dat"))
  (destructuring-bind (&key axiom rules angle)
      (symbol-plist l-system)
    (let ((sentence (compute-sentence-level axiom rules n)))
       (parse-sentence-and-write-path sentence angle filename))))
