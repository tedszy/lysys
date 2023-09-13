;;;; lysys.lisp

(in-package #:lysys)

;; Current state (x, y, angle).
(defparameter *current-state* (list 0.0 0.0 0.0))
(defparameter *path* nil)
(defparameter *stack* nil)

(defparameter *axiom* '(F + F + F + F))

(defparameter *rule* '(F -> F + F - F - F F + F + F - F))

;; New one!
;; (defparameter *rule* '(F -> F + F - F F - F F + F + F - F))

(defparameter *angle* (* (/ pi 180.0) 90.0))
(defparameter *angle-increment* (* (/ pi 180.0) 10.0))

;; Rule substitution.
;; Sentences begin as axioms and grow larger
;; by applications of substitution rules.
;; After substituting, we flatten the sentence.

(defun apply-substitution (sentence)
  (alexandria:flatten
   (loop for u in sentence
	 collecting (if (eq u (first *rule*))
			(cddr *rule*) 
			u))))

(defun compute-level (n)
  (let ((sentence *axiom*))
    (loop for k from 1 to n
	  do (setf sentence
		   (apply-substitution sentence)))
    sentence))

(defun sentence-to-string (sentence)
  (format nil "~{~A~}" sentence))

;; Walk through the sentence and write the path
;; to file immediately. No need to keep it in
;; memory as a huge list.

(defun write-path (sentence filename)
  (let ((current-state (list 0.0 0.0 0.0)))
    (with-open-file
	(out filename :direction :output :if-exists :supersede)
    
      (format out "~a ~a ~a~%"
      	      (first current-state)
      	      (second current-state)
      	      (* 180 (/ pi) (third current-state)))

      (loop for u in sentence
	    do (progn
		 (case u

		   (F (progn
			(incf (first current-state)
			      (cos (third current-state)))
			(incf (second current-state)
			      (sin (third current-state)))
			(format out "~a ~a ~a~%"
			 (first current-state)
			 (second current-state)
			 (* 180 (/ pi) (third current-state)))))
		   
		   (+ (incf (third current-state) *angle*))

		   (- (decf (third current-state) *angle*))))))))
