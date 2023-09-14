;;;; lysys.lisp

(in-package #:lysys)

(defun apply-rules (sentence rules)
  (alexandria:flatten 
   (loop for u in sentence
	 collecting (if (assoc u rules)
			(cddr (assoc u rules)) 
			u))))

(defun iterate-rules-on-axiom (axiom rules depth)
  " Start with axiom, then apply the rules to it
over and over, until given depth is reached."
  (let ((sentence axiom))
    (loop for k from 1 to depth
	  do (setf sentence
		   (apply-rules sentence rules)))
    sentence))

;; pen-state...
;; 1 = pen was down when step was taken.
;; 0 = pen was up when step was taken.

(defun write-vertex (out state &key (pen-state 1))
  "Record the pen-state along with x and y."
  (format out "~a ~a ~a ~%" (first state) (second state) pen-state))

;; Start with F stepping from the origin in vertical direction (90.0).
;; This can be changed by initializing current-state differently.
;; current-state is a list (x y angle).
;;
;; The only sentence nodes that result in a write of state to file are:
;;
;; F
;; F*
;; s> (pop)

(defun parse-sentence-and-write-vertices (sentence angle filename)
  "After parsing all the sentence nodes, return the number 
of vertices that were written to file."
  (let ((current-state (list 0.0 0.0 (deg-angle 90.0)))
	(stack nil))
    (with-open-file
	(out filename :direction :output :if-exists :supersede)
      (write-vertex out current-state) 
      (loop for u in sentence
	    with vertices = 0
	    do (case u
		 (F (progn
		      (incf (first current-state)
			    (cos (third current-state)))
		      (incf (second current-state)
			    (sin (third current-state)))
		      (write-vertex out current-state)
		      (incf vertices)))
		 (F* (progn
		       (incf (first current-state)
			     (cos (third current-state)))
		       (incf (second current-state)
			     (sin (third current-state)))
		       (write-vertex out current-state :pen-state 0)
		       (incf vertices)))
		 (+ (incf (third current-state) angle))
		 (- (decf (third current-state) angle))
		 (s< (push (copy-seq current-state) stack))
		 (s> (progn (setf current-state (pop stack))
			    (write-vertex out current-state :pen-state 0))))
	    finally (return vertices)))))

;; The returned number of vertices will help you to
;; decide if you want to render with Asymptote.
;; Too small and it's not that interesting.
;; Too big and it will take forever.

(defun compute-l-system (l-system depth &key (filename "path-vertices.dat"))
  "Example: (compute-l-system 'pentadendrite 5)"
  (destructuring-bind (&key axiom rules angle)
      (symbol-plist l-system)
    (let ((sentence (iterate-rules-on-axiom axiom rules depth)))
       (parse-sentence-and-write-vertices sentence angle filename))))

;; Assuming default filename, now use the Asymptote script...
;;
;; $ asy -V -fpdf render-path.asy

