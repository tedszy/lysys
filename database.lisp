;; database.lisp
;;
;; Database of L-systems using symbol plists.

(in-package :lysys)

(defun deg-angle (deg) (* (/ pi 180.0) deg))

(defmacro define-l-system (sym &key axiom rules angle)
  `(setf (symbol-plist ',sym)
	 (list :axiom ',axiom
	  :rules ',rules
	  :angle (deg-angle ,angle))))

(define-l-system rings
  :axiom (F + F + F + F)
  :rules ((F -> F F + F + F + F + F + F - F))
  :angle 90.0)

(define-l-system quadratic-koch
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
	  (X -> F - < < X > + X >  + F < + F X > - X))
  :angle 22.5)

(define-l-system fern2
  :axiom (F)
  :rules ((F -> F F + < + F - F - F > - < - F + F + F >))
  :angle 22.5)

(define-l-system thunder
  :axiom (F)
  :rules ((F -> F < + F F > < - F F > F < - F > < + F > F))
  :angle 35.0)

(define-l-system sticks
  :axiom (X)
  :rules ((F -> F F)
	  (X -> F < + X > F < - X > + X))
  :angle 20.0)

(define-l-system dragon
  :axiom (F X)
  :rules ((X -> X + Y F +)
	  (Y -> - F X - Y))
  :angle 90.0)

