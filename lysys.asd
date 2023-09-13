;;;; lysys.asd

(asdf:defsystem #:lysys
  :description "Lyndenmayer systems with Asymptote graphics."
  :author "Ted Szylowiec <tedszy@gmail.com>"
  :license  "MIT License"
  :version "0.0.1"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
	       (:file "database")
               (:file "lysys")))
