;;;; paip/paip.asd
   
(in-package :asdf-user)


(defsystem "paip"
  :description "Code from Peter Norvig's
  Paradigms in Artificial Intelligence
  Programming."
  :author "Peter Norvig & Larry Devlin"
  :mailto "larrydevlin1770@gmail.com"
  :depends-on ("alexandria"
               "repl-utilities")
  :serial t
  :components ((:file "package")
	       (:file "utils")
               (:file "ch-02")
               (:file "ch-03")
               (:file "ch-04")
               (:file "ch-05")
               (:file "ch-06--tools")
               (:file "ch-07")
               (:file "ch-08")
               (:file "ch-09-efficiency")
               (:file "ch-09a")
               (:file "ch-09b")
               (:file "ch-10-more-efficiency")
               (:file "ch-11")
               (:file "ch-12")
               (:file "ch-13")
               (:file "ch-14")
               (:file "ch-14a")
               (:file "ch-14b")
               (:file "ch-14c")
               (:file "ch-15")
               (:file "ch-16")
	       (:file "ch-16a")
               (:file "ch-17")))
              
              
