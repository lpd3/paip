;;;; paip/paip.asd
                 
(eval-when (:load-toplevel)
  (qml:quicklisp))

(let ((dependencies '(:alexandria
                      :repl-utilities)))
  (dolist (dep dependencies)
    (when (not (find-package dep))
      (quicklisp:quickload dep))))
  
                 
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
               (:file "ch-16")))
              
              