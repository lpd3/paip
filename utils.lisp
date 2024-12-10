(in-package :paip-utils)


(defmacro trace-all (&optional (package *package*))
  (unless (packagep package)
    (setq package (find-package package)))
  (let ((symbol (gensym "symbol"))
	(symval (gensym "symval")))
    `(with-package-iterator (genfn ,package :internal :external)
       (loop
	(multiple-value-bind (morep ,symbol _ __)
	    (genfn)
	  (unless morep
	    (return))
          (let ((,symval ,symbol))
	    (when (fboundp ,symval)
	      (trace (symbol-value ,symval)))))))))
