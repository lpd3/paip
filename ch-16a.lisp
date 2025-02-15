;;;; ch-16a.lisp

;;;; I broke this into a
;;;; separate file in the hopes
;;;; that the forms provided
;;;; here would be compiled
;;;; after the forms in the
;;;; previous file. That way
;;;; I shouldn't have to wrap
;;;; a bunch of functions
;;;; in an `eval-when` block.

(in-package :expert)

(defun mycin ()
  "Determine what organism
   is infecting a patient"
  (emycin
   (list
    (defcontext
	patient
	(name sex age) ())
    (defcontext
	culture
	(site days-old) ())
    (defcontext
	organism
	() (identity)))))

;; Parameters for patients

(defparm name patient t
  "Patient's name: "
  t read-line)

(defparm sex patient
  (member male female)
  "Sex: "  t)

(defparm age patient number
  "Age: " t)

(defparm burn patient
  (member no mild serious)
  "Is ~A a burn patient? If so, mild or serious?" t)

(defparm compromised-host
  patient yes/no
  "Is ~A a compromised host?")

(defparm wbc patient number
  "What is ~A's white blood cell count?")

;; Parameters for culture

(defparm site culture
  (member blood)
  "From what site was the culture for ~A taken?" t)

(defparm days-old culture
  number
  "How many days ago was this culture (~A) obtained?" t)

;; Parameters for organism

(defparm identity organism
  (member
   pseudomonas
   klebsiella
   enterobacteriaceae
   staphylococcus
   bacteroides
   streptococcus)
  "Enter the identity (genus) of ~A" t)

(defparm gram organism
  (member acid-fast pos neg)
  "The gram stain of ~A:" t)

(defparm morphology organism
  (member rod coccus)
  "Is ~A a rod or coccus (etc.):")

(defparm aerobicity organism
  (member aerobic
	  anaerobic))

(defparm growth-conformation
    organism
    (member chains pairs
	    clumps))

(clear-rules)

(defrule 1
  if (immunosuppressed patient is yes)
  then 1.0 (compromised-host patient is yes))

(defrule 2
  if (leukopenia patient is yes)
  then 1.0 (immunosuppressed patient is yes))

(defrule 3
  if (wbc patient < 2.5)
  then .9 (leukopenia patient is yes))

(defrule 52
  if (site culture is
	   blood)
  (gram organism is neg)
  (morphology organism
	      is rod)
  (burn patient is
	serious)
  then .4
  (identity organism is
	    pseudomonas))

(defrule 71
  if (gram organism is pos)
  (morphology organism
	      is coccus)
  (growth-comformation
   organism is clumps)
  then .7
  (identity organism is
	    staphylococcus))

(defrule 73
  if (site culture is blood)
     (gram organism is neg)
  (morphology organism
	      is rod)
  (aerobicity organism
	      is anaerobic)
  then .9
  (identity organism is
	    bacteroides))

(defrule 75
  if (gram organism is neg)
  (morphology organism
	      is rod)
  (compromised-host
   patient is yes)
  then .6
  (identity organism is
	    pseudomonas))

(defrule 107
  if (gram organism is neg)
  (morphology organism
	      is rod)
     (aerobicity organism is aerobic)
  then .8
  (identity organism is
       enterobacteriaceae))

(defrule 165
  if (gram organism is pos)
  (morphology organism
	      is coccus)
  (growth-conformation
   organism is chains)
  then .7
  (identity organism is
	    streptococcus))


