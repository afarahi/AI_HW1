;;;
;;; This is the Common Lisp implementation of Sensory Graphplan.  Original 
;;; Lisp Graphplan implementation by Mark Peot <peot@rpal.rockwell.com>.
;;; Enhancements by Dave Smith <de2smith@ptolemy.arc.nasa.gov>
;;; Support for Factored Expansion, PDDL
;;; domains, and other optimizations by Dave Smith, Dan Weld
;;; <weld@cs.washington.edu>, and Corin Anderson <corin@cs.washington.edu>.
;;;
;;; Copyright (c) Mark Peot, 1995; University of Washington, 1997, 1998.
;;;
;;; Please send mail to bug-sgp@cs.washington.edu if you download this
;;; code, find bugs in it, or just wish to receive occasional news about
;;; possible new versions. 
;;;

;;; $Id: loader.lisp,v 1.9 1999/05/19 22:14:59 corin Exp $

(in-package :cl-user)

(defpackage "DOMAINS" (:use :common-lisp))
(defpackage "LOGIC" (:use :common-lisp))
(defpackage "GP" (:use :common-lisp :domains :logic))
(defpackage "SGP" (:use :common-lisp :domains :logic))

(in-package :gp)

(defun concat-directory (dir new)
  (make-pathname :directory (nconc (pathname-directory dir) (list new))))

(defparameter *gp-dir* "./")
(defparameter *pddl-dir* "./domains/")

(defparameter *gp-files* '(|domains.lisp| |logic.lisp| |gp.lisp| |sgp.lisp|))

(defun LOAD-gp ()
  (dolist (file *gp-files*)
    (format t "~&; Loading ~A" file)
    (load (merge-pathnames *gp-dir* (symbol-name file)))))

(defun compile-and-load (file)
  (compile-file file)
  (load file))
  
(defun COMPILE-gp ()
  (dolist (file *gp-files*)
    (format t "~&; Compiling & loading ~A" file)
    (compile-and-load (merge-pathnames *gp-dir* (symbol-name file)))))

(defun LOAD-domains (&rest domains)
  (setq domains (if domains
                  (loop for domain in domains collect
                        (make-pathname :type "pddl" 
                                       :defaults (merge-pathnames *pddl-dir*
                                                                  (format nil "~a" domain))))
                  (directory (merge-pathnames *pddl-dir* "*.pddl"))))
  (in-package :domains)
  (dolist (domain domains)
    (load domain))
  (in-package :gp)
  t)

(import '(load-gp compile-gp load-domains) :cl-user)
