;; $Id: entry.lisp,v 1.2 1998/06/05 06:35:20 corin Exp $

(load "loader")
(defvar *init* nil)
(unless *init*
  (compile-gp)
  (setf *init* t))



;;;(import '(domains::*the-problem* domains::*last-was-problem*))

(in-package :gp)

(defun GO! (filenames-file &key (output "sgp.out"))
  ;; filenames-file is a file that contains a list of PDDL
  ;; files.

  (with-open-file (stream (make-pathname :name filenames-file))
    (with-open-file (outstream (make-pathname :name output)
		     :if-exists :supersede :direction :output)
      (let ((file-list (read-files stream (list '$eof$))))
	(dolist (pddl-file file-list)
	  ;; Load the PDDL file
	  (in-package :domains)
	  (load pddl-file :verbose nil)

	  (when *last-was-problem*
	    ;; Garbage collect so that the previous invocation of 
	    ;; the planner won't affect this invocation as much.
	    (user::gc t)
	  
	    (format t "~&~A~%" *the-problem*)
	    (format outstream "~&~A~%" *the-problem*)
	    

	    ;; Invoke the planner on the problem defined in the PDDL
	    ;; file.
	    (in-package :gp)
	    (let* ((start-time (get-internal-run-time))
		   (the-plan (plan domains::*the-problem*
				   :factored-expansion t
				   :dvo nil
				   :lpvo nil
				   :ground nil
				   :levels 999
				   :return-lists t
				   :reduced-output t))
		   (end-time (get-internal-run-time))
		   (the-linear-plan (linearize the-plan)))
	      (cond ((eq the-plan nil)
		     (format t "~&NO SOLUTION~%")
		     (format outstream "~&NO SOLUTION~%"))
		    ((equal the-linear-plan '())
		     (format t "~&()~%")
		     (format outstream "~&()~%"))
		    (t
		     (format t "~&(~A~{~&~A~})~%" 
			     (car the-linear-plan) (cdr the-linear-plan))
		     (format outstream "~&(~A~{~&~A~})~%" 
			     (car the-linear-plan) (cdr the-linear-plan))))
		     
	      (format t "~&~A~%" 
		      (* (/ (- end-time start-time)
			    internal-time-units-per-second)
			 1000))
	      (format outstream "~&~A~%" 
		      (* (/ (- end-time start-time)
			    internal-time-units-per-second)
			 1000)))))))))
	      

(defun read-files (stream eof)
  (let ((filename (read-line stream nil eof)))
    (if (eq filename eof) nil
      (cons filename (read-files stream eof)))))

(defun linearize (the-plan)
  (cond (the-plan
	 (append (car the-plan) (linearize (cdr the-plan))))
	(t nil)))


;;;
;;; $Log: entry.lisp,v $
;;; Revision 1.2  1998/06/05 06:35:20  corin
;;; A few small mods, for calling convention and compiler warnings
;;;
;;; Revision 1.1  1998/06/04 23:43:40  corin
;;; Made ready for competition calling convention
;;;
