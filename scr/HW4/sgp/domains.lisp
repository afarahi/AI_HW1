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

;;; $Id: domains.lisp,v 1.12 2000/01/14 00:23:08 corin Exp $

(in-package :domains)

(export '(define def-domain def-problem
	  *domains* *problems* *the-problem* *last-was-problem*
	  domain-name domain-requirements domain-operators 
	  domain-problems domain-types
	  problem-name problem-domain problem-init problem-goal
	  problem-objects
	  situation-objects
	  op-name op-parameters op-precondition op-effect op-variables
	  op-components op-typing
	  comp-precondition comp-effect comp-operator comp-name
	  check-requirements get-domain get-problem process-typed-list
          oneof uncertain iff observes forall exists))

;;; Functions that accept domain and problem definitions in UCPOP format

(defmacro DEFINE ((dtype name) &body body)
  (case dtype
    (domain `(apply #'def-domain '(,name ,@body)))
    (problem `(apply #'def-problem '(,name ,@body)))
    (situation `(apply #'def-situation '(,name ,@body)))
    ))

(defvar *domains* nil)
(defvar *problems* nil)
(defvar *situations* nil)
(defvar *the-problem* nil)
(defvar *last-was-problem* nil)


(defstruct (domain (:print-function print-domain))
  name
  requirements
  constants				; List of lists.  Each element of constants
					; is a conjunctive clause that delcares the 
					; appropriate constant.
  (types nil)				; An assoc list of (type . '(super-types))
  operators
  problems)

(defstruct (problem (:print-function print-problem))
  name
  domain
  requirements
  init
  goal
  objects				; A list of props, like init, that
					; are the objects in this problem
)

(defstruct (operator (:conc-name op-) (:print-function print-operator))
  name
  (parameters nil)
  variables                             ; derived from parameters
  typing                                ; addl preconditions derived from parameters
  (components nil)			; The components derived from this operator.
					; This list is computed at plan time, because
					; we wish to unroll universally quantified 
					; effects, and the universal base is known only
					; at plan time.
  (precondition nil)
  (effect nil)
  )

(defstruct (situation (:print-function print-situation))
  name
  domain
  init					; The initial conditions, as specified
					; exactly as in the situation or 
					; problem definition.
  objects				; A list of props, like init, that
					; are the objects in this problem
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DOMAIN support code

(defun print-domain (domain &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "<DOMAIN
 Name:  ~A
 Requirements: ~A
 Constants: ~A
 Operators:  ~A
 Problems:  ~A"
          (domain-name domain)
          (domain-requirements domain)
	  (domain-constants domain)
          (domain-operators domain)
          (domain-problems domain)))

(defun def-domain (name &rest clauses)
  (let ((operators nil)
        (requirements nil)
	(uses-typing nil)		; Does the domain use real typing?
	(constants nil)
	(types nil)
        (warnings nil)
        (problems (loop for problem in *problems*
                        when (eql name (problem-domain (get problem 'problem)))
                         collect problem)))
    (setf *last-was-problem* nil)
    (dolist (clause clauses)
      (case (car clause)
	(:extends
	 ;;; This domain inherits actions, constants, etc. from one or
	 ;;; more other domains.
	 (let ((extends (mapcar #'(lambda (name) (get-domain name)) 
				(cdr clause))))
	   (dolist (domain extends)
	     ;; Inherit requirements.
	     (setq requirements (union requirements (domain-requirements domain)))
	     ;; Inherit actions.
	     (setq operators (union operators (domain-operators domain)))
	     ;; Inherit constants.
	     (setq constants (union constants (domain-constants domain)))
	     ;; Inherit types.
	     (setq types (union types (domain-types domain))))))
        (:requirements 
         (setq requirements (union (cdr clause) requirements))
	 ;;; Always use typing.  Else, variables to actions that aren't
	 ;;; bound by a precondition are never bound.
	 (setq uses-typing t)
	 )
        (:action			; Used to be :operator
         (push (apply #'def-operator (cons uses-typing (cdr clause))) operators))
	(:constants
	 (setq constants (car (process-typed-list (cdr clause) nil nil nil
						  :include-objects t))))
	(:types
	 (setq types (make-super-types (cdr clause))))
	(:predicates nil)		; Ignore this piece of information
        (t (pushnew (car clause) warnings))))
    (when warnings
      (format t "~&;~4TWarning: unable to process parameter(s), ~{~S, ~}in domain ~A" 
            warnings name))
    (let ((domain (make-domain :name name
                               :requirements requirements
			       :constants constants
			       :types types
                               :operators operators
                               :problems problems)))
      (setf (get name 'domain) domain)))
  (pushnew name *domains*)
  name)

(defun get-domain (name)
  (get (find-symbol (symbol-name name) :domains) 'domain))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OPERATOR support code


(defun print-operator (operator &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "<Operator: ~A>" (op-name operator)))



;; Recursively translate the PDDL style parameter list into a conjunction
;; of clauses.  The types will be either (type ?var) or 
;; (:or (type1 ?var) (type2 ?var) ...)
(defun process-typed-list (typed-list untyped-variables type-clauses typed-variables
			   &key (include-objects nil))
  (if (null typed-list)			; Base case:  no more elements in the typed list.
      (cons (append type-clauses	; Default type is object.
		    (mapcar #'(lambda (var) (list 'object var)) untyped-variables))
	    (append typed-variables untyped-variables))

    (let ((typed-list-elem (pop typed-list))) ; Grab the next parameter
      (if (eql typed-list-elem '-)	; Hey - it's the type delimiter.
	  (let ((type (pop typed-list))) ; The next guy is either a type or 
					; a list of types, preceeded by :either
	    (process-typed-list		; Call process-typed-list recursively...
	     typed-list			; ...with the shorter parameter list...
	     nil			; ...and no variables pending a type...
	     (append type-clauses	; ...and a few more type clauses.
		     (mapcar #'(lambda (var) ; For each variable, add a clause
				 (if (consp type) ; List of types.
				     (append '(domains::or)
					     (mapcar #'(lambda (type-elem)
							  (list type-elem var))
						     (rest type)))
				   (list type var))) ; Just the variable.
			     untyped-variables))
	     (append typed-variables untyped-variables) ; The typed variables
	     :include-objects include-objects
	     ))
	(process-typed-list typed-list (append untyped-variables (list typed-list-elem))
			    type-clauses typed-variables 
			    :include-objects include-objects)))))
							    

(defun make-super-types (type-list)
  (let* ((type-pairs (make-type-pairs type-list nil nil))
	 (types (remove-duplicates (mapcar #'car type-pairs))))
    (mapcar #'(lambda (type)
		(cons type (compute-reachable type (list type)
					      type-pairs type-pairs)))
		types)))

;; Give the type-list as defined in PDDL, produce a list
;; of pairs of types (SUPER-TYPE SUB-TYPE) for each
;; super/sub type given explicitly in the type list.
(defun make-type-pairs (type-list subtypes type-pairs)
  (cond ((null type-list)
	 (append (mapcar #'(lambda (sub) (list 'object sub))
			 subtypes)
		 type-pairs))
	((eq (first type-list) '-)
	 (make-type-pairs 
	  (cddr type-list) nil
	  (append (expand-super subtypes (second type-list))
		  type-pairs)))
	(t (make-type-pairs (cdr type-list)
			    (cons (car type-list) subtypes)
			    type-pairs))))
				 
;; subtypes is a list of atomic types.  super-types
;; is either an atomic type or a list of 'either
;; followed by one or more super-types.
(defun expand-super (subtypes super-types)
  (cond ((null super-types) nil)
	((not (consp super-types))
	 (mapcar #'(lambda (sub) (list super-types sub)) subtypes))
	((eq (first super-types) 'either)
	 (expand-super subtypes (cdr super-types)))
	(t (append (expand-super subtypes (car super-types))
		   (expand-super subtypes (cdr super-types))))))


;; Compute the list of all reachable supertypes from the given
;; subtype.
(defun compute-reachable (sub supers type-pairs all-type-pairs)
  (cond ((null type-pairs) supers)
	((and (eq (first (car type-pairs)) sub)
	      (not (member (second (car type-pairs)) supers)))
	 ;; Recursively descend on supertype.
	 (union
	  (compute-reachable (second (car type-pairs))
			     (cons (second (car type-pairs)) supers)
			     all-type-pairs all-type-pairs)
	  (compute-reachable sub supers (cdr type-pairs) all-type-pairs)))
	(t (compute-reachable sub supers (cdr type-pairs) all-type-pairs))))


(defun def-operator (uses-typing name &key parameters precondition effect vars)
  (let (variables typing)		; Declare variables, typing.
    (if uses-typing			; This domain uses honest to goodness typing.
	(let ((types-and-vars (process-typed-list parameters nil nil nil
						  :include-objects nil)))
	  (setq typing (append (car types-and-vars) typing))
	  (setq variables (append (cdr types-and-vars) variables)))

      (progn				; Old, adl style types.
	(dolist (parameter parameters)	; Step through each parameter.  
	  (cond ((consp parameter)	; Typed parameters are a list of two things:
		 (push parameter typing) ; The type (parameter is actually the type+var)
		 (push (second parameter) variables)) ; And the variable name.
		(t (push parameter variables)))) ; Non-typed parameters are just the var names

      ;; Reverse the stacks of parameters and types that we created, so that 
      ;; they are in the input order.
      (setq variables (nreverse variables)
	    typing (nreverse typing))))

    ;; Create the new operator
    (setf (get name 'operator)
          (make-operator :name name
                         :parameters (append parameters vars)
                         :variables variables
                         :typing typing
			 ;; If we're typing our parameters, add the parameter types
			 ;; as preconditions.
                         :precondition (reorder-precs 
					(preprocess-precs (if typing 
							      `(domains::and 
								,@typing 
								,precondition)
							    precondition)))
                         :effect effect))
    
    ;; Return the new operator
    (get name 'operator)))
  

(defun reorder-precs (preconditions)
  ;; Accumulate all the equality and inequality constraints from the
  ;; preconditions.  Put them at the end of the list of preconditions.
  ;; Right before the (in)equality constraints come the negated
  ;; propositions.
  (cond ((null preconditions) nil)
	((not (listp preconditions)) preconditions)
	((eq (car preconditions) 'and)
	 ;; Move all the eq and ineq conjuncts to the end
	 
	 (cons 'and
	       (sort (mapcar #'reorder-precs (cdr preconditions)) #'prec-order)))
	(t (cons (car preconditions)
		 (mapcar #'reorder-precs (cdr preconditions))))))
	 
(defun prec-ordinal (p)
  (cond ((not (listp p)) 0)		; Atoms go first
	((eq (car p) '=) 2)		; = and not = are last
	((and (eq (car p) 'not)
	      (eq (caadr p) '=)) 2)
	((eq (car p) 'not) 1)		; Second are negated props
	(t 0)))				; Positive propositions also lead off

(defun prec-order (p1 p2)
  (< (prec-ordinal p1) (prec-ordinal p2)))


(defun preprocess-precs (precs)
  (cond ((null precs) precs)
	((not (listp precs)) precs)
	((eq (car precs) 'imply)
	 (list 'or
	       (list 'not (preprocess-precs (second precs)))
	       (preprocess-precs (third precs))))
	(t (mapcar #'preprocess-precs precs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SITUATION support code


;;; Situations are a convenient way to collect together common parts of
;;; a suite of problems.  Because any previous problems can also be used
;;; in the role of situations, we create a new situation both when we 
;;; define one explicitly and when a new problem is defined.

(defun print-situation (situation &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "<SITUATION
 Name:  ~A
 Domain:  ~A
 Objects:  ~A
 Initial-Condition:  ~A"
          (situation-name situation)
          (situation-domain situation)
          (situation-objects situation)
          (situation-init situation)))


(defun def-situation (name &rest clauses)
  (let ((domain nil)
	(init '(domains::and))
	(objects nil)
	(warnings nil))
    (setf *last-was-problem* nil)
    (dolist (clause clauses)
      (case (car clause)
	(:domain (setq domain (second clause)))
	(:init (setq init (append '(domains::and) (cdr clause))))
	(:objects (setq objects (union (car (process-typed-list (cdr clause) nil nil nil
								:include-objects t))
				       objects)))
	(t (pushnew (car clause) warnings))))
    
    (when warnings
      (format t "~&;~4TWarning: unable to process parameter(s), ~{~S, ~}in situation ~A"
	      warnings name))
    
    (let* ((situation 
	    (make-situation
	     :name name
	     :domain domain
	     :init init
	     :objects objects)))
      (setf (get name 'situation) situation))
    (pushnew name *situations*))
  name)

(defun get-situation (name)
  (get (find-symbol (symbol-name name) :domains) 'situation))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PROBLEM support code

;;;The problem structure remembers the domain, goals, and initial conditions
;;;associated with a single problem instance.  Don't try to reuse these.  Create
;;;it, use it and throw it away.

(defun print-problem (problem &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "<PROBLEM
 Name:  ~A
 Domain:  ~A
 Goal:  ~A
 Initial-Condition:  ~A"
          (problem-name problem)
          (problem-domain problem)
          (problem-goal problem)
          (problem-init problem)))


(defun def-problem (name &rest clauses)
  (let ((domain nil)
	(requirements nil)
	(situation nil)
	(init '(domains::and))
	(goal nil)
	(objects nil)
	(warnings nil))
    (setf *last-was-problem* t)
    (dolist (clause clauses)
      (case (car clause)
	(:domain (setq domain (second clause)))
	(:requirements (setq requirements (union (cdr clause) requirements)))
	(:situation (setq situation (get-situation (second clause)))
		    (unless situation
		      (format t "~&;~4TWarning: unable to find situation ~A in problem ~A"
			      (second clause) name))
		    (when (and situation
			       (not (eq (situation-domain situation) domain)))
		      (format t "~&;~4TWarning: situation ~A uses a domain different from that of problem ~A"
			      (second clause) name)))
	(:init (setq init (append '(domains::and) (cdr clause))))
	(:goal (setq goal (preprocess-precs (second clause))))
	(:objects (setq objects (union (car (process-typed-list (cdr clause) nil nil nil
								:include-objects t))
				       objects)))
	(:length nil)			; Just ignore this piece of information
	(t (pushnew (car clause) warnings))))
    
    (when warnings
      (format t "~&;~4TWarning: unable to process parameter(s), ~{~S, ~}in problem ~A"
	      warnings name))
    
    ;; Handle situation expansion
    (when situation
      ;; First, the inits.  The inits from the problem will has an AND at its car,
      ;; if they're non-nil.  If there's a conflict between an init in a situation
      ;; and an init in a problem, the problem's  version wins.
      (setq init (append '(domains::and)
			 (combine-inits (cdr (situation-init situation)) (cdr init))))

      ;; Now, the objects.  The problem and the situation will simply have
      ;; lists of objects.
      (setq objects (union objects (situation-objects situation))))
      
    
    (let* ((problem (make-problem
		     :name name
		     :domain domain
		     :requirements requirements
		     :init (let ((domain-struct (get domain 'domain)))
			     (unless domain-struct
			       (error "Domain ~A not found for problem ~A.~%"
				      domain name))
			     (append init 
				     (domain-constants domain-struct)
				     objects))
		     :objects (append objects (domain-constants (get domain 'domain)))
		     :goal goal)))
      (setf (get name 'problem) problem))
    (pushnew name *problems*)
    (setf *the-problem* name)

    ;; Create a situation based on this problem, and add that situation
    ;; to the global list.
    (let ((situation 
	   (make-situation
	    :name name
	    :domain domain
	    ;; Should the domain constants be part of the situation defined by 
	    ;; this problem?  I'm guessing not.
	    :init init			; Keep the leading AND
	    :objects objects)))
      (setf (get name 'situation) situation)
      (pushnew name *situations*))
    
    (let ((domain-struct (get domain 'domain)))
      (when (domain-p domain-struct)
	(pushnew name (domain-problems domain-struct)))))
  name)

(defun get-problem (name)
  (get (find-symbol (symbol-name name) :domains) 'problem))


(defun combine-inits (sit-inits prob-inits)
  (if (null sit-inits) prob-inits
    (let* ((next-init (car sit-inits))
	   (neg-next-init (simple-invert next-init)))
      (if (member neg-next-init prob-inits :test #'equal)
	  ;; The problem inits override the situation inits when there
	  ;; is a conflict.  Continue to the next situation init.
	  (combine-inits (cdr sit-inits) prob-inits)
	(cons next-init
	      (combine-inits (cdr sit-inits) prob-inits))))))

(defun simple-invert (p)
  (if (eq (first p) 'not) (second p)
    (list 'not p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Checking requirments

(defun check-requirements (capabilities problem domain)
  (let* ((domain-requirements (domain-requirements domain))
         (problem-requirements (problem-requirements problem))
         (trouble
          (set-difference (reduce-requirements (union problem-requirements
                                                      domain-requirements))
                          (reduce-requirements capabilities))))
    (when trouble
      (warn "Planner is unlikely to work on ~A because of 
~{~S ~}
in the domain or problem description." (problem-name problem) trouble))

    (null trouble)))

(defparameter *requirement-reductions*
  '((:adl :conditional-effects :disjunctive-preconditions 
          :quantified-preconditions :quantified-effects :typing)
    (:ucpop :adl :domain-axioms :procedural-attachments :safety-constraints)))

(defun reduce-requirements (reqs)
  (loop for req in reqs nconc
        (let ((reduction (assoc req *requirement-reductions*)))
          (if reduction 
            (reduce-requirements (rest reduction))
            (list req)))))
