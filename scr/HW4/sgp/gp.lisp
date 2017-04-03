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

;;; $Id: gp.lisp,v 1.33 2000/01/14 23:49:49 corin Exp $

(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))

;;; This loads all code into the gp package which uses the domains package
;;;  (use-package :domains)  already done in defpackage for :gp in loader
(in-package :gp)

(export '(process-quantifier-many))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Runtime options
;;;
;;; *ground* (:ground)
;;;    If non-nil, precompute all the ground instances of all
;;;    operators before planning.  Works with factored expansion
;;;    when :typing is used.
;;;
;;; *dvo* (:dvo)
;;;    If non-nil, choose the next goal to search on by selecting
;;;    the goal with the least number of establishing actions.
;;;
;;; *lpvo* (:lpvo)
;;;    If non-nil, choose the next action to try by selecting 
;;;    that which has the least number of preconditions.

(defvar *ground*)
(defvar *dvo*)
(defvar *lpvo*)


(defvar *trace-gp-top-level* nil)
(defvar *trace-gp-contexts* nil)
(defvar *trace-gp-actions* nil)
(defvar *trace-gp-print-detailed-actions* nil)
(defvar *trace-gp-mutex* nil)
(defvar *trace-gp-bc* nil)
(defvar *trace-gp-csp* nil)
(defvar *stat-gp-time-expansion* nil)
(defvar *stat-gp-time-bc* nil)
(defvar *stat-gp-time-mutex* nil)
(defvar *stat-gp-graph-size* nil)
(defvar *stat-gp-mutex-count* nil)

;;;
;;; TRACE-GP trace-vars
;;;
;;; Print function traces of key Graphplan routines.  See the list
;;; of defvars above for *trace-gp-XXX* variables that can be 
;;; enabled.  trace-vars should be zero or more XXX's.
(defmacro trace-gp (&rest args)
  (dolist (arg args)
      (set (intern (format nil "*TRACE-GP-~A*" arg)) t)))

(defmacro untrace-gp (&rest args)
  (dolist (arg args)
    (set (intern (format nil "*TRACE-GP-~A*" arg)) nil)))

;;;
;;; STAT-GP stat-vars
;;;
;;; Print timing information about key Graphplan routines.  See 
;;; the list of defvars above for *stat-gp-XXX* variables that can 
;;; be enabled.  stat-vars should be zero or more XXX's.
(defmacro stat-gp (&rest args)
  (dolist (arg args)
      (set (intern (format nil "*STAT-GP-~A*" arg)) t)))

(defmacro unstat-gp (&rest args)
  (dolist (arg args)
    (set (intern (format nil "*STAT-GP-~A*" arg)) nil)))

;;;Timing macro.
(defmacro TIME-IF (flag . code)
  (if flag
    `(time . ,code)
    `(progn . ,code)))

(defmacro TRACER (exp &rest args)
  `(when ,exp
     (format t ,@args)))

(defmacro TIMER (flag code &rest args)
  `(cond (,flag (format t ,@args)
		(time ,code))
	 (t ,code)))

;;;
;;; A wrapper to instance?  If *ground* is non-nil, then
;;; all the possible expressions are variable-free.  A simple
;;; equal test will be sufficient to tell if the one expression
;;; is an instance of another.
;;;
(defun gp-instance? (var-exp constant-exp bindings)
  (if *ground*
      (if (equal var-exp constant-exp)
	  nil
	+false+)
    (instance? var-exp constant-exp bindings)))
  

;;;------------------------structures---------------------------

;;;The plan graph.
;;;After n time periods, there are n+1 proposition levels, n+1 goal-caches
;;;and n action-levels.  action and proposition levels are ordered from the
;;;most recent to the oldest. The goal caches are used for memoizing impossible
;;;goal combinations.
(defstruct (PLAN-GRAPH (:conc-name pg-))
  (proposition-levels nil)
  (action-levels nil)
  (goal-caches nil)
  problem
  domain)

(defmethod PRINT-OBJECT ((plan-graph plan-graph) stream)
  (format stream "<Plan-Graph:")
  (loop for action-level in (pg-action-levels plan-graph)
        as  i from (length (pg-action-levels plan-graph)) downto 1
        as  prop-level in (pg-proposition-levels plan-graph)
        do
        (format stream "~%   Prop-Level ~A: ~A" i prop-level)
        (format stream "~%   Action-Level ~A: ~A" i action-level))
  (format stream "~%   Prop-Level 0: ~A>" (car (last (pg-proposition-levels
                                                   plan-graph)))))

;;; base structure for both proposition and action nodes in the graph
(defstruct NODE     
  (mutex nil))

;;;Propositions need to be linked to each other (via mutex relationships) and to
;;;the preceding action level (in order to find mutex relationships.).  Props
;;;are linked to the succeeding action level in order to efficiently identify
;;;action mutex relationships.
(defstruct (PROPOSITION (:include node) (:conc-name prop-) 
                        (:print-function print-proposition))
  description				; The s-exp that denotes this proposition
  (establishers nil)                    ; Actions that established this prop.
  (consumers nil)                       ; Actions that use this prop.
  (number 0))				; For sorting purposes later on.

(defun PRINT-PROPOSITION (proposition &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "<Proposition: ~A>" (prop-description proposition)))

(defvar *prop-number* 0)

(defmacro new-proposition (&rest args)
  `(make-proposition ,@args :number (incf *prop-number*)))


;;; An instantiated component:  a ground instance of a component.
(defstruct (INSTANTIATED-COMPONENT (:include node) (:conc-name inst-comp-)
	    (:print-function print-inst-comp))
  preconditions				; The precond. props of this component.
  effects				; The effect props.
  component				; The component that is being instantiated
  bindings)				; The variable bindings used in this 
					; instantiation of the component.
					; NB:  The component itself may also
					; have bindings as well if *ground* is
					; non-nil.

(defun PRINT-INST-COMP (inst-comp &optional (stream t) depth)
  (declare (ignore depth))
  (let ((component (inst-comp-component inst-comp)))
    (if (eq component 'persist)
	(format stream "(Persist ~A)" 
		(prop-description (first (inst-comp-preconditions inst-comp))))
      (format stream "(~A~{ ~A~})" component
	      (plug (op-variables (comp-operator component)) 
		    (if *ground*
			(comp-bindings (inst-comp-component inst-comp))
		      (inst-comp-bindings inst-comp))
		    ))
      )))
  



;;;----------------------THE PLANNER------------------------
;;;
;;; This is the top level interface

(defparameter *GP-CAPABILITIES* 
    '(:strips :equality :typing :conditional-effects :disjunctive-preconditions 
      :universal-preconditions :existential-preconditions :adl :open-world))

(defvar *initial-props* nil "List of propositions in initial state")
(defvar *objects* nil "List of objects in the universe")

(defvar *closed-world* nil)

;;; This is the main entry point to the planner.
;;; The first argument should be the name of a problem (see 
;;; domains.lisp).  Optional keyword arguments are:
;;;     :levels - number of levels to expand (default 10)
;;;     :all-plans - if t returns all plans found at the level
;;;     :domain - used to override domain specified in problem
;;;     :ground - compute all ground instances of components before
;;;               beginning planning process
;;;     :dvo - When choosing which goal to search next, choose the
;;;            one with the least number of establishing components.
;;;     :lpvo - When choosing which component to try next, choose the
;;;             one with the least number of preconditions

(defun PLAN (problem &key (domain nil) (levels 10) (all-plans nil)
			  (ground nil) (dvo t) (lpvo t))
  (declare (special the-components the-domain level))
  (setf *initial-props* nil)
  (setf the-components nil)
  (setf *ground* ground)
  (setf *dvo* dvo)
  (setf *lpvo* lpvo)
  (setq problem (get-problem problem))
  (unless problem (format t "Problem not defined") (return-from plan nil))
  (setq domain (get-domain (or domain (problem-domain problem))))
  (setq the-domain domain)
  (unless domain (format t "Domain not defined") (return-from plan nil))
  (when (check-requirements *gp-capabilities* problem domain)
    ;; List the set of options
    (format t "~&Operators are ground ~A~%"
	    (if *ground* "initially" "at graph expansion time"))
    (format t "~&Dynamic variable ordering is ~A~%"
	    (if *dvo* "enabled" "disabled"))
    (format t "~&Least preconditional action ordering is ~A~%"
	    (if *lpvo* "enabled" "disabled"))

    (setf *closed-world* (not (member ':open-world (domain-requirements domain))))

    (let ((plan-graph (initialize-plan-graph problem domain))
	  (plans nil)
	  (leveled-off nil)
	  (old-node-count 0)
	  (old-mutex-count 0)
	  (old-goal-cache-counts '()))
      (declare (special plan-graph))
      ;; Reset the comonents associated with each operator.
      (mapcar #'(lambda (op) (setf (op-components op) nil)) (domain-operators domain))
      (setf the-components (generate-components (first (pg-proposition-levels plan-graph))
						(domain-operators domain)))

      ;; The main loop.
      (format t "~%Levels")
      (loop for level from 1 upto levels 
				  ;; Until a solution is found, or until it can be
				  ;; proven that no solution exists...
          until (or plans
		    (and leveled-off
			 (exists-match 
			  (subseq (reverse old-goal-cache-counts) leveled-off)
			  (subseq (reverse (mapcar #'hash-table-count 
						   (pg-goal-caches plan-graph)))
				  leveled-off)))) do
	    (tracer *trace-gp-top-level* "~%")
	    (format t " ~S" level)

	    ;; (1) EXPAND THE GRAPH
	    (timer *stat-gp-time-expansion*
		   (extend-plan-graph plan-graph)
		   "~&Expanding graph...~%")

	    ;; Determine some stats about the new level of the planning
	    ;; graph, and decide if the graph has leveled off.
	    (let* ((new-action-count 
		    (length (car (pg-action-levels plan-graph))))
		   (new-proposition-count
		    (length (car (pg-proposition-levels plan-graph))))
		   (new-node-count
		    (+ new-action-count new-proposition-count))
		   (new-action-mutex-count
		    (apply #'+ (mapcar #'(lambda (n) (length (node-mutex n)))
				       (car (pg-action-levels plan-graph)))))
		   (new-proposition-mutex-count
		    (apply #'+ (mapcar #'(lambda (n) (length (node-mutex n)))
				       (car (pg-proposition-levels plan-graph)))))
		   (new-mutex-count
		    (+ new-action-mutex-count new-proposition-mutex-count)))
	      (tracer *stat-gp-graph-size*
		      "~&  ~A actions ~A propositions~%"
		      new-action-count new-proposition-count)
	      (tracer *stat-gp-mutex-count*
		      "~&  ~A action mutexes  ~A proposition mutexes~%"
		      new-action-mutex-count new-proposition-mutex-count)
	      (when (and (null leveled-off)
			 (= new-node-count old-node-count)
			 (= new-mutex-count old-mutex-count))
		(setq leveled-off level))
	      (setq old-node-count new-node-count)
	      (setq old-mutex-count new-mutex-count))

	    ;; Record the size of each memoized set of failing goals
	    (setq old-goal-cache-counts (mapcar #'hash-table-count
						(pg-goal-caches plan-graph)))

	    (tracer *trace-gp-top-level* "~%~2TChecking goals")

	    ;; (2) SEARCH FOR A SOLUTION
	    (timer *stat-gp-time-bc*
		   (setq plans (find-plans-in-pg plan-graph all-plans))
		   "~&Backward searching...~%"))
      
      (cond ((null plans)
	     (format t "~&No solution.~%"))
	    (all-plans
	     ;; List out all the plans found.
	     (mapcar #'(lambda (p) 
			 (components-to-actions (nreverse (prune-persists p))))
		     plans))
	    (t 
	     ;; List out the first satisfising plan found.
	     (components-to-actions (nreverse (prune-persists plans))))))))

;;; l1 and l2 are lists of integers.  Return non-nil iff l1 and l2
;;; have some pair of integers the same in the same place.
(defun exists-match (l1 l2)
  (cond ((null l1) nil)
	((null l2) nil)
	((eq (car l1) (car l2)) t)
	(t (exists-match (cdr l1) (cdr l2)))))

;;;
;;; Initialize-plan graph builds the plan graph structure and puts 
;;; the initial conditions in at level 1.
;;;
(defun INITIALIZE-PLAN-GRAPH (problem domain)
  (setq *prop-number* 0)
  (setf *initial-props* (mapcar #'(lambda (prop) (new-proposition :description prop))
				(clauses (problem-init problem))))
  (setf *objects* (mapcar #'(lambda (o) (find-prop o *initial-props*))
			  (problem-objects problem)))
  (make-plan-graph 
   :proposition-levels 
   (list (mapcar #'(lambda (prop) (new-proposition :description prop))
                 (clauses (problem-init problem))))
   :goal-caches (list (make-hash-table :test #'equal))
   :problem problem
   :domain domain))


;;;
;;; A COMPONENT, in the factored expansion sense (see Anderson, 
;;; Weld, & Smith.  "Conditional Effects in Graphplan", Fourth Int'l
;;; Conference on AI Planning Systems).
;;;
(defstruct (component (:conc-name comp-) (:print-function print-component))
  name					; A generated name.
  operator				; The operator from whence this component came.
  (bindings nil)			; The set of variable bindings that ground
					; this component.
  (precondition nil)
  (effect nil))

(defun print-component (component &optional (stream t) depth)
  (declare (ignore depth))
;;;  (format stream "<Component: ~A (~A) [~A]>" 
;;;	  (comp-name component) (comp-operator component) (comp-precondition component))
  (format stream "<Component: ~A (~A)>" 
	  (comp-name component) (comp-operator component)))


;;; Given the list of initial propositions, and the operators
;;; from the domain, generate the components to be used.  We
;;; need to have the init-props because there may be universal
;;; quantifications to be instantiated into components.
(defun GENERATE-COMPONENTS (init-props operators)
  (cond ((null operators) '())
	(t (let ((components (generate-components-from-op init-props (car operators))))
	     (setf (op-components (car operators)) components)
	     (append components
		   (generate-components init-props (cdr operators)))))))

;;
;; We actually know everything at this point to be able to 
;; calculate the ground instances of the components, and not
;; merely their lifted forms.  Let's try to do just that.
;; The upshot is that we won't have to worry about unification
;; and type checking during graph expansion and back-chaining
;; search.
;;

(defun GENERATE-COMPONENTS-FROM-OP (init-props operator)
  (declare (special operator components))
  (setf components nil)
  ;; Do we really want to modify the operators themselves?  Surely
  ;; not -- what would we do when we load a new problem with the
  ;; same domain?  Instead, we should just be sure that no one
  ;; uses any op- accessors that might include free variable
  ;; references.
  (if *ground*
      (ground-comp (op-variables operator) (op-typing operator) nil
		   nil init-props operator)
    (gen-comp (list (op-precondition operator))
	      (list (op-effect operator))
	      nil
	      init-props nil))
  components)


;;; Compute all the ground instances of the given operator.  For
;;; each ground instance, generate the components for that ground
;;; instance.
(defun ground-comp (variables type-preconditions processed-type-precs
		    bindings
		    init-props operator)
  (cond ((null variables) 
	 (gen-comp (set-difference 
		    ;; Clauses flattens the precondition list by removing all the
		    ;; silly extra ANDs.  cdr strips off *its* leading AND.
		    (cdr (conjuncts (list (plug (op-precondition operator) bindings))))
		    (plug processed-type-precs bindings)
		    :test #'equal)
		   (list (plug (op-effect operator) bindings))
		   nil init-props bindings))
	(type-preconditions
	 ;; Use the next type preconditions to bind zero or more variables.
	 ;; We might bind zero variables if the variable bindings already
	 ;; ground the type precondition.
	 (let* ((type-prec (car type-preconditions))
		(sub-names (sub-names (first type-prec))))
	   (dolist (sub-name sub-names)
	     (let ((subtype-prec (cons sub-name (cdr type-prec))))
	       (dolist (prop init-props)
		 (let* ((new-bindings (instance? subtype-prec (prop-description prop) bindings))
			(bound-var (unless (eq new-bindings +false+) (caar new-bindings))))
		   (unless (eq new-bindings +false+)
		     (ground-comp (remove-if #'(lambda (v) (eq v bound-var)) variables)
				  (cdr type-preconditions) (cons subtype-prec processed-type-precs)
				  new-bindings
				  init-props operator)))))))
	 )
	
	(t
	 ;; There are no type preconditions, but there are still variables
	 ;; left unbound.  Just bind each variable to each possible object
	 ;; in the universe.  Ick.
	 (dolist (prop init-props)
	   (let* ((next-var (car variables))
		  (new-bindings (instance? (list 'object next-var)
						(prop-description prop) bindings)))
	     (when (eq new-bindings +false+)
	       (format t "~&Warning:  Unbound variable ~A in operator ~A~%"
		       next-var operator))
	     ;; Always assume that the variable was bound.  If it wasn't, then
	     ;; we're toasted anyway, so it doesn't matter how we go down.
	     (ground-comp (cdr variables) type-preconditions processed-type-precs
			  new-bindings init-props operator))))))



;;; Here's where we do the factoring in factored expansion.  On entry,
;;; an operator's unconditional preconditions and unconditional effects
;;; are given.  We produce a separate component for each possible 
;;; [un]conditional effect.
(defun gen-comp (precs			; The preconditions to the component.
		 action-effects		; The action's effects that remain unprocessed.
		 comp-effects		; The components effects that we've accumulated.
		 init-props		; The initial propositions that are true.
		 bindings)		; The variable bindings, if any.
		 
  (if (null action-effects)
      (when comp-effects (make-new-comp precs comp-effects bindings))
    (let* ((effect (pop action-effects))
	   (keyword (when (consp effect) (first effect))))
      
      (case keyword
	(when				; Call gen-comp twice:  once to handle the 
					; conditional, and once to proceed with
					; the remaining action-effects.

	    (gen-comp (append precs	; The action's preconditions,
			      (list (second effect))) ; plus the conditional's antecedant.
		      (list (third effect)) ; The condition's effect.
		      nil		; Only the condition's effect can be part of 
					; the component's effect.
		      init-props bindings)
	  (gen-comp precs action-effects comp-effects init-props bindings))
	(and (gen-comp precs (append action-effects (rest effect)) comp-effects init-props bindings))
	(forall (gen-comp precs (append (process-quantifier-many effect nil)
						  action-effects) 
				    comp-effects init-props bindings))
	(t (gen-comp precs action-effects (cons effect comp-effects) init-props bindings))))))


;;; Create a new component.
(defun make-new-comp (precs effects bindings)
  (declare (special components operator))
  (let* ((component-id (gensym))
	 (new-component (make-component :name component-id
					:operator operator
					:bindings bindings
					:precondition (conjuncts (cons 'and precs))
					:effect (cons 'and effects))))
    (push new-component components)))



;;;-------------------EXTENDING THE PLAN GRAPH---------------------
;;;These functions extend the plan graph.

(defun EXTEND-PLAN-GRAPH (plan-graph)
  (declare (special the-components))

  (let ((previous-prop-level (first (pg-proposition-levels plan-graph)))
        (new-prop-level nil)
        (new-action-level nil))
    (declare (special previous-prop-level new-prop-level new-action-level))
  
    ;;Add the persistence actions.
    (progn (tracer *trace-gp-top-level* "~%  Adding persistence components for: ")
	   (time-if nil (add-null-components previous-prop-level)))
  
    ;;Instantiate all of the possible actions from the last
    ;;set of propositions.
    (progn (tracer *trace-gp-top-level* "~%  Adding components: ")
	   (time-if nil (add-inst-components the-components)))

    (tracer *trace-gp-actions* "~%~4Tnew propositions: ~{    ~a~%~}" 
            (loop for prop in new-prop-level
                  unless (find 'persist (prop-establishers prop) :key #'inst-comp-component)
                  collect prop))

    ;;Add the new action (component) level.
    (push new-action-level (pg-action-levels plan-graph))
    
    ;; For each new prop, sort the prop's establishers in ascending order
    ;; of number of preconditions.
    (when *lpvo*
      (dolist (prop new-prop-level)
	(setf (prop-establishers prop)
	  (sort (prop-establishers prop) #'< 
		:key #'(lambda (e) (length (inst-comp-preconditions e)))))))

    ;;Add the new proposition level.
    (push new-prop-level (pg-proposition-levels plan-graph))
    
    ;;Add a new goal cache.
    (push (make-hash-table :test #'equal) (pg-goal-caches plan-graph))

    ;;Look for mutual exclusion relationships using the three
    ;;rules given in the paper.
    (tracer *trace-gp-top-level* "~%  Finding mutex: ")

    (timer *stat-gp-time-mutex*
	   (derive-mutex previous-prop-level new-prop-level new-action-level)
	   "~&Finding mutexes...~%")))

  
;;;The proposition lookups would be faster if we replaced the
;;;unordered lists each level with something that has more structure.
(defun find-prop (prop prop-level)
  (find prop prop-level
        :key #'prop-description
        :test #'equal))

;;; Create a new proposition at the newest proposition level.  If the prop
;;; already existed at that level, do nothing.  A proposition in this
;;; case is a structured object.  The input (prop) is a description --
;;; a list, like (CLEAR TABLE).
(defun new-prop (prop)
  (declare (special new-prop-level))
  (let ((new-prop (find-prop prop new-prop-level)))
    (unless new-prop
      (setf new-prop (new-proposition :description prop))
      (push new-prop new-prop-level))
    new-prop))

(defun INTERSECT-P (a b)
  (loop for ele in a thereis
        (member ele b :test #'eq)))

;;; adds nops to graph
(defun ADD-NULL-COMPONENTS (previous-prop-level)
  (declare (special new-action-level))
  (dolist (persisted-prop previous-prop-level)
    (let* ((new-prop (new-prop (prop-description persisted-prop)))
           (persistence-inst-comp
            (make-instantiated-component :component 'persist
                         :preconditions (list persisted-prop)
                         :effects (list new-prop))))
      (push persistence-inst-comp (prop-consumers persisted-prop))
      (push persistence-inst-comp new-action-level)
      (push persistence-inst-comp (prop-establishers new-prop))
      (tracer *trace-gp-actions* " ~A~%" new-prop))
    ))



;;;Add-inst-components incrementally instantiates each component schema for all
;;;possible variable bindings.  Process-precs finds the
;;;precondition objects in the previous proposition level.  Add-inst-comp actually
;;;constructs the action and wires it into the plan graph.

(defun add-inst-components (components)
  (dolist (component components)
    (declare (special component))
    (process-precs (clauses (comp-precondition component)) nil nil)))

;;; Given the name of a (type) predicate, return the list of equivalent
;;; predicates under the subtype-of relation.  That is, if the predicate
;;; is TOOL, and the type heirarchy is OBJECT -> PHYSOBJ -> TOOL, then
;;; (sub-names OBJECT) returns (OBJECT PHYSOBJ TOOL), and (sub-names TOOL)
;;; returns (TOOL).
(defun sub-names (predicate)
  (declare (special the-domain))
  (let ((sub-types (assoc predicate (domain-types the-domain))))
    (cond ((null sub-types) (list predicate))
	  (t (cdr sub-types)))))

;; process-precs is the first in a series of three or four functions
;; that result in adding an action to the plan graph.  process-precs
;; ensures that all the action's preconditions are held true in the 
;; previous level's proposition list.  process-precs is recursive, 
;; with the base case being no preconditions.  When there are no
;; preconditions, the next phaces, processing the effects, is entered.

(defun process-precs (preconditions	; The list of preconditions that we're processing.
		      bindings		; The bindings that we've established so far.
		      precondition-props ; The list of preconditions propositions from
					; the previous proposition level that we're 
					; using because they're true.
		      &optional (process-only nil)) ; Only determine if the preconditions
					; are true or false.
  ;; previous-prop-level is declared special in extend-plan-graph
  (declare (special previous-prop-level))

  (if (null preconditions)
      ;; There are no more preconditions -- base case.
      (cond (process-only (list precondition-props))
	    (t
	     ;; Proceed to to the effects handling.
	     ;; Note:  at this moment, we know that the action should
	     ;; be added to the action level -- all of its 
	     ;; preconditions are satisfied.
	     (process-comp-effects bindings precondition-props)))

    ;; pre is the name of the precondition proposition (like, ON or
    ;; CLEAR, or maybe :and).
    ;; keyword is nil if pre is a real domain proposition, or 
    ;; keyword is a keyword like :and, :or, etc. if the precondition
    ;; is compound.
    (let* ((pre (pop preconditions))
           (keyword (when (consp pre) (first pre))))

      (flet 
	  ((check-precondition (pre)	; Define the check-precondition function.
					; check-precondition tries to match pre
					; to all the propositions in the previous level.
					; If there's a match, recursively call 
					; process-precs.
	     ;; Here's where we'll cope with subtypes.  We'll loop
	     ;; through all the possible subtypes of the predicate
	     ;; that we're given.
	     (let ((preconds nil))
	       (dolist (sub-name (sub-names (first pre)))
		 (let ((sub-pre (cons sub-name (cdr pre))))
		   ;; March through the list of propositions in the previous level.
		   ;; We're looking for one that matches our precondition pre.
		   (dolist (precondition-obj previous-prop-level)
		     (let ((new-bindings 
			    (gp-instance? sub-pre (prop-description precondition-obj)
					  bindings)))
		       (unless (or (eql new-bindings +false+)
				   (intersect-p (prop-mutex precondition-obj)
						precondition-props))
			 (setq preconds (append (process-precs preconditions new-bindings
					      (cons precondition-obj precondition-props)
					      process-only)
			       preconds)))))))
	       preconds)))
		     

	;; If the precondition was a compound statement, we handle the 
	;; keyword here.
        (case keyword 
          (= (when (check-eq (second pre) (third pre) bindings)
		(process-precs preconditions bindings precondition-props process-only)))

          (and (process-precs (append (rest pre) preconditions)
                               bindings precondition-props process-only))
          (or 
	   ;; A disjunction.  Try each disjunct in turn.
	   (let ((preconds nil))
	     (dolist (pre (rest pre))
	       (setq preconds (append (process-precs (cons pre preconditions)
				    bindings precondition-props process-only)
		     preconds)))
	     preconds))

	  (forall (process-precs (append (process-quantifier-many pre bindings)
						   preconditions)
				  bindings precondition-props process-only))

	  (exists
	   (let ((preconds nil))
	     (dolist (disjunct (process-quantifier-many pre bindings))
	       ;; Only one of the disjuncts needs to be true.  Exhaustively
	       ;; try all of them.
	       (setq preconds (append (process-precs (cons disjunct preconditions)
				    bindings precondition-props process-only)
		     preconds)))
	     preconds))

	  (nil (process-precs preconditions bindings precondition-props process-only))

	  (not 
	   ;; The only negation that's presently handled is atomic negation and
	   ;; negation of =.  In particular, we don't presently handle negated
	   ;; compound goals, like (not (and ...)) or (not (exists ...)).
	   (if *closed-world*
	       ;; Closed world.
	       
	       ;; We handle inequality testing explicitly with the CHECK-NEQ
	       ;; form.
	       (cond ((eql (first (cadr pre)) '=)
		      (when (check-neq (second (cadr pre)) (third (cadr pre)) bindings)
			(process-precs preconditions bindings precondition-props process-only)))

		     ;; If the negated proposition is in ground form, we can
		     ;; try to use the closed-world assumption to establish
		     ;; neg-prop's.  However, if the prop isn't grounded, we'd
		     ;; have to search through all possible bindings of its variables.
		     ;; Ick.  This is why it's always best to use typing, and to 
		     ;; push the negated propositions to the end of the precondition
		     ;; list.  Even if all the parameters are typed as OBJECT, 
		     ;; their variables will be bound by the time we have to test
		     ;; for the proposition's existence at the previous prop level.
		     ((ground? (plug (second pre) bindings))

		      ;; If PRE isn't in the previous proposition level, then we
		      ;; might be able to make it appear using the closed world 
		      ;; assumption.  If PRE's positive form (aka (not pre)) _doesn't_
		      ;; exist in the initial state, then PRE holds by CWA.  Should
		      ;; this be the case, then we add PRE to all levels of the 
		      ;; planning graph, we add in no-ops for PRE as appropriate, and
		      ;; recompute mutexes between PRE and other props, and between
		      ;; its no-ops and respective {actions,components}.
		      
		      (unless (or (member (plug pre bindings) previous-prop-level
					  :test #'equal :key #'prop-description)
				  (member (plug (second pre) bindings) *initial-props* 
					  :test #'equal :key #'prop-description))
			(add-by-cwa (plug pre bindings) (not process-only)))
			
		      ;; Now try to check the preconditions.
		      (check-precondition pre))


		     ;; It'd be really wierd if we got down here.  Hmmm...
		     ;; The only way we could have gotten this far is if the 
		     ;; proposition in question, pre, had any unbound variables.
		     ;; It shouldn't, however, because all the 'free' variables
		     ;; in an operator's schema, the paramters and VARS, are
		     ;; all bound by typing information.
		     (t (check-precondition pre)))

	     ;; Open world
	     (if (eql (first (cadr pre)) '=)
		 (when (check-neq (second (cadr pre)) (third (cadr pre)) bindings)
		   (process-precs preconditions bindings precondition-props process-only))
	       (check-precondition pre))))

	  ;; We lazily add negated predicates as their positives forms are discovered.
	  (t (when *closed-world*
	       (let ((plugged-pre (plug pre bindings)))
		 ;; Consider only those predicates that are now fully grounded.
		 (when (ground? plugged-pre)
		   ;; If the negative form exists at the previous level, OR if
		   ;; the positive form exists in the initial state, then don't
		   ;; add the negative form.
		   (unless (or (member (invert plugged-pre) previous-prop-level
				       :test #'equal :key #'prop-description)
			       (member plugged-pre *initial-props* 
				       :test #'equal :key #'prop-description))
		     (add-by-cwa (invert (plug pre bindings)) (not process-only))))))
	     (check-precondition pre)))))))


(defun add-by-cwa (prop add-new-nop)
  (declare (special plan-graph previous-prop-level new-action-level new-prop-level))
  ;; Add prop to the first level
  ;; Compute the mutexes
  
  ;; For the remaining levels...
  ;; ...add the no-op
  ;; ...add the new prop
  ;; ...compute the mutexes with the no-op
  ;; ...compute the mutexes with the new prop
  
  (let ((old-prop (new-proposition :description prop))
	(levels (length (pg-action-levels plan-graph))))
    (push old-prop (car (last (pg-proposition-levels plan-graph))))
    (setf *initial-props* (car (last (pg-proposition-levels plan-graph))))

    ;; There aren't any mutexes in the initial state, obviously...

    (dotimes (l levels) ;; 0..levels-1
      (let* ((new-prop (new-proposition :description prop))
	     (new-action (make-instantiated-component :component 'persist
						      :preconditions (list old-prop)
						      :effects (list new-prop))))
	;; Add the new proposition
	(push new-prop (nth (- levels 1 l) (pg-proposition-levels plan-graph)))
	
	;; Add the no-op
	(push new-action (nth (- levels 1 l) (pg-action-levels plan-graph)))
	(push new-action (prop-consumers old-prop))
	(push new-action (prop-establishers new-prop))
	
	
	
	(let ((prev-p-level (nth (+ 1 (- levels 1 l)) (pg-proposition-levels plan-graph)))
	      (new-p-level (nth (- levels 1 l) (pg-proposition-levels plan-graph))))

	  ;; Add the new no-op's mutexes
	  (find-interfering-components-one new-action prev-p-level new-p-level)
	  (find-competing-needs-one old-prop)

	  ;; We don't need to find induced conflicts because
	  ;; the no-op doesn't induce anything.

	  ;; Add the new prop's mutexes
	  (find-exclusive-propositions-one (car new-p-level) (cdr new-p-level)))
	      

	;; Update the old-prop indicator.
	(setq old-prop new-prop)))
  
    ;; Add in the no-op at the current action level
    (when add-new-nop
      (let* ((new-prop (new-proposition :description prop))
	     (new-action (make-instantiated-component :component 'persist
						      :preconditions (list old-prop)
						      :effects (list new-prop))))
	
	;; Add the no-op
	(push new-action new-action-level)
	(push new-action (prop-consumers old-prop))
	(push new-action (prop-establishers new-prop))
	(push new-prop new-prop-level))))


  (setf previous-prop-level (first (pg-proposition-levels plan-graph))))





;;; Called from process-precs.  The antecedant of the component in question
;;; holds at the previous proposition level.  Process the component's
;;; effects and add them to the next proposition level.
(defun process-comp-effects (bindings precondition-props)
  (declare (special component))
  (process-effects (clauses (comp-effect component)) bindings precondition-props nil))


;; Process the effects of a component.  We know that the component is valid
;; at this action level (process-precs said so).  So all that's left is to
;; wade through the effects of the component, reducing the compound effects
;; like AND's and FORALL's into simpler things.
;; Of course, this function is recursive; the base case is when there
;; are no more effects.
(defun process-effects (effects		; The list of effects that we need to process.
			bindings	; The bindings that we establish.
			precondition-props ; The propositions from the previous level
					; that we're using to establish our 
					; preconditions.
			atomic-effects)	; The conjunction of effects, sans keywords
					; and variables.
  ;; previous-prop-level is declared special in extend-plan-graph
  (declare (special previous-prop-level))

  (if (null effects)
      (when atomic-effects		; Base case.
	(add-inst-comp bindings precondition-props atomic-effects))

    (let* ((effect (pop effects))	; Grab the next effect in line.
           (keyword (when (consp effect) (first effect))))

      ;; Handle compound effects.  We've already taken care of a few such
      ;; effects, namely WHEN effects.  The WHEN effects were dealt with
      ;; at the time that the factored expansion took place.
      ;;
      ;; The only compound effects that we're concerned with are AND
      ;; and FORALL
      (case keyword
        (and
         (process-effects (append (rest effect) effects) 
                          bindings precondition-props atomic-effects))
	(forall
	 (process-effects (append (process-quantifier-many effect bindings)
				  effects)
			  bindings precondition-props atomic-effects))

	;; It's a simple proposition.  Just add it to the list of
	;; atomic effects and recurse.
        (t (process-effects effects bindings precondition-props 
                            (cons effect atomic-effects)))))))


;; Given a {universal,existential} quantification, find all the possible
;; ways to satisfy the antecedant clause.  For each of those ways, generate 
;; the effects of the quantifier, and append them into a big list.  
(defun process-quantifier (quantification ; The quantified proposition, e.g.
					; (forall (?x - blob) (Q ?x)).
			   init-props	; The propositions from level 0 that
					; remain to be examined.
			   bindings)	; The bindings that we've established 
					; so far.

  (if (null init-props)         ; Base case -- no propositions left.
      nil
    (let* ((prop (car init-props)) ; The next proposition to consider
           (new-bindings (instance-with-subtypes? (second quantification) 
                                    (prop-description prop) bindings)))
      (if (eql new-bindings +false+)    ; This proposition doesn't match.
                                        ; Proceed with the remaining propositions.
          (process-quantifier quantification (cdr init-props) bindings)
                                        ; This proposition did match.  Use the
                                        ; new binding to plug the variables in the 
                                        ; quantifier's effects, and continue with 
                                        ; our list of propositions.
        (append (plug (rest (rest quantification)) new-bindings)
                (process-quantifier quantification (cdr init-props) bindings))))))
  

(defun instance-with-subtypes? (var-exp constant-exp bindings)
  (do* ((sub-types (sub-names (car var-exp)) (cdr sub-types))
	(sub-type (car sub-types) (car sub-types))
	(new-bindings (instance? (cons sub-type (cdr var-exp))
				 constant-exp bindings)))
      ((or (null (cdr sub-types)) (not (eq new-bindings +false+)))
       new-bindings)))
      

;;; Handle the case of an {existential,universal} quantifier with multiple
;;; variables.
;;; Returns a list of propositions, with each member representing one
;;; instantiation of the quantified variables for the quantifier's effect.
;;;
;;; process-quantifier-many actually handles both universal and existential
;;; quantification.  The only real difference is whether the result is a
;;; conjunction or a disjunction.
(defun process-quantifier-many (quantification ; The quantified proposition:
					; (forall (?x - obj) (P ?x))
				bindings) ; The bindings that we've 
					; established so far.
  
  ;; Convert the PDDL-style type list into a list of type/var pairs.
  (let ((var-clauses (first (process-typed-list (second quantification) nil nil nil)))
        (effect-list (list (third quantification)))) ; The list that we'll return.

    (dolist (var-clause var-clauses)    ; Consider each type/var pair in turn
      (cond ((eq (first var-clause) 'or) 
	     (let ((effects nil))
	       (dolist (v-c (cdr var-clause))
		 (setq effects 
		   (append effects (process-quantifier 
				    ; The 'forall is just a placeholder
				    (append (list 'forall v-c) effect-list)
				    *objects* bindings))))
	       (setq effect-list effects)))
            (t (setq effect-list (process-quantifier (append (list 'forall var-clause) 
                                                            effect-list)
						    *objects* bindings)
                     ))))
    effect-list))


;;; Add the instantiated component to the current component level in
;;; the planning graph.
(defun add-inst-comp (bindings		; The bindings that we needed to 
					; instantiate this component.
		      precondition-props ; The propositions from the previous level 
					; that were required to use this component.
		      atomic-effects)	; The effects of this component.  May contain
					; variables.

  (declare (special component new-action-level))
  (let* ((effect-props (loop for effect in atomic-effects collect 
                             (new-prop (plug effect bindings))))
	  (inst-comp (make-instantiated-component :component component
						  :bindings bindings
						  :preconditions precondition-props
						  :effects effect-props)))
    (dolist (precondition precondition-props)
      (push inst-comp (prop-consumers precondition)))
    (dolist (effect effect-props)
      (push inst-comp (prop-establishers effect)))
    (push inst-comp new-action-level)
    (tracer *trace-gp-actions* "~&~10T~A --> ~{~a ~}" inst-comp effect-props)))



;;;;;;;;;;;;;;;;;;;;;;;;; MUTEX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;The mutex operations have the largest effect on performance in this
;;;implementation.  Start here if you want to optimize performance.

(declaim (inline mutexp))
(defun MUTEXP (node1 node2)
  (member node1 (node-mutex node2) :test #'eq))

(defun ADD-MUTEX (node1 node2)
  (unless (mutexp node1 node2)
    (push node1 (node-mutex node2))
    (push node2 (node-mutex node1))))

;;;Note that proposition lookups and add-mutex are the inner loops of this
;;;routine.

(defun DERIVE-MUTEX (previous-prop-level new-prop-level new-action-level)
  "Find the mutex relations when we're using the Factored Expansion method."

  (tracer *trace-gp-mutex* "~%~4Tfinding interfering components")
  (time-if nil (find-interfering-components new-action-level 
					    previous-prop-level new-prop-level))
  (tracer *trace-gp-mutex* "~%~4Tfinding competing needs")
  (time-if nil (find-competing-needs previous-prop-level))

  (tracer *trace-gp-mutex* "~%~4Tfinding induced component conflicts")
  (time-if nil (find-induced-conflicts new-action-level previous-prop-level))

  (tracer *trace-gp-mutex* "~%~4Tfinding exclusive propositions")
  (time-if nil (find-exclusive-propositions new-prop-level)))



(defun FIND-INTERFERING-COMPONENTS (new-action-level previous-prop-level new-prop-level)
  ;; Mark actions as interfering if
  ;;     1. one action clobers the other's preconditions or
  ;;	 2. the actions have opposite effects.
  (dolist (inst-comp new-action-level)
    (find-interfering-components-one inst-comp previous-prop-level new-prop-level)))

(defun find-interfering-components-one (inst-comp previous-prop-level new-prop-level)
  (dolist (effect (inst-comp-effects inst-comp))
    (let* ((description (prop-description effect))
	   (inverted (invert description)))

      ;; Find instantiated components that have opposite effects
      (when (inverted? description)
	(let ((deleted-effect
	       (find-prop inverted new-prop-level)))
	  (when deleted-effect
	    (add-mutex effect deleted-effect)
	    (dolist (establisher (prop-establishers deleted-effect))
	      (when (or (eql (inst-comp-component inst-comp) 'persist)
			(eql (inst-comp-component establisher) 'persist)
			(not (eql (comp-operator (inst-comp-component inst-comp))
				  (comp-operator (inst-comp-component establisher)))))
		(add-mutex inst-comp establisher))))))

      ;; Find instantiated components that clobber preconditions
      (let ((deleted-precondition
	     (find-prop inverted previous-prop-level)))
	(when deleted-precondition
	  (dolist (consumer (prop-consumers deleted-precondition))
	    ;; Either the components must be different, or have different bindings
	    (when (and (not (eq inst-comp consumer))
		       (or (eql (inst-comp-component inst-comp) 'persist)
			   (eql (inst-comp-component consumer) 'persist)
			   (not (eql (comp-operator (inst-comp-component inst-comp))
				     (comp-operator (inst-comp-component consumer))))
			   (if *ground*
			       (not (equal (comp-bindings (inst-comp-component inst-comp))
					   (comp-bindings (inst-comp-component consumer))))
			       (not (equal (inst-comp-bindings inst-comp)
					   (inst-comp-bindings consumer))))))
	      (add-mutex inst-comp consumer))))))))


;;; Mark actions with incompatable preconditions as mutually exclusive
;;; This function does about twice as much work as it needs to.
(defun FIND-COMPETING-NEEDS (previous-prop-level)
  (dolist (prop previous-prop-level)
    (find-competing-needs-one prop)))

(defun find-competing-needs-one (prop)
    (dolist (mutex-prop (prop-mutex prop))
      (dolist (action1 (prop-consumers prop))
        (dolist (action2 (prop-consumers mutex-prop))
          (add-mutex action1 action2)))))


;; Does component Ci entail Cj at this level?  Here's what we do:
;; consider each of the preconditions of Ci.  For each precondition,
;; strike out all the propositions in the proposition level that are
;; mutex with that precondition.  All that will be left in the 
;; list of propositions, then, are props that are consistent with 
;; Ci.  Now, see if the _negation_ of Cj's precondition _can_ be
;; satisfied.  If so, then Ci is _not_ entailed by Cj.  The reason
;; why we take the negation of Cj's precondition is so that we can
;; reuse the logic from the process-precs function here.
(defun COMPONENT-ENTAILS (Ci Cj propositions)
  (let ((state-of-world nil))		; We're going to build the state of the world
					; that's consistent with Ci's preconditions


    (dolist (prop2 propositions)
      (let ((is-mutex nil))
	(dolist (prop1 (inst-comp-preconditions Ci))
	  (when (mutexp prop1 prop2)
	    (setq is-mutex t)))
	(unless is-mutex (push prop2 state-of-world))))


    ;; I now have a list of propositions that are consistent with 
    ;; allowing Ci to be an action at this next level.  The question, now
    ;; is whether the negation of Cj's preconditions are consistent
    ;; with this world view.  If it _can_ be satisfied, then there is 
    ;; some way to avoid firing off Cj when Ci is chosen.  Thus, Ci would
    ;; _not_ entail Cj.
    
    ;; Ci entails Cj iff the negation of Cj's antecedant is _not_
    ;; consistent with the world, AND Cj's antecedant _is_ consistent.

    (and (not (consistent? (list (cons 'or (mapcar #'(lambda (precond)
						       (invert (prop-description precond)))
						   (inst-comp-preconditions Cj))))
			   (mapcar #'prop-description state-of-world)))
	 (consistent? (list (cons 'and (mapcar #'prop-description
					       (inst-comp-preconditions Cj))))
		      (mapcar #'prop-description state-of-world)))))

;;; Given a set of list of propositions and the current state of
;;; the world, determine if the list of props is consistent with
;;; the world.
(defun CONSISTENT? (props state-of-world)
  (if (null props)
      t
    (let* ((prop (pop props))
	   (keyword (when (consp prop) (first prop))))
      (flet ((check-proposition (prop)
	       (let ((found-matching-prop nil))
		 (dolist (world-prop state-of-world)
		   (unless (eql (gp-instance? prop world-prop nil) +false+)
		     (setq found-matching-prop (consistent? props state-of-world))))
		 found-matching-prop)))
	
	(case keyword
	  (= (when (check-eq (second prop) (third prop) nil)
		(consistent? props state-of-world)))
	  (and (consistent? (append (rest prop) props) state-of-world))
	  (or (let ((found-matching-disjunct nil))
		 (dolist (prop (rest prop))
		   (when (check-proposition prop) (setq found-matching-disjunct t)))
		 found-matching-disjunct))
	  (forall (consistent? (append (process-quantifier-many prop nil)
					props) state-of-world))
	  (not 
	   (if (eql (first (cadr prop)) '=)
	       (when (check-neq (second (cadr prop)) (third (cadr prop)) nil)
		 (consistent? props state-of-world))
	     (check-proposition prop)))
	  (t (check-proposition prop)))))))




;; Mark components Cm and Cn as mutex if there is a Ck such that:
;; Cm induces Ck at this level, and
;; Ck is mutex with Cn
(defun FIND-INDUCED-CONFLICTS (new-action-level previous-prop-level)
  ;; Idea:  As we go along, generate, for each Ck, a list of those
  ;; Cns that are mutex with Ck.

  (let ((mutex-Ck-Cn (mapcar #'(lambda (Ck) (cons Ck nil)) new-action-level))
	(mutexes-are-valid (mapcar #'(lambda (Ck) (cons Ck nil)) new-action-level)))

    (dolist 
	(Cm (remove-if #'(lambda (inst-comp)
			   (or (eql (inst-comp-component inst-comp) 'persist)
			       (= (length (op-components 
					   (comp-operator (inst-comp-component inst-comp))))
				  1)))
		       new-action-level))

      (dolist (Ck new-action-level)
	(when (induces? Cm Ck previous-prop-level)
	  (unless (cdr (assoc Ck mutexes-are-valid))
	    ;; Compute the set of Cn's that are mutex with this Ck
	    (let ((Cns (assoc Ck mutex-Ck-Cn)))
	      (dolist (Cn new-action-level)
		(when (mutexp Ck Cn)
		  (push Cn (cdr Cns))))
	      (unless (equal Cns (assoc Ck mutex-Ck-Cn))
		(break))
	      )
	    (setf (cdr (assoc Ck mutexes-are-valid)) t))
	  (dolist (Cn (cdr (assoc Ck mutex-Ck-Cn)))
	    (add-mutex Cm Cn)))))))



;;;
;;; Does component Ci induce Cj at this level?
;;;
(defun INDUCES? (Ci Cj previous-prop-level)
  (and (not (eql Ci Cj))		; Can't induce yourself.
       (not (eql (inst-comp-component Cj) 'persist)) ; Persistence components don't
					; induce anything.
       (eql (comp-operator (inst-comp-component Ci)) ; They must come from the same 
	    (comp-operator (inst-comp-component Cj))) ; operator.
       (if *ground*
	   (equal (comp-bindings (inst-comp-component Ci)) ; They have the same
		  (comp-bindings (inst-comp-component Cj))) ; variable bindings
	 (equal (inst-comp-bindings Ci)	; They have the same variable bindings.
		(inst-comp-bindings Cj)))
       (component-entails Ci Cj previous-prop-level)))


;;; Propositions are also exclusive if all ways of obtaining 
;;; them are mutually exclusive
(defun FIND-EXCLUSIVE-PROPOSITIONS (new-prop-level)
  (loop for (prop1 . rest) on new-prop-level do
	(find-exclusive-propositions-one prop1 rest)))

(defun find-exclusive-propositions-one (prop1 rest)
  (dolist (prop2 rest)
    (unless (mutexp prop1 prop2)
      (when (loop for act1 in (prop-establishers prop1) always
		  (loop for act2 in (prop-establishers prop2) always
			(mutexp act1 act2)))
	(add-mutex prop1 prop2)))))



;;;-----------------------The search engine-----------------------
;;;
;;; do a backward chaining search once all goal nodes occur in current level
;;; to see if they really are consistent

;;;This function finds any satisficing plan in the current plan graph.
;;;The search function does not incorporate all of the suggestions in
;;;the original paper.
(defun FIND-PLANS-IN-PG (plan-graph all-plans)
  (time-if nil
    (let* ((problem (pg-problem plan-graph))
	   (prop-levels (pg-proposition-levels plan-graph))
	   (previous-prop-level (first prop-levels)))
      (declare (special previous-prop-level))
      (let* ((goals (goal-clauses (list (problem-goal problem)) 
				  (car (last (pg-proposition-levels plan-graph)))))
	     (goal-list (process-precs goals nil nil t)))

	(loop for goal-props in goal-list thereis
	  (when goal-props
	    (if all-plans
		(find-all-plans-in-pg1 goal-props (pg-action-levels plan-graph)
				       prop-levels (pg-goal-caches plan-graph) 0)
	      (find-plan-in-pg1 goal-props (pg-action-levels plan-graph)
				prop-levels (pg-goal-caches plan-graph) 0))))))))

(defun FIND-PLAN-IN-PG1 (goal-props action-levels prop-levels goal-caches level)
  (declare (special level))
  (tracer *trace-gp-bc* "~&~4T~Aseeking:  ~{~&~a ~}~%" 
          (make-string (* 2 level) :initial-element #\ )
          goal-props)
  (cond ((null goal-props) (list t))
	((null action-levels) (list t))
        ((memoized-failure-p goal-props (first goal-caches))
         (tracer *trace-gp-bc* " -- failure: memoized goals")
         nil)    ; at the very first level of the graph
        (t (let ((plan (assign-goals goal-props goal-props action-levels prop-levels
                                     goal-caches nil nil)))
             (if (null plan)
               (memoize-as-failure goal-props (first goal-caches)))
             plan))))

(defun FIND-ALL-PLANS-IN-PG1 (goal-props action-levels prop-levels goal-caches level)
  (declare (special level))
  (tracer *trace-gp-bc* "~&~4T~Aseeking:  ~{~a ~}" 
          (make-string (* 2 level) :initial-element #\ )
          goal-props)
  (cond ((null goal-props) '(t))
        ((null action-levels)  '(t))
        ((memoized-failure-p goal-props (first goal-caches))
         (tracer *trace-gp-bc* " -- failure: memoized goals")
         nil)    ; at the very first level of the graph
        (t (let ((plans (assign-all-goals goal-props action-levels prop-levels
                                          goal-caches nil nil)))
             (if (null plans)
               (memoize-as-failure goal-props (first goal-caches)))
             plans))))

(defun MEMOIZED-FAILURE-P (goals cache)
  (gethash (canonical-goal-order goals) cache))

(defun MEMOIZE-AS-FAILURE (goals cache)
  (setf (gethash (canonical-goal-order goals) cache) t))

(defun CANONICAL-GOAL-ORDER (goals)
  (sort (mapcar #'prop-number goals) #'<))



;;;
;;; assign-goals searches over all possible ways to establish the
;;; goal propositions with actions/components.
;;;
(defun ASSIGN-GOALS (goal-props all-the-goals 
		     action-levels prop-levels goal-caches 
		     next-goals actions)
  (declare (special level the-components))

  (cond ((null goal-props)		; No more goals are remaining to be
					; satisfied.  Proceed to the next level.
	 (let ((plan (find-plan-in-pg1 next-goals
				       (rest action-levels)
				       (rest prop-levels)
				       (rest goal-caches)
				       (1+ level))))
	   (when plan
	     (cons actions plan))))
	(t (multiple-value-bind (g remaining-goal-props)
	       (best-goal-prop goal-props)

	     ;; Try to satisfy goal g.  Once that is done, recursively
	     ;; try to satisfy remaining-goal-props.

	     ;; Why would there be a goal with no establishers?  That'd
	     ;; be something like a proposition node at some level i > 0
	     ;; that has no establishing action.  Hmmm...

	     (cond ((null (prop-establishers g))
		    (assign-goals remaining-goal-props all-the-goals
				  action-levels prop-levels goal-caches
				  next-goals actions))
		   (t


		    ;; Loop until we find an establisher for which the 
		    ;; remainder of the plan can be determined.
		    (loop for establisher in (prop-establishers g) thereis
			  ;; !!! What if there is no establisher for this goal???
	      
			  ;; If the proposed establisher is mutex with any of the 
			  ;; actions/components already chosen, then this establisher
			  ;; won't work.
			  (unless (member establisher actions :test #'mutexp)
			    ;; Try to assign actions/components to the remaining goals.
			    (tracer *trace-gp-bc* "~&~A : ~A~%" g establisher)

			    ;; If we're in the world of components, then we have to
			    ;; be a little more careful.  If we select component Ci
			    ;; derived from operator O, then we need to consider all
			    ;; the other components Cj from operator O.  Remember that
			    ;; we can't execute _components_, only operators.  Thus,
			    ;; if there's a component Cj in O that negates one of our
			    ;; goals, then we need to be sure that Cj never fires
			    ;; off.  Hence, our line of attack:  For each component Cj
			    ;; of the operator, determine if the component's effects
			    ;; are consistent with the goals at this level.  If they 
			    ;; are, we're okay.  If not, then add the negation of the
			    ;; antecedant of Cj to the goal list for the next lower
			    ;; goal level.  Of course, the antecedant is probably a 
			    ;; conjunction, so the negation will be a disjunction of
			    ;; several negated clauses.
			    (let* ((component (inst-comp-component establisher))
				   (bindings (if *ground*
						 (when (not (eql component 'persist))
						   (comp-bindings component))
					       (inst-comp-bindings establisher))))
			      (assign-goals-confrontation-helper
			       establisher
			       ;; Find the other components of the same 
			       ;; operator.  Note that we're scaning through
			       ;; the list of *all* components, not simply 
			       ;; those that were instantiated at the last 
			       ;; action level.  A component might be induced 
			       ;; by an action at this level and might need 
			       ;; to be prevented from firing.  If the 
			       ;; establishing component is a no-op, then 
			       ;; there are no such other components. 
			       ;; Otherwise, select just the sibling 
			       ;; components.
			       (if (eql component 'persist) nil
				 (remove-if-not 
				  #'(lambda (comp)
				      (and (not (eql comp component))
					   (if *ground* (equal (comp-bindings comp) bindings) t)
					   (eql (comp-operator comp) (comp-operator component))))
				  the-components))
			       remaining-goal-props
			       all-the-goals action-levels prop-levels goal-caches
			       next-goals actions))))))))))
			 
(defun best-goal-prop (goals)
  (if *dvo*
      (best-goal-prop-helper (cdr goals) (car goals) 
			     (length (prop-establishers (car goals))) nil)
    (values (car goals) (cdr goals))))

;; Find the goal that has the fewest establishers.
(defun best-goal-prop-helper (goals best-goal num-establishers other-goals)
  (cond ((null goals) (values best-goal other-goals))
	((< (length (prop-establishers (car goals))) num-establishers)
	 ;; The next goal in goals is a better choice that best-goal.
	 (best-goal-prop-helper (cdr goals) (car goals)
				(length (prop-establishers (car goals)))
				(cons best-goal other-goals)))
	(t (best-goal-prop-helper (cdr goals) best-goal num-establishers
				  (cons (car goals) other-goals)))))


(defun ASSIGN-GOALS-CONFRONTATION-HELPER (establisher other-components
					   remaining-goal-props all-the-goals 
					   action-levels prop-levels goal-caches
					   next-goals actions)
  (let ((new-goals (inst-comp-preconditions establisher))
	(component (pop other-components))
	(bindings (if *ground*
		      (comp-bindings (inst-comp-component establisher))
		    (inst-comp-bindings establisher))))

    (if (null component)
	(assign-goals remaining-goal-props
		      all-the-goals
		      action-levels prop-levels goal-caches
		      (union new-goals next-goals)
		      (union (list establisher) actions))

      ;; Here's where the fun begins.  The list of components in 
      ;; other-components are the components that belong to the same 
      ;; operator as establisher.
    
      ;; We have to decide here, for each of those components, if it's okay 
      ;; for the component to fire.  That is, if the effect of the component
      ;; firing will negate one of our goals, then we can't let that component
      ;; to be activated.  To avoid being activated, we have to ensure that 
      ;; at least one of its antecedants is not true by this time in the 
      ;; plan.
    
      ;; First, let's see if the component would cause us any grief.
      (if (consistent? (list (cons 'or (mapcar #'(lambda (prop)
						   (plug (invert prop) bindings))
					       (clauses (comp-effect component))
					       )))
		       (mapcar #'prop-description all-the-goals))
	  
	  (progn

	    ;; Yes, this component _does_ have one or more effects that are at odds
	    ;; with our goals.
	
	    ;; What we'll have to do, then, is search through all the possible 
	    ;; ways to prevent the antecedant from being held true.
	    (do* ((antecedant-list (conjuncts (comp-precondition component)) 
				   (cdr antecedant-list))
		  (antecedant-pred (car antecedant-list) (car antecedant-list))
		  (plan nil))
		((or (null antecedant-list)
		     plan)
		     plan)
	      ;; For each antecedant clause, search for its negation 
	      ;; in the list of propositions at the next level.  If
	      ;; the negation can be found, we'll add it to the list of
	      ;; goals to be satisfied at the next lower level.

	      (let* ((neg-predicate (plug (invert antecedant-pred) bindings))
		     (neg-prop (find-if #'(lambda (prop)
					    (equal (prop-description prop)
						 neg-predicate))
					(first (rest prop-levels))))
		     (neg-prop-persist (find-if #'(lambda (inst-comp)
						    (and (eq (inst-comp-component inst-comp)
							     'persist)
							 (eq (car (inst-comp-preconditions 
								   inst-comp))
							     neg-prop)))
						(first action-levels))))
		
		;; We've found the negation of a particular antecedant of this offending
		;; component.  Let's try to build a plan that includes establishing this
		;; negative proposition.  If it succeeds (returns non-nil), we're happy.
		;; If doing so fails, plan remains nil, and we iterate to try the next
		;; clause in the antecedant to negate.


		;; What if I first checked if the negated goal will be impossible
		;; to satisfy because it's mutex with other goals at the next
		;; level?

		(cond ((or (and (eq (first neg-predicate) '=)
				(check-eq (second neg-predicate) (third neg-predicate) nil))
			   (and (eq (first neg-predicate) 'not)
				(eq (first (cdr neg-predicate)) '=)
				(check-neq (second (cdr neg-predicate)) 
					   (third (cdr neg-predicate)) nil)))
		       (setq plan (assign-goals-confrontation-helper 
				   establisher other-components 
				   remaining-goal-props all-the-goals
				   action-levels prop-levels goal-caches
				   next-goals actions)))
		      (neg-prop
		       
		       (unless 
			   (or (find-if #'(lambda (prop) (mutexp neg-prop prop)) 
					next-goals)
			       (find-if #'(lambda (act) (mutexp neg-prop-persist act)) 
					actions))

			 ;; We need to:
			 ;; (1) Find the persist component for this negated prop.
			 ;; (2) Determine if the persist component is mutex with
			 ;;     any of the other components at this level.
			 ;; (3) If it is, we fail.  Else, we add the persist component
			 ;;     to the action list at this level.
			 ;; (4) Add the negated goal to the goal list at the next
			 ;;     lower level.
		    
			 ;; Why do we need to do (1) - (3)?
			 ;; Consider two actions A and B.  A establishes property R,
			 ;; B establishes Q and, conditionally on R, establishes S.
			 ;; If the goal at this level is R and Q and not S, we want 
			 ;; B to fire _first_, then A to fire.  Suppose (1) - (3) 
			 ;; weren't executed.  We would consider B at this level, and
			 ;; see that it can establish Q.  We see that R => S, which 
			 ;; is bad, so we post !R as a goal at the prev. level.  Then
			 ;; we consider A at this level.  A establishes R, which is 
			 ;; good for the goal.  If B is ordered before A in this step,
			 ;; we're happy.  But if A is ordered before B, then A's R will 
			 ;; cause the R => S component to fire, and we won't have our
			 ;; goals.  Yes, !R was added as a goal at the prev. level,
			 ;; but A, at _this_ level, established !R.  What we really 
			 ;; needed was some way to say, yes, !R will be established
			 ;; at the prev. level, and it should remain true throughout 
			 ;; this level.  That is precisely what the presist-!R component
			 ;; will do.


			 (setq plan (assign-goals-confrontation-helper 
				     establisher other-components 
				     remaining-goal-props all-the-goals
				     action-levels prop-levels goal-caches
				     (union (list neg-prop) next-goals) 
				     (union (list neg-prop-persist) actions)))))))))
	
	;; No, this component didn't conflict with our goals.  But what of the other 
	;; components?
	(assign-goals-confrontation-helper establisher other-components 
					   remaining-goal-props all-the-goals
					   action-levels prop-levels goal-caches
					   next-goals actions)))))



(defun ASSIGN-ALL-GOALS (goal-props action-levels prop-levels goal-caches 
                                    next-goals actions)
  (declare (special level))
  (if goal-props
    (loop for establisher in (prop-establishers (first goal-props)) append
          (unless (member establisher actions :test #'mutexp)
            (assign-all-goals (rest goal-props)
                              action-levels prop-levels goal-caches
                              (union (inst-comp-preconditions establisher)
				     next-goals)
                              (union (list establisher) actions))))
    (let ((plans (find-all-plans-in-pg1 next-goals
                                        (rest action-levels)
                                        (rest prop-levels)
                                        (rest goal-caches)
                                        (1+ level))))
      (when plans
        (mapcar #'(lambda (plan)  (cons actions plan)) plans)))))

;;;--------------------PRUNING NULL STEPS--------------------
;;; I don't think null steps are very interesting, so I prune them
;;; from the output - the only actions returned are those that really
;;; do something
(defun PRUNE-PERSISTS (plan)
  (loop for plan-level in plan
        unless (eq plan-level t)
        collect
        (loop for step in plan-level
	    unless (eq 'persist (inst-comp-component step))
              collect step)))


(defun COMPONENTS-TO-ACTIONS (plans)
  ;; For each plan, determine which _actions_ are really taken
  ;; (as opposed to components), and return the appropriate list
  ;; of instantiated operators.

  (cond ((null plans) nil)
	(t (let ((plan (pop plans)))
	     ;; Unroll this plan, and recurse on the rest.
	     (cons (components-to-actions-one plan)
		   (components-to-actions plans))))))

  
(defun COMPONENTS-TO-ACTIONS-ONE (plan)
  ;; For now, let's just do a one-to-one translation from the 
  ;; components to the actions.
  
  (cond ((null plan) nil)
	(t (let* ((step (pop plan))
		  (op (comp-operator (inst-comp-component step)))
		  (bindings (if *ground*
				(comp-bindings (inst-comp-component step))
			      (inst-comp-bindings step)))
		  (this-step (cons (op-name op)
				   (plug (op-variables op) bindings)))
		  (rest-of-plan (components-to-actions-one plan)))
	     ;; If we're at the end of the plan, or if this component
	     ;; is the only representative of its action, then add
	     ;; this step to the plan.
	     (if (or (null rest-of-plan)
		     (not (find-if #'(lambda (s) (equal s this-step))
				   rest-of-plan)))
		 (cons this-step rest-of-plan)
	       rest-of-plan)))))
