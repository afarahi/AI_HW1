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

;;; $Id: sgp.lisp,v 1.9 2000/01/15 00:00:56 corin Exp $

(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))


;;; This loads all code into the gp package which uses the domains package
;;;  (use-package :domains)  already done in defpackage for :gp in loader
(in-package :sgp)


;;;Timing macro.
(defmacro TIME-IF (flag . code)
  (if flag
    `(time . ,code)
    `(progn . ,code)))

(defvar *trace-gp-top-level* nil)
(defvar *trace-gp-contexts* nil)
(defvar *trace-gp-actions* nil)
(defvar *trace-gp-print-detailed-actions* nil)
(defvar *trace-gp-mutex* nil)
(defvar *trace-gp-bc* nil)
(defvar *trace-gp-csp* nil)
(defvar *stat-gp-graph-size* t)
(defvar *stat-gp-time-expansion* nil)
(defvar *stat-gp-time-bc* nil)
(defvar *stat-gp-time-dnf* nil)
(defvar *stat-gp-num-contexts* t)

(defvar *use-forward-checking* nil)
(defvar *use-induced-mutexes* t)

(defmacro trace-gp (&rest args)
  (dolist (arg args)
      (set (intern (format nil "*TRACE-GP-~A*" arg)) t)))

(defmacro untrace-gp (&rest args)
  (dolist (arg args)
    (set (intern (format nil "*TRACE-GP-~A*" arg)) nil)))

(defmacro TRACER (exp &rest args)
  `(when ,exp
     (format t ,@args)))

;;;------------------------structures---------------------------

;;;The plan graph.
;;;After n time periods, there are n+1 proposition levels, n+1 goal-caches
;;;and n action-levels.  action and proposition levels are ordered from the
;;;most recent to the oldest. The goal caches are used for memoizing impossible
;;;goal combinations.
(defstruct (PLAN-GRAPH (:conc-name pg-) (:print-function print-plangraph))
  (static-props nil)
  (proposition-levels nil)
  (action-levels nil)
  (goal-caches nil)
  (static-inits nil)
  problem
  domain
  worlds				; Possible worlds list.  Each
					; element is a list of propositions
					; that were true in this PW at level
					; 0.
  contexts)                             ; number of possible worlds

(defun PRINT-plangraph (plan-graph &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "<Plan-Graph:")
  (format stream "~%   Static-props: ~A" (pg-static-props plan-graph))
  (loop for action-level in (pg-action-levels plan-graph)
        as  i from (length (pg-action-levels plan-graph)) downto 1
        as  prop-level in (pg-proposition-levels plan-graph)
        do
        (format stream "~%   Prop-Level ~A: ~A" i prop-level)
        (format stream "~%   Action-Level ~A: ~A" i action-level))
  (format stream "~%   Prop-Level 0: ~A>" (car (last (pg-proposition-levels
                                                   plan-graph)))))

;;; 

;;; What are clusters?  Technically, a cluster is a list of propositions 
;;; along with a description of the proposition.  Here's the idea: recall
;;; that we have two shades of propositions on GP:  there's the data 
;;; structure proposition, and there's the list of symbols that's the 
;;; prop's _description_.  In CGP, each prop data structure also has an
;;; associated context -- the world (number) in which it lives.  Instead
;;; of having each CGP proposition (i.e., prop/context pair) laying about 
;;; in the planning graph, we encapsulate all the prop/context pairs into 
;;; one data structure -- the cluster.
;;;
;;; A cluster, then, is a (world-independent) description of a proposition,
;;; along with a list of instantiations of that prop in the possible worlds.
(defstruct (cluster (:print-function print-cluster))
  (description nil)
  (props nil))

(defun PRINT-cluster (cluster &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "~A" (cluster-description cluster))
  (dolist (node (cluster-props cluster))
    (print-context (node-context node) stream)))

;;; base structure for both proposition and action nodes in the graph
(defstruct (NODE (:print-function print-node))   
  (mutex nil)
  (context nil)
  )

(defun PRINT-node (node &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "~A" (node-description node))
  (print-context (node-context node) stream))

(defun node-description (node)
  (typecase node
    (proposition
     (prop-description node))
    (action
     (act-description node))))

(defun PRINT-context (context stream)
  (format stream "{~A}" context))

;;;Propositions need to be linked to each other (via mutex relationships) and to
;;;the preceding action level (in order to find mutex relationships.).  Props
;;;are linked to the succeeding action level in order to efficiently identify
;;;action mutex relationships.
(defstruct (PROPOSITION (:include node) (:conc-name prop-)) 
  (establishers nil)                    ; actions that established this
  (consumers nil)                       ; actions that use this
  (cluster #() :type (vector cluster *))
  (number 0)                           ; for sorting purposes
  (static nil))

(defun prop-description (prop)
  (cluster-description (prop-cluster prop)))

(defvar *prop-number*)

(defmacro new-proposition (&rest args)
  `(make-proposition ,@args :number (incf *prop-number*)))

;;;Actions need to be linked to each other (via mutex relationships) and are
;;;linked to the surrounding prop levels in much the same way as propositions.
(defstruct (ACTION (:include node) (:conc-name act-)
                   (:print-function print-action))
  effects
  preconditions
  operator
  bindings
  (aspects nil)                         ; other conditional effects for the action
  (inducers nil))                      ; aspects that induce this one (at this level)


(defun print-action (a &optional (stream t) depth)
  (declare (ignore depth))
  (if *trace-gp-print-detailed-actions*
      (format stream "~A [~A => ~A]" (act-description a)
	      (act-preconditions a) (act-effects a))
    (if (act-effects a)
	(format stream "~A" (act-description a))
      (format stream "$~A$" (act-description a)))))

(defun act-description (action)
  (let ((operator (act-operator action)))
    (if (eq operator 'persist)
      (list 'Persist (prop-description (first (act-preconditions action))))
      (cons (op-name operator)
            (plug (op-variables operator) 
                  (act-bindings action))))))

;;;----------------------THE PLANNER------------------------
;;;
;;; This is the top level interface

(defparameter *GP-CAPABILITIES* 
  '(:strips :equality :typing :conditional-effects :disjunctive-preconditions 
    :uncertainty :sensing))
(defparameter *REFORMULATE* nil)


;;; This is the main entry point to the planner.
;;; The first argument should be an instance of a problem struct (see 
;;; domains.lisp) and the second argument is the max number of levels to 
;;; consider. 
(defun PLAN (problem &key (domain nil) (levels 10) (all-plans nil))
  (setq problem (get-problem problem))
  (unless problem (format t "Problem not defined") (return-from plan nil))
  (setq domain (get-domain (or domain (problem-domain problem))))
  (unless domain (format t "Domain not defined") (return-from plan nil))
  (check-requirements *gp-capabilities* problem domain)
  (when (and *use-forward-checking* all-plans)
    (format t "~&*** Sorry, forward checking is not available when finding all plans.~%")
    (setf *use-forward-checking* nil))
  (if *use-forward-checking*
      (format t "~&Using forward checking CSP solver~%")
    (format t "~&Using backtracking CSP solver~%"))
  (if *use-induced-mutexes*
      (format t "~&Using induced mutexes~%")
    (format t "~&Not using induced mutexes~%"))
  (let ((plan-graph (initialize-plan-graph problem domain))
        (plans nil))
    (format t "~%Levels")
    (loop for level from 1 upto levels 
          until plans do
          (tracer *trace-gp-top-level* "~%")
          (format t "~&***** ~A *****~%" level)
	  (cond (*stat-gp-time-expansion*
		 (format t "~&Expanding graph...~%")
		 (time (extend-plan-graph plan-graph)))
		(t (extend-plan-graph plan-graph)))
	  (tracer *stat-gp-graph-size* "~&Actions:  ~A  Propositions:  ~A~%" 
		  (length (car (pg-action-levels plan-graph)))
		  (length (car (pg-proposition-levels plan-graph))))
          (tracer *trace-gp-top-level* "~%~2TChecking goals")
	  (cond (*stat-gp-time-bc*
		 (format t "~&Backward searching...~%")
		 (time (setq plans (find-plans-in-pg plan-graph all-plans))))
		(t (setq plans (find-plans-in-pg plan-graph all-plans)))))
    (when plans 
      (if all-plans 
        (mapcar #'(lambda (plan) (nreverse (simplify-plan plan)))
                plans)
        (nreverse (simplify-plan plans))))))

;;;Initialize-plan graph builds the plan graph structure and puts 
;;;the initial conditions in at level 1.
(defun INITIALIZE-PLAN-GRAPH (problem domain)
  ;; reset counters used for canonical orders
  (setq *prop-number* 0)
  (let* ((static (static-conditions problem))
	 (worlds (if *stat-gp-time-dnf* 
		     (time (dnf (problem-init problem)))
		   (dnf (problem-init problem))))
	 (contexts (length worlds))
	 (plan-graph (make-plan-graph 
		      :worlds worlds
		      :contexts contexts
		      :static-inits static
		      :goal-caches (list (make-hash-table :test #'equal))
		      :problem problem
		      :domain domain)))
    (when (or *trace-gp-top-level*
	      *trace-gp-contexts*
	      *stat-gp-num-contexts*)
      (if (= contexts 1)
	  (format t "~&1 context~%")
	(format t "~&~A contexts~%" contexts)))
    (when (and (or *trace-gp-top-level* 
		   *trace-gp-contexts*)
;	       (> contexts 1)
)
      (format t "~&Contexts:")
      (loop for world in worlds
	  for context upfrom 1 do
	    (format t "~&--- ~a ---~%~{~&~5T~a~%~}" context world)))
    (generate-inits plan-graph)))

;;; construct the proposition nodes for the first level of the plan graph
;;; This is complicated because we have to generate contexts if there are 
;;; any disjunctive or uncertain initial conditions
(defun generate-inits (plan-graph)
  (let ((static-inits (pg-static-inits plan-graph))
        (new-prop-level nil)
        (static-props nil))
    (declare (special static-props new-prop-level))
    (loop for world in (pg-worlds plan-graph)
          for context upfrom 1 do
          (dolist (literal world)
            (if (member literal static-inits :test #'equal)
              (add-static-prop literal context)
              (add-prop literal context))))
    (setf (pg-static-props plan-graph) static-props
          (pg-proposition-levels plan-graph) (list new-prop-level))
          
    plan-graph))

;;; return the set of initial conditions that are never deleted
(defun Static-Conditions (problem)
  (let* ((ops (domain-operators (get-domain (problem-domain problem))))
         (effect-literals (loop for op in ops nconc (effect-literals op)))
         (static nil))
    (dolist (init (collect-literals (problem-init problem)))
      (let ((inverted-init (invert init)))
        (when (loop for effect in effect-literals never
                    (listp (instance? effect inverted-init nil)))
          (push init static))))
    static))

(defun collect-literals (expression)
  (let ((literals nil))
    (declare (special literals))
    (collect-literals1 expression)
    literals))

(defun collect-literals1 (expression)
  (declare (special literals))
  (case (when (consp expression) (first expression))
    (and (dolist (c (rest expression)) (collect-literals1 c)))
    ((or xor oneof if iff when unknown)
     (dolist (c (rest expression)) 
       (collect-literals1 c)
       (collect-literals1 (invert c))))
    (not (let ((i (second expression)))
            (if (and (consp i) (keyword? (first i)))
              (collect-literals1 (invert i))
              (pushnew expression literals :test #'equal))))
    (t (pushnew expression literals :test #'equal))))

(defun effect-literals (op)
  (let ((literals nil))
    (declare (special literals))
    (effect-literals1 (op-effect op))
    literals))

(defun effect-literals1 (expression)
  (declare (special literals))
  (case (when (consp expression) (first expression))
    (and (dolist (c (rest expression)) (effect-literals1 c)))
    ((or xor oneof if iff unknown)
     (dolist (c (rest expression)) 
       (effect-literals1 c)
       (effect-literals1 (invert c))))
    (when (effect-literals1 (third expression)))
    (not (let ((i (second expression)))
            (if (and (consp i) (keyword? (first i)))
              (effect-literals1 (invert i))
              (pushnew expression literals :test #'equal))))
    (t (pushnew expression literals :test #'equal))))


(defun add-static-prop (prop context)
  (declare (special static-props))
  (let ((cluster (find-cluster prop static-props)))
    (unless cluster
      (setf cluster (make-cluster :description prop))
      (push cluster static-props))
    (add-context cluster context t)))

(defun add-prop (prop context)
  (declare (special new-prop-level))
  (let ((cluster (find-cluster prop new-prop-level)))
    (unless cluster
      (setf cluster (make-cluster :description prop))
      (push cluster new-prop-level))
    (add-context cluster context nil)))

(defun add-context (cluster context static)
  (let ((prop (find context (cluster-props cluster) 
                    :key #'prop-context :test #'eql)))
    (unless prop
      (setq prop (new-proposition :context context :cluster cluster :static static))
      (push prop (cluster-props cluster)))
    prop))

  
;;;-------------------EXTENDING THE PLAN GRAPH---------------------
;;;These functions extend the plan graph.

(defun EXTEND-PLAN-GRAPH (plan-graph)

  (let ((static-props (pg-static-props plan-graph))
        (previous-prop-level (first (pg-proposition-levels plan-graph)))
        (new-prop-level nil)
        (new-action-level nil)
        )
    (declare (special static-props previous-prop-level new-prop-level new-action-level))
  
    ;;Add the persistence actions.
    (tracer *trace-gp-top-level* "~%  Adding persistence actions: ")
    (time-if nil (add-null-actions previous-prop-level))
  
    ;;Instantiate all of the possible actions from the last
    ;;set of propositions.
    (tracer *trace-gp-top-level* "~%  Adding actions: ")
    (time-if nil (add-actions (domain-operators (pg-domain plan-graph))
                              (pg-contexts plan-graph)))
    (tracer *trace-gp-actions* "~%~4Tnew propositions: ~%~{~10T~a~%~}" 
            (loop for cluster in new-prop-level nconc
                  (loop for prop in (cluster-props cluster)
                        unless (find 'persist (prop-establishers prop) :key #'act-operator)
                        collect prop)))

    ;;Add the new action level.
    (push new-action-level (pg-action-levels plan-graph))
    
    ;;Add the new proposition level.
    (push new-prop-level (pg-proposition-levels plan-graph))
    
    ;;Add a new goal cache.
    (push (make-hash-table :test #'equal) (pg-goal-caches plan-graph))

    ;;Look for mutual excusion relationships using the three
    ;;rules given in the paper.
    (tracer *trace-gp-top-level* "~%  Finding mutex: ")
    (derive-mutex previous-prop-level new-prop-level)))

  
;;;The proposition lookups would be faster if we replaced the
;;;unordered lists each level with something that has more structure.
(defun find-cluster (prop prop-level)
  (find prop prop-level
        :key #'cluster-description
        :test #'equal))

(defun find-prop (description context prop-level &optional (cluster nil))
  (let ((cluster (or cluster (find-cluster description prop-level))))
    (when cluster
      (find context (cluster-props cluster) :key #'prop-context))))

(defun INTERSECT-P (s1 s2)
  (loop for ele in s1 thereis
        (member ele s2 :test #'eq)))

;;; adds nops to graph
(defun ADD-NULL-ACTIONS (previous-prop-level)
  (declare (special new-action-level new-prop-level))
  (dolist (cluster previous-prop-level)
    (let ((new-cluster (make-cluster 
                             :description (cluster-description cluster))))
      (push new-cluster new-prop-level)
      (dolist (prop (cluster-props cluster))
        (let* ((new-prop (new-proposition :context (prop-context prop) :cluster new-cluster))
               (persistence-action
                (make-action :operator 'persist
                             :preconditions (list prop)
                             :effects (list new-prop)
                             :context (prop-context prop))))
          (push new-prop (cluster-props new-cluster))
          (push persistence-action (prop-consumers prop))
          (push persistence-action new-action-level)
          (push persistence-action (prop-establishers new-prop))
          (tracer *trace-gp-actions* "~&~10T~A~%" new-prop))))))

;;;Add-actions incrementally instantiates each operator schema for all
;;;possible variable bindings.  Add-actions itself just builds (or finds) the
;;;precondition objects in the previous proposition level.  Add-op actually
;;;constructs the action and wires it into the plan graph.

(defun add-actions (operators contexts)
  (declare (special contexts))
  (dolist (operator operators)
    (declare (special operator))
    (process-precs (clauses (op-precondition operator)) nil nil)))

(defun process-precs (preconditions bindings precondition-clusters)
  (if (null preconditions)
    (process-op-effects bindings precondition-clusters)
    (let* ((pre (pop preconditions))
           (keyword (when (consp pre) (first pre))))
      (case keyword
        (not (let ((c (second pre)))
               (if (eql '= (first c))
                 (when (check-neq (second c) (third c) bindings)
                   (process-precs preconditions bindings 
                                  precondition-clusters))
                 (check-precondition pre preconditions
                                     bindings precondition-clusters))))
        (= (when (check-eq (second pre) (third pre) bindings)
               (process-precs preconditions bindings 
                              precondition-clusters)))
        (and (process-precs (append (rest pre) preconditions)
                             bindings precondition-clusters))
        (or (dolist (pre (rest pre))
               (check-precondition pre preconditions bindings 
                                   precondition-clusters)))
        ((forall exist imply) nil)              ; haven't figured these out yet
        (t (check-precondition pre preconditions
                               bindings precondition-clusters))))))

(defun check-precondition (pre preconditions bindings precondition-clusters)
  (declare (special static-props previous-prop-level))
  (dolist (cluster static-props)
    (let ((new-bindings (instance? pre (cluster-description cluster)
                                   bindings)))
      (when (listp new-bindings)
        (process-precs preconditions new-bindings 
                       (cons cluster precondition-clusters))
        )))
  (dolist (cluster previous-prop-level)
    (let ((new-bindings (instance? pre (cluster-description cluster)
                                   bindings)))
      (when (listp new-bindings)
        (process-precs preconditions new-bindings 
                       (cons cluster precondition-clusters))
        ))))

(defun process-op-effects (bindings precondition-clusters)
  (declare (special operator contexts))
  (let ((effect-clauses (clauses (op-effect operator)))
        (aspects nil))
    (declare (special aspects))
    (loop for context from 1 upto contexts do
          (let ((precondition-props nil))
            (when (dolist (cluster precondition-clusters t)
                    (let ((prop (find context (cluster-props cluster) :key #'prop-context)))
                      (when (or (null prop)
                                (intersect-p (prop-mutex prop) precondition-props))
                        (return nil))
                      (push prop precondition-props)))
              (process-effects effect-clauses bindings context precondition-props nil))))
    ;; All of these aspects belong to the same action (operator & bindings)
    (dolist (a aspects)
      ;; set the aspects slot of action-aspect
      (setf (act-aspects a) aspects)
      ;; now see if any other aspects are induced by this one
      (dolist (b aspects)
        (when (and (not (eql a b))
		   (not (eq (act-context a) (act-context b)))
                   (aspect-induces a b))
          (push a (act-inducers b)))))))

;; Does aspect Ai induce Aj at this level?  This will be true iff it is not 
;; possible to confront Aj at this level.  Thus, for each precondition P of Aj either
;; 1) the negation of P must not exist at the level,
;; 2) the negation of P must be mutex with one of the preconditions of Ai.
;; 3) the negation of P must be the negation of an effect of Ai  
;;    Put another way, (persist (not P)) is mutex with Ai
(defun ASPECT-induces (Ai Aj)
  (declare (special previous-prop-level))  
  (let (;;;(Aj-context (act-context Aj))	; Used to be prop-context
        ;; Collect all the props mutex with preconditions of Ai
        (Ai-mutex (remove-duplicates (loop for prop in (act-preconditions Ai) 
                                           append (prop-mutex prop)))))
    ;; The following implements condition 3 above by adding the negations of Ai effects to Ai-mutex
    ;; this additional rule doesn't seem to have much impact, but doesn't hurt either.
    (dolist (eff (act-effects Ai))
      (let ((inv-eff (find-prop (invert (prop-description eff)) 
				;; Should this really be Aj-context?  Why am I looking
				;; in Aj's PW for the negation of Ai's effects?  It might
				;; very well be that the negation of Ai's effects in Ai's
				;; PW are consistent with Aj's effects, even though the
				;; negation of Ai's effects are INconsistent with Aj's
				;; effects when those effects are placed in Aj's context.
				(act-context Ai)
				previous-prop-level)))
        (when inv-eff (pushnew inv-eff Ai-mutex))))
    (loop for prop in (act-preconditions Aj) always
          (let ((inv-prop (find-prop (invert (prop-description prop)) 
				     ;; An aspect's preconditions may not be
				     ;; limited to those propositions in which
				     ;; the aspect is taken.  For example, sensory
				     ;; actions take preconditions from all 
				     ;; the worlds.
				     (prop-context prop)
				     previous-prop-level)))
            (or (null inv-prop)
                (member inv-prop Ai-mutex))))))
           
(defun process-effects (effects bindings context precondition-props atomic-effects)
  (if (null effects)
      ;; We actually want to include the null aspect for actions -- that is, 
      ;; aspect of an action for which all the action's preconditions are false,
      ;; and thus the aspect that has no effects.  Why do we want such a beast?
      ;; Because, when solving the CSP, we need to be able to tell if an action A
      ;; in PW u will be mutex with any action B in PW w.  If B doesn't manifest
      ;; itself in w at all (i.e., only B's null aspect fires), then we can't
      ;; tell that A might be mutex with B.  Even worse, executing C before A 
      ;; and B might allow aspect b of B to fire in w, and b might be mutex with
      ;; the chosen a:u of Au.

      (add-action bindings context precondition-props atomic-effects)

    (let* ((effect (pop effects))
           (keyword (when (consp effect) (first effect))))
      (case keyword
        (and
         (process-effects (append (rest effect) effects) 
                          bindings context precondition-props atomic-effects))
        (when 
          (process-conditional effect effects bindings context
                               precondition-props atomic-effects))
	(observes
	 (process-observation effect effects bindings context
			      precondition-props atomic-effects))
        ((forall or exists) nil)
        (t (process-effects effects bindings context precondition-props 
                            (cons effect atomic-effects)))))))

(defun process-conditional (conditional effects bindings context 
                                        precondition-props atomic-effects)
  (let ((prec (second conditional)))
    (process-cond-precs (clauses prec)
                               (append (clauses (third conditional)) effects)
                               bindings context precondition-props atomic-effects)
    (process-cond-precs (clauses (invert prec))
                               effects
                               bindings context precondition-props atomic-effects)))

(defun process-cond-precs (preconditions effects bindings context 
                                                precondition-props atomic-effects)
  (declare (special previous-prop-level))
  (if (null preconditions)
    (process-effects effects bindings context precondition-props atomic-effects)
    (let* ((pre (pop preconditions))
           (keyword (when (consp pre) (first pre))))
      (case keyword 
        (not (let ((c (second pre)))
               (if (eql '= (first c))
                 (when (check-neq (second c) (third c) bindings)
                   (process-cond-precs preconditions effects bindings context 
                                       precondition-props atomic-effects))
                 (check-cond-precondition pre preconditions effects bindings context
                                          precondition-props atomic-effects))))
        (= (when (check-eq (second pre) (third pre) bindings)
               (process-cond-precs preconditions effects bindings context 
                                   precondition-props atomic-effects)))
        (and (process-cond-precs (append (rest pre) preconditions)
                                  effects bindings context 
                                  precondition-props atomic-effects))
        (or (dolist (pre (rest pre))
               (check-cond-precondition pre preconditions effects bindings context
                                        precondition-props atomic-effects)))
        ((forall exists) nil)       ; still need to do this
        (t (check-cond-precondition pre preconditions effects bindings context
                                    precondition-props atomic-effects))))))

(defun check-cond-precondition (pre preconditions effects bindings context
                                    precondition-props atomic-effects)
  (declare (special static-props previous-prop-level))
  (let* ((plugged (plug pre bindings))
	 (static-cluster (find-cluster plugged static-props))
         (prop-cluster (find-cluster plugged previous-prop-level)))
    (when (or static-cluster prop-cluster)
      (let ((prop (or (find-prop nil context nil static-cluster) ; Gotta check 'em both.
		      (find-prop nil context nil prop-cluster))))
        (when (and prop
                   (not (intersect-p (prop-mutex prop) precondition-props)))
          (process-cond-precs preconditions effects bindings context
                              (cons prop precondition-props)
                              atomic-effects))))))


;;;
;;; The aspect that we're looking at is an observational aspect.  
(defun process-observation (observation	; The observation expression. 
			    effects	; The effects remaining from this aspect.
					; We won't modify them here, but pass
					; them along to a recursive call to
					; process-effects.
			    bindings	; The variable bindings used for this
					; aspect.
			    context	; The world that we're presently in.
			    precondition-props ; The propositions from the previous
					; level that we're using to establish
					; our preconditions.
			    atomic-effects) ; The effects that will be added 
					; at the next planning graph level.

  (declare (special contexts static-props previous-prop-level))

  (let* ((F-unplugged (second observation)) ; F is the formula being observed.
	 (F (plug F-unplugged bindings)) ; Let's take care of those variables, 
					; shall we?
	 (P (collect-atoms F))		; The atoms of the formula
	 (Cu nil))			; The C_u's


    (loop for u from 1 upto contexts do
	  ;; V is a list representing all the V_j's.
	  ;; Each element in V is a list of possible values for P.
	  (let* ((props (append previous-prop-level static-props))
		 (V (mapcar #'(lambda (p)
				
				;; values is the V_j for our particular
				;; p_j (p_j is called p here)
				(let ((values nil))
				  (let* ((true-prop (find-prop p u props))
					 (nil-prop (find-prop (invert p) u props)))
				    
				    (when true-prop
				      (push (cons t true-prop) values))
				    
				    (when nil-prop
				      (push (cons nil nil-prop) values)))
				 
				  values))
			    P)))

	    (let ((crossV (remove-if #'(lambda (cv)
					 (dolist (v1 cv)
					   (dolist (v2 cv)
					     (mutexp (cdr v1) (cdr v2)))))
				     (cross V))))
	      (push crossV Cu))
	    ))
		 
    
;;;   Suppose F mentions k atomic propositions {P_1 ... P_k}
;;;   For each possible world u
;;;    Let V_u,j denote the set of possible values of P_j in world u at level i-1, 
;;;        % I.e. V_u,j is either {T} {F} or {T,F}
;;;    Let C_u be a subset of V_u,1 x ... x V_u,k by filtering all mutex combos
;;;        % Note: if each prop is certain in u at level i-1 then |C_u|=1
;;;        % But in the worst case then the cardinality is 2^k for each PW
;;;    Let C be a subset of C_i x ... x C_w, by filtering out cross-world mutexes
;;;        % Note: if each prop is certain in each world, then |C| = 1
;;;        % Worst case: |C| = 2^k^w
;;;    For each elt, c, of C
;;;        % c is a w-tuple of k-tuples of boolean values for props in F
;;;        % each k-tuple, thus leads to a value for F
;;;        SA = create a new instance of S at level i
;;;            link the preconds of SA to the corresponding elements of c at i-1
;;;            add any causal effects of SA to level i+1
;;;            % optimization: don't create SA unless it discriminates below
;;;        Map F on c getting a boolean w-vector, D
;;;        % D represents the possibly sensed values in all w worlds
;;;        % D specifies a partition: those where the sensor returned T (and F)
;;;        for each x = 1 to w
;;;            for each y = x+1 to w
;;;                if D(x) <> D(y) 
;;;                then add K!x:y and K!y:x to level i+1, and link to SA 

    ;; Note: computing the cross-world mutexes is really tedious.  It'd be neat
    ;; if we could simplify this operation somehow.  Got any ideas???
    (let ((crossC (remove-if #'(lambda (c)
				 (dolist (u c) ; For each world pair in c...
				   (dolist (w c)
				     (dolist (v_u u) ; For each variable value 
				       (dolist (v_w w) ; in these worlds...
					 (mutexp (cdr v_u) (cdr v_w)))))))
			     (cross Cu))))
      
      (dolist (c crossC)
      
	;; What are the preconditions?
	(let* ((precs nil)
	       (f-values (mapcar #'(lambda (u)
				     ;; Choose any proposition's context
				     (cons (eval-f F p (reverse u))
					   (prop-context (cdar u))))
				 c))
	       (know-not-effects (know-not-effects f-values context)))
		  
	  ;; We must march through the list (as opposed to using
	  ;; a mapcar) because we want to flatten the resulting
	  ;; structure as we go.
	  (dolist (u c)
	    (setq precs (append precs (mapcar #'cdr u))))

	  ;; Continue processing the effects of this aspect
	  (process-effects effects bindings context 
			   (append precs precondition-props)
			   (append know-not-effects atomic-effects)))))
	
      

    ;; What's left?  Four things:
    ;; (1) Figure out how to fabricate the K!u:v proposition
    ;; ANSWER:   (cons '(know-not u) atomic-effects)
    ;; (2) Map the truth bit vector onto F to figure out the
    ;;     truth value of F in each world.
    ;; ANSWER:   (eval-f F p u)
    ;; (3) Create K!u:v propositions according to this partition.
    ;; (4) Include the proper preconditions to this action.
    ))


(defun cross (sets)
  ;; sets is a list of sets of values.  The output
  ;; of cross is the cross product of these sets --
  ;; a list of lists of members of the input sets.
  
  (cond ((null sets) '( () ))
	(t (let ((s (car sets))
		 (product '()))
	     (dolist (v s)
	       (setq product (append product
				     (mapcar #'(lambda (p)
						 (cons v p))
					     (cross (cdr sets))))))
	     product))))


(defun know-not-effects (f-values context)
  ;; Return a list of effects of the form (know-not u).

  (let ((f-value-w (car (find-if #'(lambda (f)
				     (eq (cdr f) context))
				 f-values)))
	(effects nil))
    (do* ((f-values f-values (cdr f-values))
	  (u (cdar f-values) (cdar f-values))
	  (f-value-u (caar f-values) (caar f-values)))
	((null f-values) effects)
      (when (not (eq f-value-u f-value-w))
	(push `(know-not ,u) effects)))))



(defun add-action (bindings context precondition-props atomic-effects)
  (declare (special operator aspects new-action-level))

  (let* ((effect-props (loop for effect in atomic-effects collect 
                            (add-prop (plug effect bindings) context)))
         (action (make-action :operator operator
                              :bindings bindings
                              :preconditions precondition-props
                              :effects effect-props
                              :context context)))
    (push action new-action-level)
    (dolist (precondition precondition-props)
      (push action (prop-consumers precondition)))
    (dolist (effect effect-props)
      (push action (prop-establishers effect)))
    (push action aspects)
    (tracer *trace-gp-actions* "~&~10T~{~a ~} --> ~A --> ~{~a ~}" precondition-props action effect-props)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   ------------------- Mutual exclusion ----------------------------------
;;;

;;;The mutex operations have the largest effect on performance in this
;;;implementation.  Start here if you want to optimize performance.

(declaim (inline mutexp))
(defun MUTEXP (node1 node2)
  (member node1 (node-mutex node2) :test #'eq))

(defun ADD-MUTEX (node1 node2)
  (unless (mutexp node1 node2)
    (push node1 (node-mutex node2))
    (push node2 (node-mutex node1))))


;;
;; When we mark act1 and act2 as mutex, we need to also consider
;; all the other sibling aspects of act1 and act2 that are induced
;; by act1 and act2.  The basic idea is, if act1 induces I, then I 
;; and act2 are mutex.  HOWEVER, with sensory actions, it might be
;; possible to selectively not execute I in its PW, while still 
;; executing act1.  Suppose act1 comes from PW u, act2 from PW w, 
;; and I induced by act1 comes from PW v.  act2 and I are mutex
;; iff the proposition (KNOW-NOT v){w} doesn't exist at the previous
;; proposition level.  If the prop does exist, then there is the 
;; possiblity that I can be conditioned on not being in PW w, and
;; act2 can be conditioned on not being in PW v.
;;
(defun add-mutex-actions (act1 act2 previous-prop-level)
  (when (and (add-mutex act1 act2)
	     *use-induced-mutexes*)

    ;; Find the clusters for the two aspects' K!u:v propositions.  Do the 
    ;; (potentially expensive) find operation only when necessary.
    (let ((act1-cluster (and (act-inducers act2)
			     (find-cluster (list 'know-not (act-context act1))
					   previous-prop-level)))
	  (act2-cluster (and (act-inducers act1)
			     (find-cluster (list 'know-not (act-context act2))
					   previous-prop-level))))
      ;; Add all the mutexes that are caused by induction.
	(dolist (inducer (act-inducers act2))
	  (unless (find-prop nil (act-context inducer) nil act1-cluster)
	    (add-mutex act1 inducer)
;;;	    (format t "*")
	    (tracer *trace-gp-mutex* " ~& ~12TAdded mutex aspect: ~a ~a" act1 inducer)))
	(dolist (inducer (act-inducers act1))
	  (unless (find-prop nil (act-context inducer) nil act2-cluster)
	    (add-mutex inducer act2)
;;;	    (format t "*")
	    (tracer *trace-gp-mutex* " ~& ~12TAdded mutex aspect: ~a ~a" inducer act2))))))



;;;Note that proposition lookups and add-mutex are the inner loops of this
;;;routine.

(defun Derive-mutex (previous-prop-level new-prop-level)
  (tracer *trace-gp-mutex* "~%~4Tfinding interfering actions")
  (time-if nil (find-interfering-actions previous-prop-level new-prop-level))
  (tracer *trace-gp-mutex* "~%~4Tfinding competing needs")
  (time-if nil (find-competing-needs previous-prop-level))
  (tracer *trace-gp-mutex* "~%~4Tfinding exclusive propositions")
  (time-if nil (find-exclusive-propositions new-prop-level))
  )

(defun FIND-INTERFERING-actions (previous-prop-level new-prop-level)
  ;; Mark actions as interfering if the actions have opposite effects.
  (dolist (cluster new-prop-level)
    (let* ((description (cluster-description cluster))
           (inverted (invert description)))
      ;; Find opposite effects (only have to look at the not effects)
      (when (inverted? description)
        (let ((deleted-cluster (find-cluster inverted new-prop-level)))
          (when deleted-cluster
            (dolist (effect (cluster-props cluster))
              (let ((deleted-effect (find (prop-context effect)
                                          (cluster-props deleted-cluster) :key #'prop-context)))
                (when deleted-effect
                  (tracer *trace-gp-mutex* " ~& ~8TMutex props due to opposite effects: ~a ~a"
                          effect deleted-effect)
                  (add-mutex effect deleted-effect)
                  (dolist (establisher1 (prop-establishers effect))
                    (dolist (establisher2 (prop-establishers deleted-effect))
                      (tracer *trace-gp-mutex* " ~& ~12TMutex establishers: ~a ~a"
                          establisher1 establisher2)
                      (add-mutex-actions establisher1 establisher2 previous-prop-level)))))))))
      ;; Find clobbered preconditions
      (let ((deleted-cluster (find-cluster inverted previous-prop-level)))
        (when deleted-cluster
          (dolist (effect (cluster-props cluster))
            (let ((deleted-pre (find (prop-context effect)
                                        (cluster-props deleted-cluster) :key #'prop-context)))
              (when deleted-pre
                (dolist (establisher (prop-establishers effect))
                  (dolist (consumer (prop-consumers deleted-pre))
                    (unless (eq consumer establisher)
                      (tracer *trace-gp-mutex* " ~& ~8TMutex actions due to deleted precs: ~a ~a"
                              establisher consumer)
                      (add-mutex-actions establisher consumer previous-prop-level))))))))))))

;;; Mark actions with incompatable preconditions as mutually exclusive
;;; This function does about twice as much work as it needs to.
(defun FIND-COMPETING-NEEDS (previous-prop-level)
  (dolist (cluster previous-prop-level)
    (dolist (prop (cluster-props cluster))
      (dolist (mutex-prop (prop-mutex prop))
        (dolist (action1 (prop-consumers prop))
          (dolist (action2 (prop-consumers mutex-prop))
            (tracer *trace-gp-mutex* " ~& ~8TMutex actions due to mutex precs: ~a ~a"
                              action1 action2)
            (add-mutex-actions action1 action2 previous-prop-level)))))))

;;; Propositions are exclusive if 1) they are logically contradictory (handled above) 
;;; or 2) if all ways of obtaining them are mutually exclusive
(defun FIND-EXCLUSIVE-PROPOSITIONS (new-prop-level)
  (loop for (cluster1 . rest-clusters) on new-prop-level do
	;; Check all propositions, by cluster
        (loop for (prop1 . rest-props) on (cluster-props cluster1) do
	      ;; Check propositions from the same cluster
              (dolist (prop2 rest-props)
                (check-exclusive-propositions-csp prop1 prop2))
	      ;; Check props from different clusters.
              (dolist (cluster2 rest-clusters)
                (dolist (prop2 (cluster-props cluster2))
                  (check-exclusive-propositions-csp prop1 prop2))))))

;; Corey thinks that something is fishy here.  Here's the situation:  When
;; we have the ability to use sensors, we can potentially avoid an induced
;; aspect by electing to not execute it in a particular world.  When do we
;; decide that two propositions, all of whose establishers are mutex
;; via. induced mutexes, are possible to execute together?  Do we not put
;; in the mutex in the first place?  Do we put in the mutex, but then
;; ignore it later on?  The former option seems easier, but I'm worried
;; that, by omitting the mutex, we might allow mutex actions to slip by.
;; Of course, if we take the stand that mutexes are only optimizations,
;; then it should be safe to omit these mutexes.

(defun check-exclusive-propositions (prop1 prop2)
  (when (and (not (mutexp prop1 prop2))
             (loop for act1 in (prop-establishers prop1) always
                   (loop for act2 in (prop-establishers prop2) always
                         (mutexp act1 act2))))
    (tracer *trace-gp-mutex* " ~& ~8TMutex props due to mutex establishers: ~a ~a" prop1 prop2)
    (add-mutex prop1 prop2)))

(defun check-exclusive-propositions-csp (prop1 prop2)
  (when (and (not (mutexp prop1 prop2))
	     ;; All ways of establishing prop1...
             (loop for act1 in (prop-establishers prop1) always
		   ;; All ways of establishing prop2...
                   (loop for act2 in (prop-establishers prop2) always
                         (and (mutexp act1 act2) ; Actions are mutex...
			      (eq (act-context act1) ; ...and live in the same
				  (act-context act2)))))) ; possible world
    (tracer *trace-gp-mutex* " ~& ~8TMutex props due to mutex establishers: ~a ~a" prop1 prop2)
    (add-mutex prop1 prop2)))


;;;-----------------------The search engine-----------------------
;;;
;;; do a backward chaining search once all goal nodes occur in current level
;;; to see if they really are consistent

;;;This function finds any satisficing plan in the current plan graph.
;;;The search function does not incorporate all of the suggestions in
;;;the original paper.
(defun FIND-PLANS-IN-PG (plan-graph all-plans)
  (declare (special contexts))
  (setq contexts (pg-contexts plan-graph))

  (time-if nil
    (let* ((prop-levels (pg-proposition-levels plan-graph))
           (problem (pg-problem plan-graph))
           (goals (clauses (problem-goal problem)))
           (goal-props nil))
      ;; Make sure that the goals exist in all the contexts and
      ;; are non-mutex.
      (when (loop for goal in goals always
		  ;; We must consider both the static proposition cluster and the
		  ;; mutable proposition cluster.  The static proposition clusters
		  ;; list only the propositions that won't be deleted -- there may
		  ;; very well be worlds in which the props are _added_.
                  (let ((static-cluster (find-cluster goal (pg-static-props plan-graph)))
			(prop-cluster (find-cluster goal (first prop-levels))))
                    (when (or static-cluster prop-cluster)
                      (loop for context from 1 upto contexts always
                            (let ((prop (or (and prop-cluster
						 (find context (cluster-props prop-cluster)
						       :key #'prop-context))
					    (and static-cluster
						 (find context (cluster-props static-cluster)
						       :key #'prop-context)))))
                              (when (and prop
                                         (loop for goal-prop in goal-props never
                                               (mutexp prop goal-prop)))
                                (push prop goal-props)))))))
	;; Okay, the goals are non-mutex.  Let's try to find a plan.
        (if all-plans
          (find-all-plans-in-pg1 goal-props (pg-action-levels plan-graph)
                                 prop-levels 
				 (pg-static-props plan-graph)
				 (pg-goal-caches plan-graph) 0)
          (find-plan-in-pg1 goal-props (pg-action-levels plan-graph)
                            prop-levels 
			    (pg-static-props plan-graph)
			    (pg-goal-caches plan-graph) 0))))))

;;; Note: could remove action-levels argument from find- and assign- functions
;;; below. Only need to change the termination check from (null action-levels)
;;; to (null (rest prop-levels))

(defun FIND-PLAN-IN-PG1 (goal-props action-levels prop-levels static-props goal-caches level)
  (declare (special goal-props action-levels prop-levels static-props goal-caches level))
  (tracer *trace-gp-bc* "~&~4T~Aseeking:~%~{~10T~a~%~}" 
          (make-string (* 2 level) :initial-element #\ )
          goal-props)
  (cond ((null goal-props) (list t))

	((null action-levels)
	 ;; Okay, we've regressed all the way back to the initial 
	 ;; state.  In previous versions of GP, we simply asserted
	 ;; that everything was hunky dory, and returned '(T) (note
	 ;; also that we're returning (list T) now, as '(T) caused
	 ;; erroneous stuff to sneak into the plan).
	 
	 ;; HOWEVER, it might be the case that we posted a subgoal
	 ;; at the previous level, WITHOUT checking that the goal
	 ;; prop even existed at this level.  Why would we have done
	 ;; this?  We might have been using the :NO-EXEC option for
	 ;; some action, and the :NO-EXEC{u} 'aspect' requires a
	 ;; (know-not u){w} predicate for each PW w <> u.  Hence, we
	 ;; might have blindly added the (know-not u){w} subgoals,
	 ;; even though their props didn't exist at this level.
	 
	 ;; What's the solution?  Simply check over all the goals in
	 ;; goal-props.  If any of them are not in the car of the
	 ;; prop-levels, then we know that we really don't have a
	 ;; legit plan, and should backtrack.

	 (let ((success? (list t)))
	   (dolist (g goal-props)
	     (let* ((prop-c (find-cluster (prop-description g) (car prop-levels)))
		    (static-c (find-cluster (prop-description g) static-props))
		    (prop-p (and prop-c (find-if #'(lambda (p) 
						     (eq (prop-context p)
							 (prop-context g)))
						 (cluster-props prop-c))))
		    (static-p (and static-c (find-if #'(lambda (p) 
						     (eq (prop-context p)
							 (prop-context g)))
						     (cluster-props static-c)))))
	       (unless (or prop-p static-p)
		 (setq success? nil))))
	   success?))

        ((memoized-failure-p goal-props (first goal-caches))
         (tracer *trace-gp-bc* " -- failure: memoized goals")
         nil)    ; at the very first level of the graph
        ;; The following check is not needed because if subgoals are mutex, 
        ;; actions that generated them are also mutex and this is caught in
        ;; assign-goals before we get here.
        ;; ((mutex-prop-set? goal-props)
        ;;  (memoize-as-failure goal-props (first goal-caches))
        ;;  (tracer *trace-gp-bc* " -- failure: mutex goals")
        ;;  nil)

	;; Okay, there are goals left, and we haven't already figured that
	;; we're doomed.  Try to find a plan that establishes these goals.
        (t (let ((plan (assign-goals-csp goal-props nil nil 
					 (make-hash-table :test #'equal))))
             (unless plan
               ;;(tracer *trace-gp-bc* " -- failure: no consistent actions"))
               ;; Could be smarter here and add mutex for binary goals,
               ;; but would require propagating the mutex forward in the graph
               (memoize-as-failure goal-props (first goal-caches)))
             plan))))


(defun FIND-ALL-PLANS-IN-PG1 (goal-props action-levels prop-levels 
			      static-props goal-caches level)
  (declare (special goal-props action-levels prop-levels 
		    static-props goal-caches level))
  (tracer *trace-gp-bc* "~&~4T~Aseeking:  ~{~a ~}" 
          (make-string (* 2 level) :initial-element #\ )
          goal-props)
  (cond ((null goal-props) (list (list t)))
        ((null action-levels)  (list (list t)))
        ((memoized-failure-p goal-props (first goal-caches))
         (tracer *trace-gp-bc* " -- failure: memoized goals")
         nil)    ; at the very first level of the graph
        ;; The following check is not needed because if subgoals are mutex, 
        ;; actions that generated them are also mutex and this is caught in
        ;; assign-goals before we get here.
        ;; ((mutex-prop-set? goal-props)
        ;;  (memoize-as-failure goal-props (first goal-caches))
        ;;  (tracer *trace-gp-bc* " -- failure: mutex goals")
        ;;  nil)
        (t (let ((plans (assign-all-goals-csp goal-props nil nil
					      (make-hash-table :test #'equal))))
             (unless plans
               (memoize-as-failure goal-props (first goal-caches)))
             plans))))

(defun MEMOIZED-FAILURE-P (goals cache)
  (gethash (canonical-goal-order goals) cache))

(defun MEMOIZE-AS-FAILURE (goals cache)
  (setf (gethash (canonical-goal-order goals) cache) t))

(defun CANONICAL-GOAL-ORDER (goals)
  (sort (mapcar #'prop-number goals) #'<))

;; GIVEN:    a list of remaining goals (remaining-goal-props),
;;           a list of actions to take at this level (actions),
;;           and a list of subgoals for the next level,
;; COMPUTE:  Either a new action and subgoal set to satisfy the
;;           next goal in line, OR a plan to satisfy all the 
;;           subgoals.
(defun ASSIGN-GOALS (remaining-goal-props next-goals actions)
  (declare (special goal-props action-levels prop-levels goal-caches))
  (if remaining-goal-props
    (destructuring-bind (goal . rest) remaining-goal-props
      (if (prop-static goal)
        (assign-goals rest next-goals actions)
        (loop for establisher in (prop-establishers goal) thereis
              (unless (member establisher actions :test #'mutexp)
                (assign-goals rest
                              (union (act-preconditions establisher) next-goals)
                              (adjoin establisher actions))))))
    (let ((plan (check-unexpected-consequences next-goals actions goal-props)))
      (when plan
        (cons actions plan)))))


;;;3.  For each g:w in Si+1, 
;;;
;;;    3.1   From level i in the plan graph, choose an a:w that produces
;;;          g:w. If there exists a variable V in Vi such that a:w is mutex
;;;          with all of V's values, then backtrack. 
;;;
;;;    3.2   Let A be the action from which aspect a:w was
;;;          derived.  For each PW u create the new variable Au.
;;;          If Au is not already in Vi, then add Au to Vi.
;;;
;;;          The domain of values for each Au is the set of
;;;          aspects of A in PW u, possibly unioned with the special
;;;          value NO-EXEC. 
;;;
;;;    3.3   Assign the value of Aw to be a, where a and w were
;;;          chosen in step 3.1

(defstruct (CSP-var (:conc-name csp-) (:print-function print-csp-var))
  context				; The PW in which this CSP var exists.
  description				; The description of the *instantiate*
					; operator from which this aspect derives.
  (aspects nil)				; The domain possible values for this 
					; variable.  The special NO-EXEC symbol 
					; is :NO-EXEC.
  )

(defun print-csp-var (csp-var &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "~A_~A = { ~{~A ~}}" 
	  (csp-description csp-var) 
	  (csp-context csp-var) (csp-aspects csp-var)))
	  

(defun ASSIGN-GOALS-CSP (remaining-goal-props next-goals actions know-not-prop-cache)
  (declare (special goal-props action-levels prop-levels goal-caches 
		    contexts know-not-prop-cache))
  (cond (remaining-goal-props
	 ;; Grab the next goal to be satisfied
	 (destructuring-bind (goal . rest) remaining-goal-props
	   (if (prop-static goal)	; If the goal is never deleted...
	       (assign-goals-csp rest next-goals actions know-not-prop-cache)
					; ...proceed with the
					; rest of the goals, AND, don't 
					; bother to propogate the goal.
	     ;; The goal isn't static.  Try all the different ways
	     ;; of establishing this goal.
	     (loop for establisher in (prop-establishers goal) thereis
		   ;; The selected goal set needs to avoid mutex goals between
		   ;; actions in the same context.  But, if two actions are mutex
		   ;; and in different worlds, that's okay -- the mutex will either
		   ;; be cleared up by a NO-EXEC, or we'll fail later (right?!?)
		   (unless (member establisher actions :test #'mutexp)
		     ;; we can use mutexp because the inclusion or exclusion of
		     ;; induced mutexes is being handled elsewhere.

		     (assign-goals-csp rest
				       (union (act-preconditions establisher) next-goals)
				       (adjoin establisher actions)
				       know-not-prop-cache))))))

	(t
	 ;; All the goals at this level are now satisfied by the aspects in 
	 ;; aspects.  Build the CSP and choose a solution to it.  The solution
	 ;; will bind the actions to aspects in the other possible worlds.
	 (let ((csp-vars nil))		; The list of CSP variables for which we need
					; to solve.
	   (dolist (a actions)		; Each chosen aspect has an associated
					; CSP variable.
	     (let ((Au (find-if #'(lambda (cv)
				    (and (equal (csp-description cv) (act-description a))
					 (eq (csp-context cv) (act-context a))))
				csp-vars)))
	       (cond ((and (null Au)
			   (not (eq 'persist (act-operator a))))
		      ;; The CSP variable for a hasn't been added to the list of
		      ;; csp-vars yet.  Create the variable.  For each PW u, the
		      ;; CSP variable can be any of the aspects available in u
		      ;; for that operator.  Of course, for the context of a, the
		      ;; only available aspect is a.
		      (do ((u 1 (+ 1 u)))
			  ((> u contexts))

			(cond ((eq u (act-context a))
			       ;; We're creating the CSP variable for the
			       ;; action/world pair that we've selected.
			       ;; There's only one legal possibility --
			       ;; the aspect that we have already selected.

			       (push (make-csp-var :context u
						   :description (act-description a)
						   :aspects (list a))
				     csp-vars))
			  
			  ;; Let action A have one aspect, a.  Suppose that a can
			  ;; be executed in PW u, but a's preconditions aren't true
			  ;; in PW v (assume that A has no preconditions, but 
			  ;; there is a conditional effect).
			  ;; Now, suppose that a{u} has been added to the plan.
			  ;; What should we do about A in world v?  Should Av's
			  ;; domain be { NO-EXEC }?  Should the domain be empty?
			  ;; Should we have an Av at all?  I'll vote for the latter
			  ;; case.  Case #1 doesn't work, because we don't need
			  ;; to _prevent_ A from executing in v; A simply doesn't
			  ;; _do_ anything in world v.  Case #2 blows holes in 
			  ;; any CSP solver, as empty domains for CSP variables
			  ;; is usually a Bad Thing.  Thus, option #3 is the best
			  ;; bet until I hear otherwise.


			      (t
			       (let ((aspects-in-u (remove-if-not 
						    #'(lambda (asp)
							(eq (act-context asp) u))
						    (act-aspects a)))
				     (know-not-u? 
				      (find-cluster (list 'know-not u) (cadr prop-levels))))
									    
				 (when aspects-in-u
				   (push (make-csp-var
					  :context u
					  :description (act-description a)

					  :aspects (append aspects-in-u 
							   (when know-not-u?
							     (list :NO-EXEC))))
					 csp-vars)))))))
			
		     ((eq 'persist (act-operator a))
		      ;; The aspect is a no-op.  Create a variable for that
		      ;; aspect, with only itself as the domain of possible 
		      ;; values.
		      (push (make-csp-var :context (act-context a)
					  :description (act-description a)
					  :aspects (list a))
			    csp-vars))

		     (t
		      ;; The CSP variable has already been added to the CSP.
		      ;; Update the available aspect choices for the variable.
		      (setf (csp-aspects Au) (list a))))))
		    
	   (if *use-forward-checking*
	       (solve-csp-fc-dvo csp-vars nil next-goals know-not-prop-cache)
	     (solve-csp csp-vars nil next-goals))
	     
))))
	   


;;
;; Okay, we now have the CSP in the form of a list of CSP variables.
;; Find solutions to the CSP, and try to plan using the aspects
;; chosen for the variables.  If the planning should fail, try another
;; solution to the CSP.
;;
(defun SOLVE-CSP (csp-vars selected-aspects next-goals)
  (declare (special action-levels prop-levels static-props goal-caches level contexts))
  (cond ((null csp-vars)
	 ;; No more variables left in the CSP.  Plan for 
	 ;; establishing the next-goals that will allow
	 ;; the selected-aspects to happen.

	 ;; Recurse on the selected-aspects

	 ;; *** NOTE ***
	 ;; We have to wait until now before we can really compute the
	 ;; preconditions of the NO-EXECs.  Until now, we didn't know for
	 ;; sure what worlds an executable aspect might have been taken.  For
	 ;; we need the (KNOW-NOT u){w} preconditions only for the worlds w
	 ;; in which we execute an aspect of A (where Au is a NO-EXEC).
	 (let* ((no-exec-subgoals 
		 ;; Note that we used cadr prop-levels here.  We use cadr
		 ;; because we're interested in examining the _previous_
		 ;; proposition level for the NO-EXEC's preconditions.
		 (noexec-precs selected-aspects (cadr prop-levels)))
		(plan (find-plan-in-pg1 (union next-goals no-exec-subgoals)
					(rest action-levels)
					(rest prop-levels)
					static-props
					(rest goal-caches)
					(1+ level))))
	   (when plan
	     (cons selected-aspects plan))))


	;; There are still CSP variables left.  Assign a value to the 
	;; next variable.  Check for consistency (ie., check for mutexes 
	;; with other selected aspects).  If consistent, recurse on the
	;; remaining CSP variables.
	(t (let* ((csv (car csp-vars))
		  (csv-domain (csp-aspects csv)))

	     ;; We must potentially try all the values in CSV's domain
	     (do* ((domain csv-domain (cdr domain))
		   (v (car domain) (car domain))
		   (plan nil))
		 ;; If we find a plan, or if we run out of values in CSV's
		 ;; domain, return.
		 ((or plan (null domain)) plan)

	       (tracer *trace-gp-csp* "~&  Using ~A to satisfy ~A~%" v csv)

	       ;; Is the value for chosen for CSV the NO-EXEC option?
	       (if (eq v :NO-EXEC)
		   (unless (noexec-conflicts (csp-description csv) 
					     (csp-context csv)
					     selected-aspects csp-vars 
					     ;; Consider the _previous_ proposition
					     ;; level.
					     (cadr prop-levels) contexts)
		     ;; Yep, using the NO-EXEC thing is okay.  HOWEVER, 
		     ;; choosing NO-EXEC for csv might force the choice 
		     ;; of NO-EXEC for other variables.  Why?  Because, 
		     ;; if we can't discriminate between worlds u and w 
		     ;; and we have chosen NO-EXEC in u, then we must
		     ;; choose NO-EXEC in w.
		     (setq plan (solve-csp (noexec-new-csvs
					    (csp-description csv)
					    (csp-context csv)
					    (cdr csp-vars)
					    ;; Previous prop level
					    (cadr prop-levels))
					   (append (noexec-new-aspects
						    (csp-description csv)
						    (csp-context csv)
						    (cdr csp-vars)
						    ;; Previous prop level
						    (cadr prop-levels))
						   selected-aspects)
					   ;; See note above for why we can't
					   ;; staple the NO-EXEC's precs onto
					   ;; the next-goals list right now.
					   next-goals)))

		 ;; No, the value V for CSV is not the NO-EXEC.  Check to see
		 ;; if V is mutex with any of the currently selected aspects.
		 ;; Figure that, if a NO-EXEC aspect has been chosen, then we
		 ;; have previously checked that that NO-EXEC is non-mutex with
		 ;; the option V.
		 (unless (member v selected-aspects 
				 :test #'(lambda (v sel-asp)
					   (and (not (consp sel-asp))
						(mutexp v sel-asp)
						;; We can check all mutexes, even those
						;; between PWs, because the mutex rule for
						;; induced aspects checks for the possiblity
						;; that NO-EXEC was chosen.
						)))
		   (setq plan (solve-csp (cdr csp-vars) 
					 (cons v selected-aspects)
					 (union (act-preconditions v) next-goals))))))))))



;;
;; How about a Forward Checking + Dynamic Variable Ordering CSP solver?
;;
(defun SOLVE-CSP-FC-DVO (csp-vars	; The variables to solve
			 selected-aspects ; The aspects that we've chosen so far
			 next-goals	; The preconditions of those aspects
			 know-not-prop-cache) ; What K!u:v props do we have?
  
  (declare (special action-levels prop-levels static-props goal-caches level contexts))
  (cond ((null csp-vars)
	 ;; If there aren't any more csp-vars, then call find-plan-in-pg1
	 ;; recursively.

	 ;; Note: We have to wait until now before we can really compute the
	 ;; preconditions of the NO-EXECs.  Until now, we didn't know for
	 ;; sure what worlds an executable aspect might have been taken.  For
	 ;; we need the (KNOW-NOT u){w} preconditions only for the worlds w
	 ;; in which we execute an aspect of A (where Au is a NO-EXEC).
	 (let* ((no-exec-subgoals 
		 ;; Note that we used cadr prop-levels here.  We use cadr
		 ;; because we're interested in examining the _previous_
		 ;; proposition level for the NO-EXEC's preconditions.
		 (noexec-precs selected-aspects (cadr prop-levels)))
		(plan (find-plan-in-pg1 (union next-goals no-exec-subgoals)
					(rest action-levels)
					(rest prop-levels)
					static-props
					(rest goal-caches)
					(1+ level))))
	   (when plan
	     (cons selected-aspects plan))))
	
	;; Else, choose the next csp-var to solve.  Question: should we sort
	;; the variables when we call solve-csp, or should we sort them here?
	;; For now, we'll just take the car of the list.
	(t (let* (;;;(csv (car csp-vars))	; Do DVO here.
		  (csv (best-csp-var csp-vars))
		  (csv-domain (csp-aspects csv))
		  (other-csp-vars (remove-if #'(lambda (v) (eq v csv)) 
					     csp-vars))
		  (csv-context (csp-context csv)))
  
	     ;; For each possible solution of the CSP var...
	     (do* ((solutions-left csv-domain (cdr solutions-left))
		   (soln (car solutions-left) (car solutions-left))
		   (plan nil))
		 ((or plan (null solutions-left)) plan)
	       
	       (let ((new-csp-vars nil)
		     (domain-wiped-out nil))

		 ;; Build a new set of CSP variables.  As we do so, 
		 ;; trim out the domain values that conflict with soln.
		 ;; If this process trims out all the domain values, we can
		 ;; skip this soln immediately.
		 (do* ((csp-vars-remaining other-csp-vars (cdr csp-vars-remaining))
		       (var (car csp-vars-remaining) (car csp-vars-remaining)))
		     ((or (null var) domain-wiped-out))
		   (let* ((same-action? (equal (csp-description csv)
					      (csp-description var)))
			  (new-domain 
			   (remove-if #'(lambda (v) 
					  (conflicts? soln v
						      same-action?
						      csv-context (csp-context var)
						      ))
				      (csp-aspects var))))
		     (setq new-csp-vars (cons (make-csp-var :context (csp-context var)
							    :description (csp-description var)
							    :aspects new-domain)
					      new-csp-vars))
		     (unless new-domain
		       (setq domain-wiped-out t))))
		 
		 (unless domain-wiped-out
		   ;; ...If the solution doesn't wipeout the domain of any other CSP var,
		   ;;    then call solve-csp recursively with the smaller set of csp-vars.
		   (if (eq soln :NO-EXEC)
		       (setq plan (solve-csp-fc-dvo new-csp-vars
						    (cons (cons (csp-description csv) csv-context)
							  selected-aspects)
						    next-goals
						    know-not-prop-cache))
		     (setq plan (solve-csp-fc-dvo new-csp-vars
						  (cons soln selected-aspects)
						  (union (act-preconditions soln) next-goals)
						  know-not-prop-cache)))
		   ))
	       )))
	 )
  
  )


;;
;; Okay, we now have the CSP in the form of a list of CSP variables.
;; Find solutions to the CSP, and try to plan using the aspects
;; chosen for the variables.  If the planning should fail, try another
;; solution to the CSP.
;;
(defun SOLVE-CSP-ALL (csp-vars selected-aspects next-goals)
  (declare (special action-levels prop-levels static-props goal-caches level contexts))
  (cond ((null csp-vars)
	 ;; No more variables left in the CSP.  Plan for 
	 ;; establishing the next-goals that will allow
	 ;; the selected-aspects to happen.

	 ;; Recurse on the selected-aspects

	 ;; *** NOTE ***
	 ;; We have to wait until now before we can really compute the
	 ;; preconditions of the NO-EXECs.  Until now, we didn't know for
	 ;; sure what worlds an executable aspect might have been taken.  For
	 ;; we need the (KNOW-NOT u){w} preconditions only for the worlds w
	 ;; in which we execute an aspect of A (where Au is a NO-EXEC).
	 (let* ((no-exec-subgoals 
		 ;; Note that we used cadr prop-levels here.  We use cadr
		 ;; because we're interested in examining the _previous_
		 ;; proposition level for the NO-EXEC's preconditions.
		 (noexec-precs selected-aspects (cadr prop-levels)))
		(plans (find-all-plans-in-pg1 (union next-goals no-exec-subgoals)
					(rest action-levels)
					(rest prop-levels)
					static-props
					(rest goal-caches)
					(1+ level))))
	   (when plans
	     (mapcar #'(lambda (plan) (cons selected-aspects plan))
		     plans))))


	;; There are still CSP variables left.  Assign a value to the 
	;; next variable.  Check for consistency (ie., check for mutexes 
	;; with other selected aspects).  If consistent, recurse on the
	;; remaining CSP variables.
	(t (let* ((csv (car csp-vars))
		  (csv-domain (csp-aspects csv)))

	     ;; We must potentially try all the values in CSV's domain
	     (do* ((domain csv-domain (cdr domain))
		   (v (car domain) (car domain))
		   (plans nil))
		 ;; If we find a plan, or if we run out of values in CSV's
		 ;; domain, return.
		 ((null domain) plans)

	       (tracer *trace-gp-csp* "~&  Using ~A to satisfy ~A~%" v csv)

	       ;; Is the value for chosen for CSV the NO-EXEC option?
	       (if (eq v :NO-EXEC)
		   (unless (noexec-conflicts (csp-description csv) 
					     (csp-context csv)
					     selected-aspects csp-vars 
					     ;; Consider the _previous_ proposition
					     ;; level.
					     (cadr prop-levels) contexts)
		     ;; Yep, using the NO-EXEC thing is okay.  HOWEVER, 
		     ;; choosing NO-EXEC for csv might force the choice 
		     ;; of NO-EXEC for other variables.  Why?  Because, 
		     ;; if we can't discriminate between worlds u and w 
		     ;; and we have chosen NO-EXEC in u, then we must
		     ;; choose NO-EXEC in w.
		     (setq plans (append plans
					 (solve-csp-all (noexec-new-csvs
							 (csp-description csv)
							 (csp-context csv)
							 (cdr csp-vars)
							 ;; Previous prop level
							 (cadr prop-levels))
							(append (noexec-new-aspects
								 (csp-description csv)
								 (csp-context csv)
								 (cdr csp-vars)
								 ;; Previous prop level
								 (cadr prop-levels))
								selected-aspects)
							;; See note above for why we can't
							;; staple the NO-EXEC's precs onto
							;; the next-goals list right now.
							next-goals))))

		 ;; No, the value V for CSV is not the NO-EXEC.  Check to see
		 ;; if V is mutex with any of the currently selected aspects.
		 ;; Figure that, if a NO-EXEC aspect has been chosen, then we
		 ;; have previously checked that that NO-EXEC is non-mutex with
		 ;; the option V.
		 (unless (member v selected-aspects 
				 :test #'(lambda (v sel-asp)
					   (and (not (consp sel-asp))
						(mutexp v sel-asp)
						;; We can check all mutexes, even those
						;; between PWs, because the mutex rule for
						;; induced aspects checks for the possiblity
						;; that NO-EXEC was chosen.
						)))
		   (setq plans (append plans 
				       (solve-csp-all (cdr csp-vars) 
						      (cons v selected-aspects)
						      (union (act-preconditions v) next-goals)))))))))))





(defun conflicts? (Au Bv same-action? u v) ;;;know-not-prop-exists?)
  (declare (special know-not-prop-cache prop-levels))
  ;; Do aspects Au and Bv conflict?  Au is an aspect of action A
  ;; in world u, and Bv is an aspect of action B in world v.

  ;; Au and Bv conflict if:
  ;; (1) Au is mutex with Bv, or
  ;; (2a) A == B and
  ;; (2b) Au is NO-EXEC and 
  ;; (2c) Bv is not NO-EXEC and
  ;; (2d) K!u:v is not available, or
  ;; (3a) A == B and
  ;; (3b) Au is not NO-EXEC and
  ;; (3c) Bv is NO-EXEC and
  ;; (3d) K!u:v is not available.
  (cond ((and (not (eq Au :NO-EXEC)) (not (eq Bv :NO-EXEC)))
	 (mutexp Au Bv))

	;; A NO-EXEC of one action is always consistent
	;; with any aspect of any _other_ action.
	((not same-action?) nil)

	;; If we can differentiate the two worlds, then
	;; there's no conflict
	((eq :YES (or (gethash (cons u v) know-not-prop-cache)
		      (setf (gethash (cons u v) know-not-prop-cache)
			(if (find-prop (list 'know-not u) v (cadr prop-levels))
			    :YES :NO))))
	 nil)


	;; I guess we couldn't differentiate the worlds.
	;; There's a conflict iff only one of Au and Bv
	;; is a :NO-EXEC.  This is the only remaining case
	;; in the cond, BTW...
	(t (or (and (eq Au :NO-EXEC) (not (eq Bv :NO-EXEC)))
	       (and (not (eq Au :NO-EXEC)) (eq Bv :NO-EXEC))))))
	


(defun sort-csp-vars (csp-vars)
  (sort csp-vars #'< :key #'(lambda (v)
			      (length (csp-aspects v)))))

(defun best-csp-var (csp-vars)
  (let ((min (apply #'min (mapcar #'(lambda (v) 
				      (length (csp-aspects v)))
				  csp-vars))))
    (find-if #'(lambda (v)
		 (eq (length (csp-aspects v)) min))
	     csp-vars)))

;;
;; Figure out what (KNOW-NOT u){w} predicates that we'll need
;; as preconditions of actions for which NO-EXEC was selected.
;;
;; If NO-EXEC was selected for Au, then, forall PW w
;; for which an Aw exists and is not NO-EXEC, include
;; (KNOW-NOT u){w}.
(defun noexec-precs (selected-aspects props)
  (let ((precs nil))
    (dolist (Au selected-aspects)
      (when (consp Au)			; a is a NO-EXEC
	(let ((c (find-cluster (list 'know-not (cdr Au)) props)))
	  ;; c is the cluster to which the (KNOW-NOT u){w}
	  ;; live, for all PW w
	  (dolist (Aw selected-aspects)
	    (when (and c
		       (not (consp Aw))	; Aw <> NO-EXEC
		       (equal (act-description Aw)
			      (car Au))) ; Aw comes from the same
					; action as Au does.

	      (pushnew 
	       (find-prop (list 'know-not (cdr Au)) (act-context Aw) props c)
	       precs :test #'equal))))))
    precs))


(defun noexec-conflicts (desc w selected-aspects csp-vars props contexts)
  ;; I want to know if it's okay to select NO-EXEC for action
  ;; A in world w.  Under what circumstances is that selection 
  ;; okay?

;;;4.  [snip]
;;;    In addition, for any action A
;;;        forall distinct worlds u,w 
;;;            if K!u:w 
;;;            then Aw=NO-EXEC is consistent with all values for Au
;;;            else Aw=NO-EXEC is only consistent with Au=NO-EXEC
  
  ;; That is, it's okay to use Aw=NO-EXEC iff we can discriminate
  ;; w from all worlds in which an aspect of A might occur.

  (let ((conflict? nil))		; Initially assume that there
					; is no conflict.
    (do ((u 1 (+ 1 u)))
	((or (> u contexts) conflict?) conflict?)
      (when (not (= u w))
	;; Do we know K!u:w?
	(unless (find-prop (list 'know-not u) w props)

	  ;; Let's say that (KNOW-NOT w){u} isn't present.  Then
	  ;; search for an Au that's not NO-EXEC in the selected
	  ;; aspects.  If we find one, then there's a conflict.

	  ;; If we don't find one, then we need to make sure that
	  ;; CSP var Au can have NO-EXEC as an option (if Au hasn't 
	  ;; already been set to NO-EXEC).  We don't
	  ;; need to update Au's domain here -- noexec-new-aspects
	  ;; will do that job.  But we must ensure that we don't 
	  ;; wipe out all of the possibilities for Au.

	  ;; No, we don't have (KNOW-NOT w){u}.  

	  (cond ((find-if #'(lambda (a)
			      (and (not (consp a))
				   (eq (act-context a) u)
				   (equal (act-description a) desc)))
			  selected-aspects)
		 ;; We have already selected an Au that is not
		 ;; a NO-EXEC thingy.  Yes, there's going to be a conflict.
		 ;; (i.e., we can't disciminate between PW w and u, but
		 ;; we're trying to avoid executing A in w but not in u.
		 (setq conflict? t))
		
		;; We haven't selected a directly-conflicting aspect
		;; for Au yet.  The only choice for Au will be NO-EXEC.
		;; Check to see if NO-EXEC is in Au's domain.  If Au
		;; can't be set as NO-EXEC, then state that there is a
		;; conflict.
		((and
		  ;; Au is still an active CSP variable...
		  (find-if #'(lambda (var)
				     (and (eq (csp-context var) u)
					  (equal (csp-description var) desc)))
			   csp-vars)
		  ;; ...but Au doen't have NO-EXEC as an option.
		  (null (find-if #'(lambda (v)
				    (eq v :NO-EXEC))
				(csp-aspects 
				 (find-if #'(lambda (var)
					      (and (eq (csp-context var) u)
						   (equal (csp-description var) desc)
						   ))
					  csp-vars)))))
		 (setq conflict? t)))

	  )))

    conflict?))



;;
;; Run through the list of CSP vars.  Remove any that
;; (1) have the same description as desc, and
;; (2) are in a context w for which we don't have
;;     (KNOW-NOT context){w}.
;; Return the resulting list.
;;
(defun NOEXEC-NEW-CSVS (desc context csp-vars props)
  ;; c is the cluster that holds the (KNOW-NOT context)
  ;; props.
  (let ((c (find-cluster (list 'know-not context) props)))
    (cond (
	   (null c)
	   (remove-if #'(lambda (var)
			  (equal (csp-description var) desc))
		      csp-vars))
	  (t (remove-if #'(lambda (var)
			    ;; Let's consider the CSP variable var
			    (and (equal (csp-description var) desc)
				 (null (find-prop (list 'know-not context)
					     (csp-context var) props c))))
			csp-vars)))))

;;
;; Run through the list of CSP vars.  If var Aw
;; has the same description as desc, and we don't
;; have a (KNOW-NOT context){w} proposition, then
;; assign CSP var Aw the value NO-EXEC.
;;
;; Ooh, ooh!  Don't forget to include the NO-EXEC
;; aspect for (desc . context)
;;
(defun NOEXEC-NEW-ASPECTS (desc context csp-vars props)
  (let ((new-aspects (list (cons desc context)))
	(c (find-cluster (list 'know-not context) props)))

    (dolist (var csp-vars)
      (when (and (equal (csp-description var) desc)
		 (null (find-prop desc (csp-context var) props c)))
	(push (cons desc (csp-context var)) new-aspects)))
    new-aspects))




;;;4.  For each pair of variables Au, Bv in Vi, let the
;;;    consistency constraints between Au and Bv be the set of
;;;    non-mutex pairs of aspects a:u and b:v where a:u and b:v
;;;    derive from actions A and B, respecitvely.  
;;;    In addition, for any action A
;;;        forall distinct worlds u,w 
;;;            if K!u:w 
;;;            then Aw=NO-EXEC is consistent with all values for Au
;;;            else Aw=NO-EXEC is only consistent with Au=NO-EXEC
;;;
;;;5.  Using {insert favorite CSP technique here}, choose an
;;;    assignment to each variable in Vi.  If no assignment
;;;    exists, backtrack.
;;;
;;;6.  Set Si-1 to be the set of preconditions of all the
;;;    non-NO-EXEC aspects assigned to the Au's.  For each Au
;;;    that is assigned the value NO-EXEC, union Si-1 with the
;;;    set K!u:w for each PW w where Aw <> NO-EXEC.  
;;;    If any of these K!u:w's are not present at level i-1 in the plan graph,
;;;    then backtrack(*).  Else, repeat this algorithm using i = i-2.




(defun ASSIGN-ALL-GOALS-CSP (remaining-goal-props next-goals actions know-not-prop-cache)
  (declare (special goal-props action-levels prop-levels goal-caches 
		    contexts know-not-prop-cache))
  (cond (remaining-goal-props
	 ;; Grab the next goal to be satisfied
	 (destructuring-bind (goal . rest) remaining-goal-props
	   (if (prop-static goal)	; If the goal is never deleted...
	       (assign-all-goals-csp rest next-goals actions know-not-prop-cache)
					; ...proceed with the
					; rest of the goals, AND, don't 
					; bother to propogate the goal.
	     ;; The goal isn't static.  Try all the different ways
	     ;; of establishing this goal.
	     (loop for establisher in (prop-establishers goal) append
		   ;; The selected goal set needs to avoid mutex goals between
		   ;; actions in the same context.  But, if two actions are mutex
		   ;; and in different worlds, that's okay -- the mutex will either
		   ;; be cleared up by a NO-EXEC, or we'll fail later (right?!?)
		   (unless (member establisher actions :test #'mutexp)
		     ;; we can use mutexp because the inclusion or exclusion of
		     ;; induced mutexes is being handled elsewhere.

		     (assign-all-goals-csp rest
					   (union (act-preconditions establisher) next-goals)
					   (adjoin establisher actions)
					   know-not-prop-cache))))))

	(t
	 ;; All the goals at this level are now satisfied by the aspects in 
	 ;; aspects.  Build the CSP and choose a solution to it.  The solution
	 ;; will bind the actions to aspects in the other possible worlds.
	 (let ((csp-vars nil))		; The list of CSP variables for which we need
					; to solve.
	   (dolist (a actions)		; Each chosen aspect has an associated
					; CSP variable.
	     (let ((Au (find-if #'(lambda (cv)
				    (and (equal (csp-description cv) (act-description a))
					 (eq (csp-context cv) (act-context a))))
				csp-vars)))
	       (cond ((and (null Au)
			   (not (eq 'persist (act-operator a))))
		      ;; The CSP variable for a hasn't been added to the list of
		      ;; csp-vars yet.  Create the variable.  For each PW u, the
		      ;; CSP variable can be any of the aspects available in u
		      ;; for that operator.  Of course, for the context of a, the
		      ;; only available aspect is a.
		      (do ((u 1 (+ 1 u)))
			  ((> u contexts))

			(cond ((eq u (act-context a))
			       ;; We're creating the CSP variable for the
			       ;; action/world pair that we've selected.
			       ;; There's only one legal possibility --
			       ;; the aspect that we have already selected.

			       (push (make-csp-var :context u
						   :description (act-description a)
						   :aspects (list a))
				     csp-vars))
			  
			  ;; Let action A have one aspect, a.  Suppose that a can
			  ;; be executed in PW u, but a's preconditions aren't true
			  ;; in PW v (assume that A has no preconditions, but 
			  ;; there is a conditional effect).
			  ;; Now, suppose that a{u} has been added to the plan.
			  ;; What should we do about A in world v?  Should Av's
			  ;; domain be { NO-EXEC }?  Should the domain be empty?
			  ;; Should we have an Av at all?  I'll vote for the latter
			  ;; case.  Case #1 doesn't work, because we don't need
			  ;; to _prevent_ A from executing in v; A simply doesn't
			  ;; _do_ anything in world v.  Case #2 blows holes in 
			  ;; any CSP solver, as empty domains for CSP variables
			  ;; is usually a Bad Thing.  Thus, option #3 is the best
			  ;; bet until I hear otherwise.


			      (t
			       (let ((aspects-in-u (remove-if-not 
						    #'(lambda (asp)
							(eq (act-context asp) u))
						    (act-aspects a)))
				     (know-not-u? 
				      (find-cluster (list 'know-not u) (cadr prop-levels))))
									    
				 (when aspects-in-u
				   (push (make-csp-var
					  :context u
					  :description (act-description a)

					  :aspects (append aspects-in-u 
							   (when know-not-u?
							     (list :NO-EXEC))))
					 csp-vars)))))))
			
		     ((eq 'persist (act-operator a))
		      ;; The aspect is a no-op.  Create a variable for that
		      ;; aspect, with only itself as the domain of possible 
		      ;; values.
		      (push (make-csp-var :context (act-context a)
					  :description (act-description a)
					  :aspects (list a))
			    csp-vars))

		     (t
		      ;; The CSP variable has already been added to the CSP.
		      ;; Update the available aspect choices for the variable.
		      (setf (csp-aspects Au) (list a))))))
		    
	   ;; Forward checking is not available when finding all plans.
	   ;; Not because of an intrinsic limitation, but rather because
	   ;; there has been no call for that feature.
	   (solve-csp-all csp-vars nil next-goals)
	     
))))


;;; Need to check to make sure every included action at this level doesn't do something
;;; undesirable if we are in a different context than expected.  So, for
;;; each action at this level, need to consider all aspects of the action
;;; and verify that either 1) the aspect cannot occur because its preconditions
;;; are mutex with preconditions required by other actions already in the plan, 
;;; or 2) the aspect doesn't harm anything, i.e. it doesn't clobber any goals 
;;; at the current level, and it doesn't clobber preconditions of other actions 
;;; at this level. If neither of these conditions hold, then we must confront 
;;; the aspect by introducing additional goals at the next lower level to 
;;; prevent the preconditions of the aspect from being satisfied.

(defun check-unexpected-consequences (next-goals actions goal-props)
  (declare (special actions))
  (tracer *trace-gp-bc* " actions ~a" actions)
  (check-unexpected-consequences1 next-goals actions goal-props))

(defun check-unexpected-consequences1 (next-goals remaining-actions goal-props)
  (declare (special goal-props action-levels prop-levels static-props goal-caches level))
  (if (null remaining-actions)
    (find-plan-in-pg1 next-goals
                      (rest action-levels)
                      (rest prop-levels)
		      static-props
                      (rest goal-caches) (1+ level))
    (let* ((action (pop remaining-actions))
           (context (act-context action))
           (previous-prop-level (second prop-levels)))
      (declare (special action context previous-prop-level))
      (check-unexpected-aspects (act-aspects action) remaining-actions next-goals))))

(defun check-unexpected-aspects (aspects remaining-actions next-goals)
  (declare (special action context actions goal-props previous-prop-level))
  (if (null aspects)
    (check-unexpected-consequences1 next-goals remaining-actions goal-props)
    (let  ((aspect (pop aspects)))
      (if (and (not (eq action aspect))               ; can't be the current action
               (not (member aspect actions))          ; can't be in the plan already
               ;; can't be in the same context
               (not (eql context (act-context aspect)))
               ;; must clobber something in the plan
               ;; this means it must be mutex with existing actions in the plan
               ;; (although it could be mutex because of incompatable preconditions)
               (find aspect actions :test #'mutexp)
               ;; must have compatable preconditions 
               ;; (could be mutex because of incompatable preconditions)
               (loop for pre in (act-preconditions aspect) never
                     (find pre next-goals :test #'mutexp))
               )
        ;;; Oh crap.  We have to confront this aspect.
        ;;; Loop throught the preconditions and find those that we can possibly confront.
        ;;; For each one of these, add it into the next-goals and recur.
        (loop for pre in (act-preconditions aspect) thereis
          (unless (member pre next-goals)
            (let* ((inverted (invert (prop-description pre)))
                   (cluster (find-cluster inverted previous-prop-level)))
              (when cluster
                (let ((confront (find (prop-context pre) (cluster-props cluster)
                                      :key #'prop-context)))
                  (when confront
                    (check-unexpected-aspects aspects remaining-actions 
                                              (cons confront next-goals))))))))
        (check-unexpected-aspects aspects remaining-actions next-goals)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simplify the plan by 
;;;     1) removing null steps, 
;;;     2) removing duplicate steps in different contexts
;;;     3) removing context information

(defun simplify-plan (plan)
  (loop for plan-level in plan
        unless (eq plan-level t)
        collect
        (let ((simplified nil))
          (dolist (step plan-level)
            (unless (and (not (consp step)) (eq 'persist (act-operator step)))
	      (cond ((consp step)
		     ;; step is really an exclusion marker.  step says
		     ;; that we're not supposed to execute the action 
		     ;; in the world specified.
		     (let ((real-step (find-if #'(lambda (s)
						   (equal (car step)
							  (car s)))
					       simplified)))
		       (if real-step
			   (nconc real-step (list (cdr step)))
			 (pushnew (list (car step) (cdr step)) simplified))))
		    (t
		     (pushnew (list (act-description step)) simplified 
			      :test #'(lambda (s1 s2)
					(equal (car s1) (car s2))))))))
          simplified))
  )

(defun print-plan (plan)
  (loop for steps in plan
        for level upfrom 1 do
        (format t "~% ~a: ~{~a ~}" level steps)))
