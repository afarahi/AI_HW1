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

;;; $Id: logic.lisp,v 1.9 1998/06/13 07:06:35 corin Exp $

(in-package :logic)

(export '(variable? +false+ plug instance? invert clauses goal-clauses
	  conjuncts inverted? keyword? check-neq check-eq ground?
          cnf dnf dnf* collect-variables collect-atoms canonicalize-variables
	  eval-f
          partition-cnf partition partition-literals partition-clauses))

(import '(domains:iff domains:oneof domains:uncertain))

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defmacro change (place form)
  `(setf ,place ,(subst place '* form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; variables and unification
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;a variable is a symbol starting with a question mark, e.g. ?x, ?y, etc.

(defun VARIABLE? (thing)
  (and (symbolp thing)
       (char= #\? (elt (symbol-name thing) 0))))


(defun GROUND? (prop)
  (cond ((null prop) t)
	((not (consp prop)) (not (variable? prop)))
	(t (and (ground? (car prop))
		(ground? (cdr prop))))))

(defconstant +FALSE+ :false)

;;;A cons-efficient variation on plug.
(defun PLUG (exp bindings)
  (cond ((null bindings) exp)
        ((null exp) nil)
        ((variable? exp)
         (let ((lookup (assoc exp bindings)))
           (if lookup (cdr lookup) exp)))
        ((atom exp) exp)
        (t (let ((car (plug (car exp) bindings))
                 (cdr (plug (cdr exp) bindings)))
             (if (and (eql car (car exp))
                      (eql cdr (cdr exp)))
               exp
               (cons car cdr))))))

;;;A "one-sided" unification algorithm.  Only one of the arguments (the first)
;;;is allowed to contain variables.  Returns +false+ if the props don't unify.
;;;nil means the two expressions are identical.
(defun instance? (var-exp constant-exp bindings)
  (cond ((eql constant-exp var-exp) bindings)
        ((and (consp var-exp)
              (consp constant-exp))
         (let ((new-bindings (instance? (first var-exp)
                                        (first constant-exp)
                                        bindings)))
           (if (eq new-bindings +false+) 
             +false+
             (instance? (rest var-exp)
                        (rest constant-exp) new-bindings))))
        ((variable? var-exp)
         ;;;first see whether there is already a binding for the var.
         (let ((var-binding (assoc var-exp bindings)))
           (if var-binding
             (if (equal (cdr var-binding) constant-exp)
               bindings
               +false+)
             (cons (cons var-exp constant-exp) bindings))))
        (t +false+)))

;; Check whether clause c1 and c2 are not equal.  Take into account the 
;; possible variable bindings.  If one of the clauses is an unbound 
;; variable, then the result is nil.
(defun CHECK-NEQ (c1 c2 bindings)
  (when (variable? c1) (setq c1 (cdr (assoc c1 bindings))))
  (when (variable? c2) (setq c2 (cdr (assoc c2 bindings))))
  (and c1 c2
       (not (equal c1 c2))))

;; Check whether clauses c1 and c2 are equal.  Take into account
;; the variable bindings.  If one of the clauses is an unbound
;; variable, the result is nil.
(defun CHECK-EQ (c1 c2 bindings)
  (when (variable? c1) (setq c1 (cdr (assoc c1 bindings))))
  (when (variable? c2) (setq c2 (cdr (assoc c2 bindings))))
  (and c1 c2
       (equal c1 c2)))    



(defun collect-variables (expression)
  (let ((variables nil))
    (declare (special variables))
    (collect-variables1 expression)
    (nreverse variables)))

(defun collect-variables1 (expression)
  (declare (special variables))
  (cond ((consp expression)
         (collect-variables1 (first expression))
         (collect-variables1 (rest expression)))
        ((variable? expression)
         (pushnew expression variables))))

(defun canonicalize-variables (expression)
  (sublis (loop for v in (collect-variables expression)
                for n upfrom 1
                collect
                (cons v (intern (format nil "?~a" n))))
          expression))
          

(defun atom? (expression)
  ;; An expression is an atom if:
  ;; (1) The expression is a symbol, or 
  ;; (2) The expression is a list AND 
  ;;     the first element of the list is not a keyword.
  (cond ((not (consp expression)) t)
	((eq (first expression) 'and) nil)
	((eq (first expression) 'or) nil)
	(t t)))
  
(defun collect-atoms (expression)
  (collect-atoms-helper expression nil))

(defun collect-atoms-helper (expression atoms)
  (cond ((atom? expression)
	 (cons expression atoms))
	((consp expression)
	 (dolist (expr (rest expression))
	   (setq atoms (collect-atoms-helper expr atoms)))
	 atoms)))


;; Evaluate formula F.  F has the list of atoms atoms, 
;; and those atoms have values in atomic-values
(defun eval-f (F atoms atomic-values)
  ;; Build an assoc list of (atom . value).  
  ;; atoms is presently a list of atom.
  ;; atomic-values is a list of (value . proposition).

  (let ((atom-assoc nil))
    (do* ((atoms-left atoms (cdr atoms-left))
	  (values-left atomic-values (cdr atomic-values))
	  (a (car atoms-left) (car atoms-left))
	  (v (caar values-left) (caar values-left)))
	((null atoms-left)
	 (let ((F-value (eval (assign-values F atom-assoc))))
	   F-value))			; Here's the value returned
					; from eval-f.
      (push (cons a v) atom-assoc))))

;; _really_ compute F's value.  atomic-values is an a-list
;; of (atom . value) pairs.
(defun assign-values (F atomic-values)
  (cond ((null F) nil)
	((and (consp F)
	      (or (eq (car F) 'and)
		  (eq (car F) 'or)))
	 (cons (car F) (mapcar #'(lambda (subF)
				   (assign-values subF atomic-values))
			       (cdr F))))
	(t (cdr (assoc F atomic-values :test #'equal)))))

  

;;;Stuff for handling nots.
(defun inverted? (prop)
  (eq (car prop) 'not))

(defun invert (p)
  (case (when (consp p) (first p))
    (not (second p))
    (and `(or ,@(loop for c in (rest p) collect (invert c))))
    (or `(and ,@(loop for c in (rest p) collect (invert c))))
    (forall (invert (list 'and (gp::process-quantifier-many p nil))))
    (t `(not ,p))))

;; Return the list of conjunctive clauses in expression.
(defun clauses (expression)
  (when expression
    (if (and (consp expression) (eq 'and (first expression)))
      (rest expression)
      (list expression))))

(defun goal-clauses (exprs init-props)
  ;; Input is a list of expressions.
  ;; Return a conjunction of goal clauses.
  ;; Do the Right Thing with universal quantification.
  (cond ((null exprs) '())
	((and (consp (car exprs)) (eq 'domains::and (first (car exprs))))
	 (goal-clauses (append (cdar exprs) (cdr exprs)) init-props))
	((and (consp (car exprs)) (eq 'domains::forall (first (car exprs))))
	 (goal-clauses (append (gp::process-quantifier-many (car exprs) nil)
			       (cdr exprs)) init-props))
	(t (cons (car exprs) (goal-clauses (cdr exprs) init-props)))))


(defun conjuncts (expr)
  (cond ((null expr) '())
	((not (consp expr)) expr)
	((and (consp (car expr))
	      (eq 'and (caar expr)))
	 (conjuncts (append (cdar expr) (cdr expr))))
	((consp (car expr))
	 (cons (cons (caar expr) 
		     (mapcar #'conjuncts (cdar expr))) 
	       (conjuncts (cdr expr))))
	(t (cons (car expr) (conjuncts (cdr expr))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  converts an expression into cnf
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cnf (expression)
  (let ((finished-clauses nil))
    (declare (special finished-clauses))
    (cnf-clause expression)
    finished-clauses))

(defun cnf-clause (clause)
  (declare (special finished-clauses))
  (case (when (consp clause) (first clause))
    (and (mapc #'cnf-clause (rest clause)))
    (or (cnf-or (rest clause)))
    (not (let ((i (second clause)))
            (if (and (consp i) (keyword? (first i)))
              (cnf-clause (invert i))
              (push (list clause) finished-clauses))))
    (oneof (cnf-oneof (rest clause)))
    (if (cnf-or `((not ,(second clause)) ,(third clause))))
    (iff (cnf-iff (rest clause)))
    ;;(uncertain (cnf-or (rest clause) clauses terms t))
    (t (push (list clause) finished-clauses))))

(defun cnf-or (or-clause)
  (cnf-or-distribute (loop for c in or-clause collect (cnf c)) nil))

(defun cnf-or-distribute (or-clause terms)
  (declare (special finished-clauses))
  (if or-clause
    (dolist (term (pop or-clause))
      (cnf-or-distribute or-clause (append term terms)))
    (push terms finished-clauses)))

(defun cnf-oneof (l)
  ;; (oneof a b c) ==> (or a b c) (or ~a ~b) (or ~a ~c) (or ~b ~c)
  (cnf-or l)
  (let ((inverted (loop for c in l collect (invert c))))
    (loop for (c1 . rest) on inverted do
        (dolist (c2 rest) 
          (cnf-or (list c1 c2))))))

(defun cnf-iff (l)
  (declare (special finished-clauses))
  (let ((inverted (loop for c in l collect (invert c))))
    (loop for (c1 . rest) on l
          for (ic1 . irest) on inverted do
          (loop for c2 in rest
                for ic2 in irest do
                (cnf-or (list c1 ic2))
                (cnf-or (list ic1 c2))))))

(defun keyword? (literal)
  (and (symbolp literal)
       (member literal '(and or not oneof forall exists if iff))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Puts an expression in dnf (i.e. generates all possible worlds)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dnf (expression)
  (let ((processed nil))
    (declare (special processed))
    (dnf1 (clauses expression) nil)
    processed))

(defun dnf1 (clauses terms)
  (declare (special processed))
  (if (null clauses)
    (push terms processed)
    (let ((clause (pop clauses)))
      (case (and (consp clause) (first clause))
        (and (dnf1 (append (rest clause) clauses) terms))
        (not (let ((i (second clause)))
                (if (and (consp i) (keyword? (first i)))
                  (dnf1 (invert i) terms)
                  (when (consistent? clause terms)
                    (dnf1 clauses (adjoin clause terms :test #'equal))))))
        (oneof 
	 ;; Exactly one of the oneof-clauses can be true in each of
	 ;; the possible worlds.  Hence, we must ensure that, if clause
	 ;; c isn't the Chosen One, then that c is _not_ true.
         (let* ((oneof-clauses (rest clause))
                (inverted (loop for c in oneof-clauses collect (invert c)))
                (new-expr (append inverted clauses))
                (head nil))
           (loop for c in oneof-clauses
                 for (i . tail) on new-expr
                 do
                 (dnf1 `(,@head ,c ,@tail) terms)
                 (push i head))))
        (or (dnf-or (rest clause) clauses terms))
        (uncertain (dnf-or (rest clause) clauses terms t))
        (if (dnf-or (list (invert (second clause)) (third clause))
                     clauses terms))
	(iff (dnf-or `((and ,(second clause) ,(third clause))
		       (and ,(invert (second clause)) ,(invert (third clause))))
		     clauses terms))
        (t (when (consistent? clause terms)
             (dnf1 clauses (adjoin clause terms :test #'equal))))))))

(defun dnf-or (or-clauses clauses terms &optional (satisfied nil))
  (let ((inverted (loop for c in or-clauses collect (invert c))))
    (dnf-or1 or-clauses inverted clauses terms satisfied)))

(defun dnf-or1 (or-clauses inverted clauses terms satisfied)
  (cond ((null or-clauses)
         (when satisfied (dnf1 clauses terms)))
        (t (let ((clause (pop or-clauses))
                 (invert (pop inverted)))
             (dnf-or1 or-clauses inverted (cons clause clauses) terms t)
             (dnf-or1 or-clauses inverted (cons invert clauses) terms satisfied)))))

(defun consistent? (literal set)
  (not (find (invert literal) set :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Puts a cnf expression in dnf (i.e. generates all possible worlds)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dnf* (cnf-expression)
  (let ((processed nil))
    (declare (special processed))
    (dnf*1 cnf-expression nil)
    processed))

(defun dnf*1 (clauses terms)
  (declare (special processed))
  (if (null clauses)
    (push terms processed)
    (let ((clause (pop clauses)))
        (dnf*-or clause (mapcar #'invert clause) clauses terms nil))))

(defun dnf*-or (or-clauses inverted clauses terms satisfied)
  (if or-clauses
    (let ((clause (pop or-clauses))
          (invert (pop inverted)))
      (add-term clause terms or-clauses inverted clauses t)
      (add-term invert terms or-clauses inverted clauses satisfied))
    (when satisfied (dnf*1 clauses terms))))

(defun add-term (term terms or-clauses inverted clauses satisfied)
  (unless (member (invert term) terms :test #'equal)
    (dnf*-or or-clauses inverted clauses (adjoin term terms :test #'equal) satisfied)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Partion a cnf expression into independent groups of clauses
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct partition
  (literals nil)
  (clauses nil))

(defun partition-cnf (expression)
  (let ((partitions nil))
    (dolist (clause expression)
      (let ((literals nil)
            (clauses nil)
            (partition nil))
        (dolist (literal clause)
          (when (and (consp literal) (eq (first literal) 'not))
            (setq literal (second literal)))
          (let ((previous-partition 
                 (dolist (p partitions) 
                   (when (member literal (partition-literals p) :test #'equal)
                     (return p)))))
            (if  previous-partition 
              (unless (eq partition previous-partition)
                ;; need to combine the partitions
                (change literals
                        (nconc * (partition-literals previous-partition)))
                (change clauses
                        (nconc * (partition-clauses previous-partition)))
                (if partition
                  (change partitions (delete previous-partition *))
                  (setq partition previous-partition)))
              (push literal literals))))
        (unless partition
          (setq partition (make-partition))
          (push partition partitions))
        (setf (partition-literals partition) literals
              (partition-clauses partition) (cons clause clauses))))
    partitions))
