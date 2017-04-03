(in-package :domains)

(defun rep-plan (reps problem)
  (when (> reps 0)
    (time (gp::plan problem))
    (rep-plan (- reps 1) problem)))

(defun test-casandra (reps)
  (format t "~&evanston:~%")
  (rep-plan reps 'a1-prob)
  (format t "~&bomb:~%")
  (rep-plan reps 'a2-prob)
  (format t "~&fetch-1:~%")
  (rep-plan reps 'a3-prob)
  (format t "~&fetch-2:~%")
  (rep-plan reps 'a4-prob)
  (format t "~&coin-1:~%")
  (rep-plan reps 'a5-prob)
  t)


;;; A.1  A plan to get to Evanston

(define (domain A1-dom) (:requirements :conditional-effects :sensing)
  (:action go-to-western-at-belmont
	   :precondition (at start)
	   :effect (and (on western) (on belmont) (not (at start))))
  
  (:action check-traffic-on-western
	   :precondition ()
	   :effect (observes (and (on western) (traffic-bad))))
  
  (:action take-belmont
	   :precondition ()
	   :effect (and (when (and (on belmont) (traffic-bad))
			  (and (not (on western)) (on ashland)))
			(when (or (not (on belmont)) (not (traffic-bad)))
			  (error))))
  
  (:action take-ashland
	   :precondition ()
	   :effect (and (when (on ashland) (at evanston))
			(when (not (on ashland)) (error))))
  
  (:action take-western
	   :precondition ()
	   :effect (and (when (and (on western) (not (traffic-bad))) (at evanston))
			(when (or (not (on western)) (traffic-bad)) (error)))))

(define (problem A1-prob) (:domain A1-dom)
  (:init (at start)
	 (not (on western)) (not (on belmont)) (not (on ashland))
	 (not (at evanston))
	 (uncertain (traffic-bad))
	 (not (error)))
  (:goal (and (at evanston) (not (error)))))


;;; A.2  Disarming a bomb

(define (domain A2-dom) (:requirements :conditional-effects :sensing)
  (:action X-ray
	   :parameters (?pkg)
	   :precondition (object ?pkg)
	   :effect (observes (contains-bomb ?pkg)))
  
  (:action move-from-rug-to-toilet
	   :parameters (?pkg)
	   :precondition (object ?pkg)
	   :effect (and (when (at ?pkg rug) (and (not (at ?pkg rug)) (at ?pkg toilet)))
			(when (not (at ?pkg rug)) (error))))
  
  (:action dunk
	   :parameters (?pkg)
	   :precondition (object ?pkg)
	   :effect (and (when (at ?pkg toilet) (and (wet ?pkg)
						    (when (contains-bomb ?pkg)
						      (bomb-disarmed))))
			(when (not (at ?pkg toilet)) (error))
			(when (not (contains-bomb ?pkg)) (error)))))

(define (problem A2-prob) (:domain A2-dom)
  (:objects pkg1 pkg2)
  (:init (at pkg1 rug)
	 (at pkg2 rug)
	 (oneof (contains-bomb pkg1) (contains-bomb pkg2))
	 (not (bomb-disarmed))
	 (not (error)))
  (:goal (and (bomb-disarmed) (not (error)))))
	 


;;; A.3  Fetching a package.

(define (domain A3-dom) (:requirements :conditional-effects :sensing)
  (:action ask-about-package
	   :effect (and (observes (package-at loc-1))
			(observes (package-at loc-2))))
  
  (:action drive
	   :parameters (?car ?loc)
	   :precondition (and (car ?car) (location ?loc))
	   ;; The paper doesn't include the (not (available ?car))
	   ;; prop, but it seems necessary.
	   :effect (and (when (available ?car) (and (at ?loc) (not (available ?car))))
			(when (not (available ?car)) (error))))
  
  (:action finish
	   :effect (and (when (and (not (error)) (package-at loc-1) (at loc-1))
			  (happy))
			(when (and (not (error)) (package-at loc-2) (at loc-2))
			  (happy)))))

(define (problem A3-prob) (:domain A3-dom)
  (:init (car car-1) (car car-2)
	 (location loc-1) (location loc-2)
	 (available car-1) (not (available car-2))
	 (oneof (package-at loc-1) (package-at loc-2))
	 (not (error))
	 (not (happy)))
  (:goal (happy)))



;;; A.4  Fetching another package
(define (domain A4-dom) (:requirements :conditional-effects :sensing)
  (:action ask-about-package
	   :effect (and (observes (package-at loc-1))
			(observes (package-at loc-2))))
  
  (:action ask-about-car
	   :effect (and (observes (available car-1))
			(observes (available car-2))))

  (:action drive
	   :parameters (?car ?loc)
	   :precondition (and (car ?car) (location ?loc))
	   ;; The paper doesn't include the (not (available ?car))
	   ;; prop, but it seems necessary.
	   :effect (and (when (available ?car) (and (at ?loc) (not (available ?car))))
			(when (not (available ?car)) (error))))
  
  (:action finish
	   :effect (and (when (and (not (error)) (package-at loc-1) (at loc-1))
			  (happy))
			(when (and (not (error)) (package-at loc-2) (at loc-2))
			  (happy)))))

(define (problem A4-prob) (:domain A4-dom)
  (:init (car car-1) (car car-2)
	 (location loc-1) (location loc-2)
;	 (available car-1) (not (available car-2))
	 (oneof (package-at loc-1) (package-at loc-2))
	 (oneof (available car-1) (available car-2))
	 (not (error))
	 (not (happy)))
  (:goal (happy)))



;;; A.5  Tossing a coin

(define (domain A5-dom) (:requirements :conditional-effects :sensing)
  (:action toss-coin
	   :effect (and (when (holding-coin) (not (holding-coin)))
			(when (fated-on-edge) (on-edge))
			(when (fated-flat) (flat-coin))
			(when (not (holding-coin)) (error))))
  
  (:action inspect-and-decide-coin
	   :effect (observes (on-edge)))
  
  (:action tip-coin
	   :effect (and (when (on-edge) (flat-coin))
			(when (not (on-edge)) (error)))))

(define (problem A5-prob) (:domain A5-dom)
  (:init (holding-coin) (not (error))
	 (oneof (fated-on-edge) 
		(fated-flat))
	 (not (on-edge)) 
	 (not (flat-coin)) 
	 )
  (:goal (and (flat-coin) (not (error)))))



;;; A.6  Tossing another coin

(define (domain A6-dom) (:requirements :conditional-effects :sensing)
  (:action toss-coin
	   :effect (and (when (holding-coin) (not (holding-coin)))
			(when (fated-on-edge) (on-edge))
			(when (fated-heads-up) (and (heads-up) (flat-coin)))
			(when (fated-tails-up) (and (tails-up) (flat-coin)))
			(when (not (holding-coin)) (error))))
  
  (:action examine-flattness
	   :effect (and (observes (flat-coin))
			(observes (on-edge))))
  
  (:action examine-side-up
	   :effect (and (observes (heads-up))
			(observes (tails-up))))
  
  (:action tip-coin
	   :effect (and (when (and (on-edge) (fated-tips-heads)) 
			  (and (not (on-edge)) (heads-up) (flat-coin)))
			(when (and (on-edge) (fated-tips-tails))
			  (and (not (on-edge)) (tails-up) (flat-coin)))
			(when (not (on-edge)) (error))))
  
  (:action turn-over
	   :effect (and (when (heads-up) (and (not (heads-up)) (tails-up)))
			(when (tails-up) (and (not (tails-up)) (heads-up)))
			(when (and (not (heads-up)) (not (tails-up))) (error)))))

(define (problem A6-prob) (:domain A6-dom)
  (:init (holding-coin)
	 (oneof (and (not (fated-heads-up))
		     (not (fated-tails-up))
		     (fated-on-edge) (fated-tips-tails))
		(and (not (fated-heads-up))
		     (not (fated-tails-up))
		     (fated-on-edge) (fated-tips-heads))
		(and (fated-heads-up) 
		     (not (fated-tails-up))
		     (not (fated-on-edge))
		     (not (fated-tips-tails)) (not (fated-tips-heads)))
		(and (fated-tails-up) 
		     (not (fated-heads-up))
		     (not (fated-on-edge))
		     (not (fated-tips-tails)) (not (fated-tips-heads))))
	 (not (error))
	 (not (heads-up)) (not (tails-up)) (not (on-edge)) (not (flat-coin)))
  (:goal (and (flat-coin) (heads-up))))



(define (domain A6-dom-2) (:requirements :conditional-effects :sensing)
  ;; (s0,s1)
  ;; FF == holding coin
  ;; TF == on-edge
  ;; FT == heads-up
  ;; TT == tails-up

  (:action toss-coin
	   :effect (and (when (and (not (s0)) (not (s1)))
			  (and (when (fated-on-edge) (s0))
			       (when (fated-heads-up) (s1))
			       (when (fated-tails-up) (and (s0) (s1)))))
			(when (or (s0) (s1)) (error))))
  
  (:action examine-flattness
	   :effect (observes (s1)))
  
  (:action examine-side-up
	   :effect (observes (s0)))
  
  (:action tip-coin
	   :effect (and (when (and (s0) (not (s1)))
			  (and (when (fated-tips-heads) (and (not (s0)) (s1)))
			       (when (fated-tips-tails) (and (s0) (s1)))))
			(when (or (not (s0)) (s1)) (error))))
  
  (:action turn-over
	   :effect (and (when (and (s1) (s0)) (not (s0)))
			(when (and (s1) (not (s0))) (s0))
			(when (not (s1)) (error)))))

(define (problem A6-prob-2) (:domain A6-dom-2)
  (:init (not (error))

	 ;; (s0,s1)
	 ;; FF == holding coin
	 ;; TF == on-edge
	 ;; FT == heads-up
	 ;; TT == tails-up
	 
	 (not (s0)) (not (s1))
	 (oneof (and (not (fated-heads-up))
		     (not (fated-tails-up))
		     (fated-on-edge) (fated-tips-tails))
		(and (not (fated-heads-up))
		     (not (fated-tails-up))
		     (fated-on-edge) (fated-tips-heads))
		(and (fated-heads-up) 
		     (not (fated-tails-up))
		     (not (fated-on-edge))
		     (not (fated-tips-tails)) (not (fated-tips-heads)))
		(and (fated-tails-up) 
		     (not (fated-heads-up))
		     (not (fated-on-edge))
		     (not (fated-tips-tails)) (not (fated-tips-heads)))))
  (:goal (and (not (s0)) (s1))))


;;; A.7  Opening a Door

(define (domain A7-dom) (:requirements :conditional-effects :sensing)
  (:action kick
	   :effect (and (when (kick-breaks-foot) (foot-broken))
			(when (kick-breaks-lock) (lock-intact))))
  
  (:action examine-lock
	   :effect (and (observes (lock-intact))
			(observes (locked))))
  
  (:action pick
	   :effect (and (when (and (lock-intact) (locked)) (not (locked)))
			(when (or (not (lock-intact)) (not (locked))) (error))))
  
  (:action open-door
	   :effect (and (when (not (locked)) (open))
			(when (locked) (error)))))

(define (problem A7-prob) (:domain A7-dom)
  (:init (lock-intact)
	 (not (foot-broken))
	 (uncertain (locked))
	 (oneof (kick-breaks-foot) (kick-breaks-lock))
	 (not (error))
	 (not (open)))
  (:goal (and (open) (not (error)))))
