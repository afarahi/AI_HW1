;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bookshelf domain
(define (domain bookshelf-domain)
  (:requirements :strips)
  
  (:predicates
      (Clear ?x)
      (On ?x ?y)
      (Unread ?x)
      (Book ?x) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the Move and Read actions below
;; See other pddl files for examples

  (:action Move
	     :parameters (?x ?y ?z)
	     :precondition (and  (Clear ?x)
				 (On ?x ?y)
				 (Clear ?z)
                           )
	     :effect (and (On ?x ?z) 
                          (not (On ?x ?y))
                          (Clear ?y)
                          (not (Clear ?z))
                     )
  )
	 
  (:action Read
	     :parameters (?x)
	     :precondition (and  (Book ?x)
				 (Clear ?x)
				 (Unread ?x)
                           )
	     :effect (not (Unread ?x))
  )
	 
	 
) ;;; Close parenthesis for domain definition

 
;;; Initial problem state
(define (problem bookshelf-organize)
  (:domain bookshelf-domain)
  (:objects Sh0 Sh1 Sh2 RN B P MRT)

  ;;; Define :init and :goal here
  ;;; See other pddl files for examples
  (:init (On MRT P) (On P B) (On B RN) (On RN Sh0) 
         (Clear MRT) (Clear Sh1) (Clear Sh2)
         (Book RN) (Book P) (Book B) (Book MRT)
         (Unread RN) (Unread P) (Unread B) (Unread MRT))
  (:goal (and (Clear RN) (On RN MRT) (On MRT P) (On P B)
              (On B Sh0) (not (Unread RN)) (not (Unread B))
              (not (Unread P)) (not (Unread MRT))))
  (:length (:serial 20) (:parallel 20)))
