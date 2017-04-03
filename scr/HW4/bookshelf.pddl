;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bookshelf domain
(define (domain bookshelf-domain)
  (:requirements :strips)
  
  (:predicates
      (Clear ?x)
      (On ?x ?y ?z)
      (Unread ?x)
      (Book ?x) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the Move and Read actions below
;; See other pddl files for examples

	 
	 
) ;;; Close parenthesis for domain definition

 
;;; Initial problem state
(define (problem bookshelf-organize)
  (:domain bookshelf-domain)
  (:objects Sh0 Sh1 Sh2 RN B P MRT)

  ;;; Define :init and :goal here
  ;;; See other pddl files for examples


  (:length (:serial 20) (:parallel 20)))
