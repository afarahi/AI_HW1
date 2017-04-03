(in-package :domains)

;;; This file contains some domains that exploit uncertainty:
;;;  - bt       is the classic bomb in the toilet domain with no sensing
;;;  - bct      makes the toilet clog if an empty package is dunked. 
;;;             flushing fixes the problem
;;;  - bcts     like bct, except allows multiple toilets
;;;  - bt-asym  demonstrates asymmetric subgoals (different subgoal structure
;;;             in different contexts). Also demonstrates confrontation

(define (domain bt)
  (:requirements :strips :equality :typing :conditional-effects :uncertainty)
  (:action dunk
             :parameters (?p)
             :precondition (and (package ?p) (bomb ?b))           ; Need quant to put this in when
             :effect (when (in ?p ?b)
                       (defused ?b))))

(define (problem bt-dd)              ; graph-plan 1 steps, 2 actions
  (:domain bt)
  (:objects b p1 p2)
  (:init (package p1) (package p2) 
  	 (bomb b)
         (oneof (in p1 b) (in p2 b)))
  (:goal (defused b)))

(define (problem bt-dd2)             ; graph-plan 1 steps, 2 actions 
  (:domain bt)
  (:objects b p1 p2)
  (:init (package p1) (package p2) 
              (bomb b)
              (or (in p1 b) (in p2 b)))
  (:goal (defused b)))

(define (problem bt-dd3)              ; fails, can't defuse when bomb isn't there
  (:domain bt)
  (:objects b p1 p2)
  (:init (package p1) (package p2) 
         (bomb B)
         (uncertain (in p1 b) (in p2 b)))
  (:goal (defused b)))

(define (problem bt-ddd)              ; graph-plan 1 steps, 3 actions
  (:domain bt)
  (:objects b p1 p2 p3)
  (:init (package p1) (package p2) (package p3)
         (bomb b)
         (oneof (in p1 b) (in p2 b) (in p3 b)))
  (:goal (defused b)))

(define (domain bct)
  (:requirements :strips :equality :typing :conditional-effects :uncertainty)
  (:action dunk
             :parameters (?p)
             :precondition (and (package ?p) (bomb ?b) (not (clogged))) ; Need quant to put this in when
             :effect (and (clogged)
		           (when (in ?p ?b)
		             (defused ?b))))
  (:action flush
             :parameters ()
             :precondition (clogged)
             :effect (not (clogged))))

(define (problem bct-df1)              
  (:domain bct)
  (:objects b p1)
  (:init (package p1)
         (bomb B) (not (clogged))
         (in p1 b))
  (:goal (defused b)))

(define (problem bct-df2)              
  (:domain bct)
  (:objects b p1 p2)
  (:init (package p1) (package p2)
         (bomb B) (not (clogged))
         (oneof (in p1 b) (in p2 b)))
  (:goal (defused b)))

(define (problem bct-df3)              
  (:domain bct)
  (:objects b p1 p2 p3)
  (:init (package p1) (package p2) (package p3)
         (bomb B) (not (clogged))
         (oneof (in p1 b) (in p2 b) (in p3 b)))
  (:goal (defused b)))

(define (problem bct-df4)              
  (:domain bct)
  (:objects b p1 p2 p3 p4)
  (:init (package p1) (package p2) (package p3) (package p4)
         (bomb B) (not (clogged))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b)))
  (:goal (defused b)))

(define (problem bct-df5)              ; five dunks; four flushes
  (:domain bct)
  (:objects b p1 p2 p3 p4 p5)
  (:init (package p1) (package p2) (package p3) (package p4) (package p5)
         (bomb B) (not (clogged))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b) (in p5 b)))
  (:goal (defused b)))

(define (problem bct-df6)              ; five dunks; four flushes
  (:domain bct)
  (:objects b p1 p2 p3 p4 p5 p6)
  (:init (package p1) (package p2) (package p3) (package p4) (package p5) (package p6)
         (bomb B) (not (clogged))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b) (in p5 b) (in p6 b)))
  (:goal (defused b)))

(define (problem bct-df7)              ; five dunks; four flushes
  (:domain bct)
  (:objects b p1 p2 p3 p4 p5 p6 p7)
  (:init (package p1) (package p2) (package p3) (package p4) (package p5) (package p6) (package p7)
         (bomb B) (not (clogged))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b) (in p5 b) (in p6 b) (in p7 b)))
  (:goal (defused b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Like previous version except allows for multiple toilets
;;;

(define (domain bcts)
  (:requirements :strips :equality :typing :conditional-effects :uncertainty)
  (:action dunk
             :parameters (?p ?t)
             :precondition (and (toilet ?t) (package ?p) 
                                (bomb ?b) (not (clogged ?t))) ; Need quant to put this in when
             :effect (and (clogged ?t)
		           (when (in ?p ?b)
		             (defused ?b))))
  (:action flush
             :parameters (?t)
             :precondition (and (toilet ?t) (clogged ?t))
             :effect (not (clogged ?t))))

;;;;;;;;;;;;;;;;;;;;
;;; Single toilet

(define (problem bcts-t1p1)              
  (:domain bcts)
  (:objects b t1 p1)
  (:init (bomb B)
         (package p1)
         (toilet t1) (not (clogged t1))
         (in p1 b))
  (:goal (defused b)))

(define (problem bcts-t1p2)              
  (:domain bcts)
  (:objects b t1 p1 p2)
  (:init (bomb B)
         (package p1) (package p2)
         (toilet t1) (not (clogged t1))
         (oneof (in p1 b) (in p2 b)))
  (:goal (defused b)))

(define (problem bcts-t1p3)              
  (:domain bcts)
  (:objects b t1 p1 p2 p3)
  (:init (bomb B)
         (package p1) (package p2) (package p3)
         (toilet t1) (not (clogged t1))
         (oneof (in p1 b) (in p2 b) (in p3 b)))
  (:goal (defused b)))

(define (problem bcts-t1p4)              
  (:domain bcts)
  (:objects b t1 p1 p2 p3 p4)
  (:init (bomb B)
         (package p1) (package p2) (package p3) (package p4)
         (toilet t1) (not (clogged t1))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b)))
  (:goal (defused b)))

(define (problem bcts-t1p5)              ; five dunks; four flushes
  (:domain bcts)
  (:objects b t1 p1 p2 p3 p4 p5)
  (:init (bomb B)
         (package p1) (package p2) (package p3) (package p4) (package p5)
         (toilet t1) (not (clogged t1))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b) (in p5 b)))
  (:goal (defused b)))

(define (problem bcts-t1p6)              ; five dunks; four flushes
  (:domain bcts)
  (:objects b t1 p1 p2 p3 p4 p5 p6)
  (:init (bomb B)
         (package p1) (package p2) (package p3) (package p4) (package p5) (package p6)
         (toilet t1) (not (clogged t1))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b) (in p5 b) (in p6 b)))
  (:goal (defused b)))

(define (problem bcts-t1p7)              ; five dunks; four flushes
  (:domain bcts)
  (:objects b t1 p1 p2 p3 p4 p5 p6 p7)
  (:init (bomb B)
         (package p1) (package p2) (package p3) (package p4) (package p5) (package p6) (package p7)
         (toilet t1) (not (clogged t1))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b) (in p5 b) (in p6 b) (in p7 b)))
  (:goal (defused b)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Two toilets

(define (problem bcts-t2p2)              
  (:domain bcts)
  (:objects b t1 t2 p1 p2)
  (:init (package p1) (package p2) (bomb B)
         (toilet t1) (not (clogged t1)) (toilet t2) (not (clogged t2))
         (oneof (in p1 b) (in p2 b)))
  (:goal (defused b)))

(define (problem bcts-t2p3)              
  (:domain bcts)
  (:objects b t1 t2 p1 p2 p3)
  (:init (package p1) (package p2) (package p3) (bomb B)
         (toilet t1) (not (clogged t1)) (toilet t2) (not (clogged t2))
         (oneof (in p1 b) (in p2 b) (in p3 b)))
  (:goal (defused b)))

(define (problem bcts-t2p4)              
  (:domain bcts)
  (:objects b t1 t2 p1 p2 p3 p4)
  (:init (package p1) (package p2) (package p3) (package p4) (bomb B)
         (toilet t1) (not (clogged t1)) (toilet t2) (not (clogged t2))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b)))
  (:goal (defused b)))


(define (problem bcts-t2p5)              ; five dunks; four flushes
  (:domain bcts)
  (:objects b t1 t2 p1 p2 p3 p4 p5)
  (:init (package p1) (package p2) (package p3) (package p4) (package p5) (bomb B)
         (toilet t1) (not (clogged t1)) (toilet t2) (not (clogged t2))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b) (in p5 b)))
  (:goal (defused b)))

(define (problem bcts-t2p6)              ; five dunks; four flushes
  (:domain bcts)
  (:objects b t1 t2 p1 p2 p3 p4 p5 p6)
  (:init (bomb B)
         (package p1) (package p2) (package p3) (package p4) (package p5) (package p6)
         (toilet t1) (not (clogged t1)) (toilet t2) (not (clogged t2))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b) (in p5 b) (in p6 b)))
  (:goal (defused b)))

(define (problem bcts-t2p7)              ; five dunks; four flushes
  (:domain bcts)
  (:objects b t1 t2 p1 p2 p3 p4 p5 p6 p7)
  (:init (bomb B)
         (package p1) (package p2) (package p3) (package p4) (package p5) (package p6) (package p7)
         (toilet t1) (not (clogged t1)) (toilet t2) (not (clogged t2))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b) (in p5 b) (in p6 b) (in p7 b)))
  (:goal (defused b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Three toilets

(define (problem bcts-t3p2)              ; five dunks; four flushes
  (:domain bcts)
  (:objects b t1 t2 t3 p1 p2)
  (:init (bomb B)
         (package p1) (package p2)
         (toilet t1) (not (clogged t1)) (toilet t2) (not (clogged t2)) (toilet t3) (not (clogged t3))
         (oneof (in p1 b) (in p2 b)))
  (:goal (defused b)))

(define (problem bcts-t3p3)              ; five dunks; four flushes
  (:domain bcts)
  (:objects b t1 t2 t3 p1 p2 p3)
  (:init (bomb B)
         (package p1) (package p2) (package p3)
         (toilet t1) (not (clogged t1)) (toilet t2) (not (clogged t2)) (toilet t3) (not (clogged t3))
         (oneof (in p1 b) (in p2 b) (in p3 b)))
  (:goal (defused b)))

(define (problem bcts-t3p4)              ; five dunks; four flushes
  (:domain bcts)
  (:objects b t1 t2 t3 p1 p2 p3 p4)
  (:init (bomb B)
         (package p1) (package p2) (package p3) (package p4)
         (toilet t1) (not (clogged t1)) (toilet t2) (not (clogged t2)) (toilet t3) (not (clogged t3))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b)))
  (:goal (defused b)))

(define (problem bcts-t3p5)              ; five dunks; four flushes
  (:domain bcts)
  (:objects b t1 t2 t3 p1 p2 p3 p4 p5)
  (:init (bomb B)
         (package p1) (package p2) (package p3) (package p4) (package p5)
         (toilet t1) (not (clogged t1)) (toilet t2) (not (clogged t2)) (toilet t3) (not (clogged t3))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b) (in p5 b)))
  (:goal (defused b)))

(define (problem bcts-t3p6)              ; five dunks; four flushes
  (:domain bcts)
  (:objects b t1 t2 t3 p1 p2 p3 p4 p5 p6)
  (:init (bomb B)
         (package p1) (package p2) (package p3) (package p4) (package p5) (package p6)
         (toilet t1) (not (clogged t1)) (toilet t2) (not (clogged t2)) (toilet t3) (not (clogged t3))
         (oneof (in p1 b) (in p2 b) (in p3 b) (in p4 b) (in p5 b) (in p6 b)))
  (:goal (defused b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (domain bt-asym)
  (:requirements :strips :equality  :typing :conditional-effects :uncertainty)
  (:action dunka
             :parameters ()
             :precondition (and (enabled) (not (clogged))) 
             :effect (when (bomb-in-a) 
                       (defused)))
  (:action dunkb
             :parameters ()
             :precondition (enabled)
             :effect (and (when (bomb-in-b)
		             (defused))
		           (when (and (not (bomb-in-b)) (not (pumping)))
		             (clogged))))
  (:action enable
             :parameters ()
             :precondition ()
             :effect (enabled))
  (:action pump
             :parameters ()
             :precondition ()
             :effect (pumping)))

(define (problem bta-c) 
  (:domain bt-asym)
  (:init (not (clogged)) (not (enabled)) (not (pumping))
         (oneof (bomb-in-a) (bomb-in-b)))
  (:goal (defused)))
