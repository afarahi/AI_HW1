;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The original Movie-Watching domain, as used is
;; Anderson, Weld, & Smith:  _Conditional Effects
;; in Graphplan_.  AIPS-98.
;;
(define (domain movie-dom) (:requirements :adl :typing)
  (:types chips dip pop cheese crackers - object)
  (:predicates (movie-rewound)
               (counter-at-two-hours)
               (counter-at-zero)
               (have-chips)
               (have-dip)
               (have-pop)
               (have-cheese)
               (have-crackers))
  

  (:action rewind-movie
           :parameters ()
           :effect (and (movie-rewound)
                        ;; Let's assume that the movie is 2 hours long
                        (when (not (counter-at-two-hours)) 
                          (not (counter-at-zero)))))
  
  (:action reset-counter
           :parameters ()
           :effect (counter-at-zero))


  ;;; Get the food and snacks for the movie
  (:action get-chips
           :parameters (?x - chips)
           :effect (have-chips))
  
  (:action get-dip
           :parameters (?x - dip)
           :effect (have-dip))

  (:action get-pop
           :parameters (?x - pop)
           :effect (have-pop))
  
  (:action get-cheese
           :parameters (?x - cheese)
           :effect (have-cheese))
  
  (:action get-crackers
           :parameters (?x - crackers)
           :effect (have-crackers)))


(define (problem movie-5) (:domain movie-dom)
  (:objects 
   c1 c2 c3 c4 c5 - chips
   d1 d2 d3 d4 d5 - dip
   p1 p2 p3 p4 p5 - pop
   z1 z2 z3 z4 z5 - cheese
   k1 k2 k3 k4 k5 - crackers)
  (:init (not (movie-rewound)) 
         ;; Counter is at something like 1 hour
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-6) (:domain movie-dom)
  (:situation movie-5)
  (:objects c6 - chips  d6 - dip  p6 - pop  z6 - cheese  k6 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-7) (:domain movie-dom)
  (:situation movie-6)
  (:objects c7 - chips  d7 - dip  p7 - pop  z7 - cheese  k7 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-8) (:domain movie-dom)
  (:situation movie-7)
  (:objects c8 - chips  d8 - dip  p8 - pop  z8 - cheese  k8 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-9) (:domain movie-dom)
  (:situation movie-8)
  (:objects c9 - chips  d9 - dip  p9 - pop  z9 - cheese  k9 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-10) (:domain movie-dom)
  (:situation movie-9)
  (:objects c10 - chips  d10 - dip  p10 - pop  z10 - cheese  k10 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-11) (:domain movie-dom)
  (:situation movie-10)
  (:objects c11 - chips  d11 - dip  p11 - pop  z11 - cheese  k11 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-12) (:domain movie-dom)
  (:situation movie-11)
  (:objects c12 - chips  d12 - dip  p12 - pop  z12 - cheese  k12 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-13) (:domain movie-dom)
  (:situation movie-12)
  (:objects c13 - chips  d13 - dip  p13 - pop  z13 - cheese  k13 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-14) (:domain movie-dom)
  (:situation movie-13)
  (:objects c14 - chips  d14 - dip  p14 - pop  z14 - cheese  k14 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-15) (:domain movie-dom)
  (:situation movie-14)
  (:objects c15 - chips  d15 - dip  p15 - pop  z15 - cheese  k15 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-16) (:domain movie-dom)
  (:situation movie-15)
  (:objects c16 - chips  d16 - dip  p16 - pop  z16 - cheese  k16 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-17) (:domain movie-dom)
  (:situation movie-16)
  (:objects c17 - chips  d17 - dip  p17 - pop  z17 - cheese  k17 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-18) (:domain movie-dom)
  (:situation movie-17)
  (:objects c18 - chips  d18 - dip  p18 - pop  z18 - cheese  k18 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-19) (:domain movie-dom)
  (:situation movie-18)
  (:objects c19 - chips  d19 - dip  p19 - pop  z19 - cheese  k19 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))


(define (problem movie-20) (:domain movie-dom)
  (:situation movie-19)
  (:objects c20 - chips  d20 - dip  p20 - pop  z20 - cheese  k20 - crackers)
  (:goal (and (movie-rewound) (counter-at-zero)
              (have-chips) (have-dip) (have-pop) 
              (have-cheese) (have-crackers))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A modified Movie-Watching domain that better 
;; demonstrates the advantage of induced mutexes.
;; Corin Anderson, August 1998.
;;
(define (domain movie-dom-2) (:requirements :adl :typing)
  (:types chips dip pop cheese crackers - object)
  (:predicates (movie-rewound)
               (counter-at-two-hours)
               (counter-at-zero)
               (have-chips)
               (have-dip)
               (have-pop)
               (have-cheese)
               (have-crackers))
  

  (:action rewind-movie
           :parameters ()
           :effect (and (movie-rewound)
                        ;; Let's assume that the movie is 2 hours long
                        (when (not (counter-at-two-hours)) 
                          (not (counter-at-zero)))))
  
  (:action play-movie
	   :parameters()
	   :precondition (and (movie-rewound) (counter-at-zero)
			      (have-chips) (have-dip) (have-pop)
			      (have-cheese) (have-crackers))
	   :effect (and (counter-at-two-hours) (movie-watched)
			(not (movie-rewound)) (not (counter-at-zero))))

  (:action reset-counter
           :parameters ()
           :effect (counter-at-zero))


  ;;; Get the food and snacks for the movie
  (:action get-chips
           :parameters (?x - chips)
           :effect (have-chips))
  
  (:action get-dip
           :parameters (?x - dip)
           :effect (have-dip))

  (:action get-pop
           :parameters (?x - pop)
           :effect (have-pop))
  
  (:action get-cheese
           :parameters (?x - cheese)
           :effect (have-cheese))
  
  (:action get-crackers
           :parameters (?x - crackers)
           :effect (have-crackers)))


(define (problem movie-2-5) (:domain movie-dom-2)
  (:objects 
   c1 c2 c3 c4 c5 - chips
   d1 d2 d3 d4 d5 - dip
   p1 p2 p3 p4 p5 - pop
   z1 z2 z3 z4 z5 - cheese
   k1 k2 k3 k4 k5 - crackers)
  (:init (not (movie-rewound)) 
         ;; Counter is at something like 1 hour
         (not (counter-at-two-hours)) (not (counter-at-zero)))
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-6) (:domain movie-dom-2)
  (:situation movie-2-5)
  (:objects c6 - chips  d6 - dip  p6 - pop  z6 - cheese  k6 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-7) (:domain movie-dom-2)
  (:situation movie-2-6)
  (:objects c7 - chips  d7 - dip  p7 - pop  z7 - cheese  k7 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-8) (:domain movie-dom-2)
  (:situation movie-2-7)
  (:objects c8 - chips  d8 - dip  p8 - pop  z8 - cheese  k8 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-9) (:domain movie-dom-2)
  (:situation movie-2-8)
  (:objects c9 - chips  d9 - dip  p9 - pop  z9 - cheese  k9 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-10) (:domain movie-dom-2)
  (:situation movie-2-9)
  (:objects c10 - chips  d10 - dip  p10 - pop  z10 - cheese  k10 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-11) (:domain movie-dom-2)
  (:situation movie-2-10)
  (:objects c11 - chips  d11 - dip  p11 - pop  z11 - cheese  k11 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-12) (:domain movie-dom-2)
  (:situation movie-2-11)
  (:objects c12 - chips  d12 - dip  p12 - pop  z12 - cheese  k12 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-13) (:domain movie-dom-2)
  (:situation movie-2-12)
  (:objects c13 - chips  d13 - dip  p13 - pop  z13 - cheese  k13 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-14) (:domain movie-dom-2)
  (:situation movie-2-13)
  (:objects c14 - chips  d14 - dip  p14 - pop  z14 - cheese  k14 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-15) (:domain movie-dom-2)
  (:situation movie-2-14)
  (:objects c15 - chips  d15 - dip  p15 - pop  z15 - cheese  k15 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-16) (:domain movie-dom-2)
  (:situation movie-2-15)
  (:objects c16 - chips  d16 - dip  p16 - pop  z16 - cheese  k16 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-17) (:domain movie-dom-2)
  (:situation movie-2-16)
  (:objects c17 - chips  d17 - dip  p17 - pop  z17 - cheese  k17 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-18) (:domain movie-dom-2)
  (:situation movie-2-17)
  (:objects c18 - chips  d18 - dip  p18 - pop  z18 - cheese  k18 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-19) (:domain movie-dom-2)
  (:situation movie-2-18)
  (:objects c19 - chips  d19 - dip  p19 - pop  z19 - cheese  k19 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


(define (problem movie-2-20) (:domain movie-dom-2)
  (:situation movie-2-19)
  (:objects c20 - chips  d20 - dip  p20 - pop  z20 - cheese  k20 - crackers)
  (:goal (and (movie-rewound) (movie-watched))))


