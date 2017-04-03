This is Sensory Graphplan (SGP),  release 1.0h.

   Original Lisp Graphplan implementation by Mark Peot
   <peot@rpal.rockwell.com>.  Enhancements by Dave Smith
   <de2smith@ptolemy.arc.nasa.gov>.  Support for Factored Expansion,
   PDDL domains, and other optimizations by Dave Smith, Dan Weld
   <weld@cs.washington.edu>, and Corin Anderson
   <corin@cs.washington.edu>.

   Copyright (c) Mark Peot, 1995; University of Washington, 1997,
   1998, 1999, 2000.

   Please send mail to bug-sgp@cs.washington.edu if you download this
   code, find bugs in it, or just wish to receive occasional news
   about possible new versions. 


**************** Contents of this file
1. Description of version 1.0h
2. Files and loading instructions
3. Starting SGP
4. Generating debugging and statistical information
5. Known bugs and workarounds
6. Revision history

**************** Description 

This version (1.0) is the first release of Sensory Graphplan.  In this
release, SGP supports:
  * Conditional effects using Factored Expansion.
  * Sensory actions and uncertainty.
  * Some simple heuristic optimizations (subgoal ordering, action
    ordering, ground operator instance precomputation).

This minor version (1.0h) is an update to version 1.0.  See the 
revision history section below for changes.

**************** Files and loading instructions

This distribution contains the following files:

./README                  : this file.
./*.lisp                  : code for SGP planner proper.
./domains/*.pddl          : PDDL domain files as distributed with PDDL
                            1.6.
./domains/bt.lisp         : Bomb-in-the-toilet domain.
./domains/casandra.lisp   : Pryor & Collins' transportation domain,
                            including uncertainty.
./domains/sick.lisp       : Medical domain used in Weld, Anderson, &
                            Smith's "Extending Graphplan to Handle
                            Uncertaintly & Sensing Actions"
./domains/movie2.pddl     : An updated Movie-Watching domain.



**************** Starting and using SGP

Begin by loading the loader file, loader.lisp.  Execute the form
(load-gp) to load the SGP files.  Alternatively, execute (compile-gp) to
compile the LISP code first and then load SGP.

There are presently two entry points to the planner.  Both entry points
are named PLAN; they exist in separate packages, the GP and SGP
packages.  The GP::PLAN form is the planner that supports conditional
effects using factored expansion.  SGP::PLAN is the planner that handles
sensory actions and uncertainty as described in the AAAI-98 paper
"Extending Graphplan to Handle Uncertainty & Sensing Actions" by Weld,
Anderson, & Smith.

To load a PDDL file and find a plan, execute the following:

(in-package :gp)		; Switch to the GP package
(load-domains "gripper.pddl")	; Load the Gripper domain file
(plan 'gripper2)		; Run the planner

The result of the PLAN form will be a list of parallel plan steps.  It
might be the case that the plan steps are preceeded by DOMAINS::.  To
print the result without the DOMAINS:: prefix, use the form:

(format t "~A~%" (plan 'gripper2))


To run SGP, the forms are slightly different:

(in-package :sgp)		; Switch to SGP package
(load "domains/bt.lisp")	; Load the Bomb-in-the-toilet domain
(trace-gp contexts)		; Enable printing of possible worlds
(plan 'bt-2sa)			; Run the planner

The result of this PLAN form is a contingent plan.  A plan step that is
contingent upon which possible world it should be executed is augmented
with a list of indices.  These indices are the possible world numbers in
which the agent should _not_ execute the action.  The planner guarantees
that sensing actions will have been taken to suitably identify that the
agent is or is not in one of these possible worlds.


**************** Generating debugging and statistical information

SGP makes it easy to generate debugging and statistical information.
Debugging output is enabled with the TRACE-GP macro and disabled with
the UNTRACE-GP macro.  TRACE-GP and UNTRACE-GP each require a single 
argument from the following table:

top-level	Displays each phase of SGP as it expands and searches 
		the planning graph.
contexts	Displays the set of contexts found in the initial
		state (SGP::PLAN only).
actions		Prints each actions and no-op as it is added to the
		planning graph.  Useful in conjunction with top-level.
print-detailed-actions	Prints the preconditions and effects of each
		action (SGP::PLAN only).
mutex		Displays which phase of mutex computation is currently
		being done by SGP.  [Also prints when each mutex is added
		to the planning graph (SGP::PLAN only).]
bc		Prints detailed information about which propositions are
		used at each planning graph level.
csp		Prints a message each time a CSP variable is bound to a 
		value (SGP::PLAN only).

To produce statistics, use the STAT-GP and UNSTAT-GP macros.  These
macros also each require a single argument:

time-expansion	Prints the time required to expand the graph at each 
		level.
time-bc		Prints the time required to perform the backward 
		chaining search when needed after each graph expansion.
time-mutex	Prints the time required to compute the set of mutexes
		for each planning graph level (GP::PLAN only).
graph-size	Prints the number of actions and propositions at each
		planning graph level.
mutex-count	Prints the number of action and proposition mutexes
		at each planning graph level (GP::PLAN only).


**************** Known Bugs and Workarounds


PDDL defines a fully recursive goal description form.  Currently, SGP
does not handle the full recursiion.  In particular, negation applies
only to atomic effects and to equality (that is, (not (P)) is
legitimate, but (not (exists (?x - square) (P ?x))) will not be
processed correctly).

The solution to this problem is to simply not have negated compound
forms in the input domains.
-----------------------
A few minor changes need to be made to make SGP runnable with Harlequin
LispWorks.  First, because the GP package is already defined in
Harlequin (as the nickname for the graphics-port package), SGP's GP
package must be renamed GRAPHPLAN.  Change the defpackage line in
loader.lisp and the in-package line in gp.lisp accordingly.  Second,
the filenames in the loader.lisp file must have the .lisp ending added
to them.
-----------------------
Sometimes inequality preconditions appear to not hold in SGP.  This 
anamoly is because the variables in the inequalities are not yet 
bound.  To fix this problem, either enable type the variables (by 
adding :typing or :adl to the domain :requirements), or make sure that 
the variables are bound before the inequality is evaluated.

**************** Revision History
$Id: README,v 1.14 2000/01/15 00:18:48 corin Exp $

1.0h	Added :extends capability to domain definitions.  Fixed minor
	bug with empty :init section in domain definitions.  Added
	:all-plans support to SGP::PLAN.  [1.14.00]
1.0g	Fixed minor bugs related to printing timing statistics.  Added
	section in README describing how to generate debugging and
	statistical information.  [5.26.99]
1.0f	Fixed bug where SGP would ignore = and (not =) preconditions.
	Fixed compilation error caused by incorrect struct slot type.
	Removed unnecessary quotes in loader.lisp, gp.lisp.  Fixed
	loading errors in mystery.pddl, fridge.pddl.  Added 
	bomb-world.lisp domain. [5.19.99]
1.0e	Removed :nicknames from loader.lisp.  Fixed bug with SGP's 
	graph size statistic reporting.  Added Harlequin LispWorks 
	workaround to README. Fixed instructions for loading SGP domain
	in README.  [4.14.99]
1.0d	Added updated movie watching domain. [1.4.99]
1.0c	Bug found in and removed from code that precomputes ground
	instantiations of operators. [10.5.98]
1.0b	Bug in SGP when producing all possible plans removed.  Bug
	affected only SGP::PLAN (not GP::PLAN). [9.5.98]
1.0a	Bug in handling quantification removed.  Support for :situation
	keyword is PDDL files added. [8.25.98]
1.0	Original release. [7.20.98]

**************** End of README
 
