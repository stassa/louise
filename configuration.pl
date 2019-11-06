:-module(configuration, [dynamic_generations/1
			,experiment_file/2
			,learner/1
			,max_invented/1
			,metarule/2
			,metarule/3
			,metarule/4
			,metarule/5
			,metarule/6
			,metarule/7
			,metarule/8
			,metarule/9
			,metarule/10
			,metarule_constraints/2
			,recursion_depth_limit/2
			,recursive_reduction/1
			,reduction/1
			,resolutions/1
			,theorem_prover/1
			]).

% Must be loaded before experiment file to allow experiment files to
% use set_configuration_option/2 without errors.
:-use_module(src(auxiliaries), [set_configuration_option/2]).
:-user:use_module(src(experiment_file)).
:-reexport(lib(program_reduction/reduction_configuration),
	   except([resolutions/1])).
:-reexport(lib(evaluation/evaluation_configuration)).
:-reexport(lib/sampling/sampling_configuration).


% Dynamic configuration options can be manipulated
% by means of set_configuration_option/2 in module auxiliaries.
:- dynamic dynamic_generations/1
          ,max_invented/1
          ,recursion_depth_limit/2
	  ,recursive_reduction/1
	  ,reduction/1
	  ,resolutions/1
	  ,theorem_prover/1.

% Body literals of H(2,2) metarules.
:-dynamic m/1
         ,m/2
         ,m/3
	 ,m/4
	 ,m/5
	 ,m/6
	 ,m/7
	 ,m/8
	 ,m/9
	 ,m/10.

:-multifile m/1
           ,m/2
	   ,m/3
	   ,m/4
	   ,m/5
	   ,m/6
	   ,m/7
	   ,m/8
	   ,m/9
	   ,m/10.

% Allows experiment files to define their own, special metarules.
% BUG: Actually, this doesn't work- module quantifiers, again.
% Needs fixing.
:-multifile metarule/2
           ,metarule/3
           ,metarule/4
	   ,metarule/5
	   ,metarule/6
	   ,metarule/7
	   ,metarule/8
	   ,metarule/9
	   ,metarule/10.

% Experiment files may or may not define metarule constraints to filter
% the Top program for unwanted clause structures (e.g. I don't like
% left-recursive clauses because they mess up evaluation).
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

/* Debug levels */
%:-debug(learn). % Debug learning steps.
%:-debug(top). % Debug Top program construction.
%:-debug(reduction). % Debug Top program construction.
%:-debug(dynamic). % Debug dynamic learning.
%:-debug(examples_invention). % Debug examples invention.


%!	dynamic_generations(?Generations) is det.
%
%	Number of metarule Generations in dynamic learning.
%
dynamic_generations(1).


%!	experiment_file(?Path,?Module) is semidet.
%
%	The Path and Module name of an experiment file.
%
experiment_file('data/examples/tiny_kinship.pl',tiny_kinship).
%experiment_file('data/examples/anbn.pl',anbn).
%experiment_file('data/examples/abduced.pl',abduced).
%experiment_file('data/examples/special_metarules.pl',special_metarules).
%experiment_file('data/examples/constraints.pl',constraints).
%experiment_file('data/examples/mtg_fragment.pl',mtg_fragment).
%experiment_file('data/examples/recipes.pl',recipes).
%experiment_file('data/examples/examples_invention.pl',path).
%experiment_file('data/thelma_louise/kinship/kinship.pl',kinship).
%experiment_file('data/thelma_louise/kinship/kinship_accuracy.pl',kinship_accuracy).
%experiment_file('data/thelma_louise/robots/robots.pl',robots).
%experiment_file('data/thelma_louise/robots/generator/robots_gen.pl',robots_gen).
%experiment_file('data/thelma_louise/noise/heroes/detect_evil.pl',detect_evil).


%!	max_invented(?Number) is semidet.
%
%	Maximum number of invented predicates in dynamic learning.
%
max_invented(1).


%!	learner(?Name) is semidet.
%
%	Name of the learning system this configuration is for.
%
%	Name is one of [louise,thelma].
%
%	Used to switch context between Louise and Thelma, where this is
%	needed. The typical use case is when experiment code must check
%	the values of configuration options that are particular to one
%	or the other system (e.g. resolutions/1 is not present in
%	Thelma etc).
%
learner(louise).


%!	metarule(?Id,?P,?Q) is semidet.
%!	metarule(?Id,?P,?Q,?R) is semidet.
%
%	An encapsulated metarule.
%
%	@tbd This representation does not define constraints. For the
%	time being this doesn't seem to be necessary but a complete
%	representation will need to include constraints.
%
metarule(abduce,P,X,Y):- m(P,X,Y).
metarule(unit,P):- m(P,_X,_Y).
metarule(projection,P,Q):- m(P,X,X), m(Q,X).
metarule(identity,P,Q):- m(P,X,Y), m(Q,X,Y).
metarule(inverse,P,Q):- m(P,X,Y), m(Q,Y,X).
metarule(chain,P,Q,R):- m(P,X,Y), m(Q,X,Z), m(R,Z,Y).
metarule(tailrec,P,Q,P):- m(P,X,Y), m(Q,X,Z), m(P,Z,Y).
metarule(precon,P,Q,R):- m(P,X,Y), m(Q,X), m(R,X,Y).
metarule(postcon,P,Q,R):- m(P,X,Y), m(Q,X,Y), m(R,Y).
metarule(switch,P,Q,R):- m(P,X,Y), m(Q,X,Z), m(R,Y,Z).
% H22 metarules redundnant given chain and inverse.
% To avoid proliferation of vaguely descriptive names these are named
% by their firts-order, universally quantified variables.
% identity and switch are also in this set (but they are already named)
metarule(xy_xy_xy,P,Q,R):- m(P,X,Y), m(Q,X,Y), m(R,X,Y).
metarule(xy_xy_yx,P,Q,R):- m(P,X,Y), m(Q,X,Y), m(R,Y,X).
%metarule(xy_xz_yz,P,Q,R):- m(P,X,Y), m(Q,X,Z), m(R,Y,Z). % switch
metarule(xy_yx_xy,P,Q,R):- m(P,X,Y), m(Q,Y,X), m(R,X,Y).
metarule(xy_yx_yx,P,Q,R):- m(P,X,Y), m(Q,Y,X), m(R,Y,X).
metarule(xy_yz_xz,P,Q,R):- m(P,X,Y), m(Q,Y,Z), m(R,X,Z).
metarule(xy_yx_zx,P,Q,R):- m(P,X,Y), m(Q,Y,Z), m(R,Z,X).
metarule(xy_zx_yz,P,Q,R):- m(P,X,Y), m(Q,Z,X), m(R,Y,Z).
metarule(xy_zx_zy,P,Q,R):- m(P,X,Y), m(Q,Z,X), m(R,Z,Y).
metarule(xy_zy_xz,P,Q,R):- m(P,X,Y), m(Q,Z,Y), m(R,X,Z).
metarule(xy_zy_zx,P,Q,R):- m(P,X,Y), m(Q,Z,Y), m(R,Z,X).

% Used in noise/heroes/detect_evil.pl
% Added here for Thelma compatibility
metarule(double_identity,P,Q,R,Y,Z,D):-m(P,X,Y),m(Q,X,Z),m(R,X,D).


%!	metarule_constraints(+Metasubstitution,+Goal) is nondet.
%
%	A Goal to be called when Metasubstitution is matched.
%
%	@tbd This predicate is multifile so that it can be declared by
%	experiment files, however the definition below is pretty
%	universally necessary to allow learning hypotheses with
%	left-recursions using dynamic learning and predicate invention.
%	So it goes into the configuration that it might be used by
%	aplicable to every experiment file. On the other
%	hand, left-recursive hypotheses may be required for some
%	problems so this definition is left commented out. This will not
%	raise any errors because metarule_constraints/2 is declared
%	dynamic.
%
/*
metarule_constraints(M,fail):-
	M =.. [m,Id,P|Ps]
	% Projection explicitly maps p/2 to p/1.
	,Id \= projection
	,forall(member(P1,Ps)
	       ,P1 == P).
*/

/*
metarule_constraints(M,fail):-
	M =.. [m,Id,P|Ps]
	,Id \= projection
	,left_recursive(P,Ps).

left_recursive(T,[T|_Ps]):-
	!.
left_recursive(T,[T,T|_Ps]):-
	!.
left_recursive(T,[I,T|_Ps]):-
	atomic_list_concat([T,A],'_',I)
	,atom_number(A,_N).
*/


%!	recursion_depth_limit(?Purpose,?Limit) is semidet.
%
%	Recursion depth Limit for the given Purpose.
%
%	Limit is an integer, which is passed as
%	the second argument to call_with_depth_limit/3 in order to
%	limit recursion in the listed Purpose.
%
%	Known purposes are as follows:
%
%	* dyamic_learning: Limits recursion during Top program
%	construction in dynamic learning.
%
recursion_depth_limit(dynamic_learning,500).


%!	recursive_reduction(?Bool) is semidet.
%
%	Whether to reduce the Top program recursively or not.
%
%	Setting Bool to true enables recursie reduction of the Top
%	program. Recursive reduction means that the result of each
%	reduction step is given as input to the reduction algorithm in
%	the next step (also known as "doing the feedbacksies").
%
%	Recursive reduction can result in a stronger reduction in less
%	time, with a lower setting for resolutions/1 (in fact, the same
%	amount of reduction can take less time exactly because the
%	resolutions/1 setting can be set to a lower value).
%
%	Recursive reduction is more useful when the Top program is large
%	and many resolution steps are required to remove all redundancy
%	from it.
%
recursive_reduction(false).


%!	reduction(?Method) is semidet.
%
%	Select a Method for Top program reduction.
%
%	One of:
%	* plotkins: discard logically redundant clauses by application
%	of Plotkin's program reduction.
%	* subhypothesis: select one hypothesis entailed by the Top
%	program.
%
reduction(plotkins).
%reduction(subhypothesis).


%!	resolutions(?Resolutions) is semidet.
%
%	Maximum number of resolutions.
%
%	Used with solve_to_depth/3.
%
%resolutions(500_000_000_000).
%resolutions(20_500_000).
%resolutions(10_500_000).
%resolutions(5_500_000).
%resolutions(500_000).
%resolutions(250_000).
%resolutions(30_000).
%resolutions(10_000).
resolutions(5000).
%resolutions(100).
%resolutions(15).
%resolutions(0).


%!	theorem_prover(?Algorithm) is semidet.
%
%	Theorem proving Algorithm to use in Top program construction.
%
%	Algorithm is one of: [resolution, tp].
%
%	With option resolution, the Top program is constructed in a
%	top-down manner, using SLD resolution.
%
%	With option tp, the Top program is constructed in a bottom-up
%	manner, using a TP operator.
%
%	Option resolution is faster because it hands off to the Prolog
%	interpreter. On the other hand, it can get lost in recursion,
%	especially when a problem has left-recursions (although this
%	doesn't quite seem to happen in practice).
%
%	Option tp is slower because it's implemented in Prolog and it's
%	not terribly optimised either. The trade-off is that it's
%	guaranteed to terminate and runs in polynomial time, at least
%	for definite programs (but then, there are no guarantees outside
%	of definite programs).
%
%	More impotantly, option tp can be used to enable predicate
%	invention, although this is not yet implemented.
%
%	Note also that the TP operator only works for datalog definite
%	programs.
%
theorem_prover(resolution).
%theorem_prover(tp).


% Loads the current experiment file in the Swi-Prolog IDE when the
% configuration is changed.
%
% It is perfectly safe to remove this directive.
%
%:-experiment_file(P,_)
%  ,edit(P).


% This line ensures the experiment file set in the configuration option
% experiment_file/2 is always updated when the configuration module is
% changed and reloaded. Don't remove it.
%
% DO NOT REMOVE THIS LINE!
:-experiment_file:reload.
