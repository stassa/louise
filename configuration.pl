:-module(configuration, [experiment_file/2
                        ,example_clauses/1
			,learner/1
                        ,learning_predicate/1
			,max_invented/1
                        ,minimal_program_size/2
			,metarule/2
			,metarule_constraints/2
			,recursion_depth_limit/2
			,recursive_reduction/1
			,reduction/1
			,resolutions/1
			,symbol_range/2
			,theorem_prover/1
			,op(100,xfx,metarule)
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
:- dynamic max_invented/1
          ,minimal_program_size/2
          ,recursion_depth_limit/2
	  ,recursive_reduction/1
	  ,reduction/1
	  ,resolutions/1
	  ,theorem_prover/1.

% Allows experiment files to define their own, special metarules.
% BUG: Actually, this doesn't work- module quantifiers, again.
% Needs fixing.
:-multifile metarule/2.

/* Debug levels
 * Note that some of the debug topics below emit identical messages.
 * In particular, 'learn' debugs learn/5 that calls top program
 * construction and reduction that are also debugged by 'top_program'
 * and 'reduction'.
*/
:-nodebug(_). % Clear all debug topics.
%:-debug(learn). % Debug learning steps.
%:-debug(metasubstitution). % Debug metasubstitutions.
%:-debug(top_program). % Debug Top program construction.
%:-debug(reduction). % Debug Top program reduction.
%:-debug(dynamic). % Debug dynamic learning.
%:-debug(predicate_invention). % Debug predicate invention.
%:-debug(examples_invention). % Debug examples invention.
%:-debug(evaluation).


%!      example_clauses(?What) is semidet.
%
%       What to do with example clauses.
%
%       This option determines how Louise treats examples that are given
%       as definite clauses with one or more body literals (rather than
%       ground atoms), i.e. "example clauses".
%
%       What is one of [bind,call].
%
%       If What is "bind", example clauses are bound to each instance of
%       a metarule where that is possible.
%
%       If What is "call", the head literal of each example clause is
%       bound to the enapsulated head literal of a metarule, then the
%       body literals of the example clause are called. This may
%       result in the universally quantified variables in the head of
%       the clause, and so the encapsulated head literal of the
%       metarule, to be bound.
%
%       Use "bind" when you have a set of definite clauses that you want
%       to transform to instances of a metarule.
%
%       Use "call" when you want to use a set of definite clauses with
%       bodies to generate examples.
%
%example_clauses(bind).
example_clauses(call).


%!	experiment_file(?Path,?Module) is semidet.
%
%	The Path and Module name of an experiment file.
%
experiment_file('data/examples/tiny_kinship.pl',tiny_kinship).
%experiment_file('data/examples/anbn.pl',anbn).
%experiment_file('data/examples/abduced.pl',abduced).
%experiment_file('data/examples/user_metarules.pl',user_metarules).
%experiment_file('data/examples/constraints.pl',constraints).
%experiment_file('data/examples/mtg_fragment.pl',mtg_fragment).
%experiment_file('data/examples/recipes.pl',recipes).
%experiment_file('data/examples/example_invention.pl',path).
%experiment_file('data/examples/kin.pl',kin).
%experiment_file('data/robots/robots.pl',robots).
%experiment_file('data/coloured_graph/coloured_graph.pl',coloured_graph).
%experiment_file('data/examples/multi_pred.pl',multi_pred).
%experiment_file('data/examples/incremental_refinmnt.pl',incremental_refinmnt).


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


%!	learning_predicate(+Learning_Predicate) is semidet.
%
%	The Learning_Predicate to be used in list_learning_results/0.
%
%	Learning_Predicate is a predicate indicator, the symbol and
%	arity of one of the following learning predicates defined in
%       Louise:
%       * learn/1
%       * learn_dynamic/1
%       * learn_with_examples_invention/2
%       * learn_minimal/1
%
%       The specified predicate will be used to list the learning
%       results for all learning targets defined in an experiment file
%       with a call to list_learning_results/0.
%
%	learning_predicate/1 is declared as multifile. To specify the
%	learning predicate to be used with list_learning_results/0, add
%	a clause of learning_predicate/1 to the relevant experiment
%	file.
%
%	For example, the following clause:
%	==
%	configuration:learning_predicate(learn_dynamic/1).
%	==
%
%	Will cause list_learning_results/0 to use learn_dynamic/1 for
%	all predicates in the experiment file containing that clause.
%
%	learning_predicate/1 is declared dynamic. You do not have to
%	specify a learning predicate for every experiment file.
%	list_learning_results/0 will default to learn/1.
%
%	Note that learning_predicate/1 will not affect learning by
%	calling learning predicates directly. That is, having added a
%	clause of learning_predicate/1 like the one above to an
%	experiment file you are free to then call learn/1 or any other
%	learning predicate on any of the learning targets in that
%	experiment file. Only the learning predicate used by
%	list_learning_results/0 is affected by this option.
%
%	Finally, note that specifying any other predicate than the three
%	learning predicates listed above as a learning_predicate will
%	cause list_learning_results/0 to raise an error.
%
%	@see list_learning_results/0
%
%       @tbd learning_predicate/1 is also used in lib/evaluation to
%       choose the learning predicate used to evaluate a learning
%       result. Predicates in that library default to learn/1 when
%       learning_predicate/1 is not defined.
%
:-dynamic learning_predicate/1.
:-multifile learning_predicate/1.


%!	max_invented(?Number) is semidet.
%
%	Maximum number of invented predicates in dynamic learning.
%
max_invented(1).


%!      minimal_program_size(?Minimum,?Maximum) is semidet.
%
%       Minimum and Maximum cardinality of a minimal program.
%
%       Each of Minimum, Maximum should be an integer between 1 and
%       positive infinity ('inf' in Prolog).
%
minimal_program_size(2,inf).


%!	metarule(?Id,?P,?Q) is semidet.
%!	metarule(?Id,?P,?Q,?R) is semidet.
%
%	An encapsulated metarule.
%
%	@tbd This representation does not define constraints. For the
%	time being this doesn't seem to be necessary but a complete
%	representation will need to include constraints.
%
abduce metarule 'P(X,Y)'.
unit metarule 'P(x,y)'.
projection_21 metarule 'P(x,x):- Q(x)'.
projection_12 metarule 'P(x):- Q(x,x)'.
identity metarule 'P(x,y):- Q(x,y)'.
inverse metarule 'P(x,y):- Q(y,x)'.
chain metarule 'P(x,y):- Q(x,z), R(z,y)'.
tailrec metarule 'P(x,y):- Q(x,z), P(z,y)'.
precon metarule 'P(x,y):- Q(x), R(x,y)'.
postcon metarule 'P(x,y):- Q(x,y), R(y)'.
switch metarule 'P(x,y):- Q(x,z), R(y,z)'.
swap metarule 'P(x,y):- Q(z,x), R(z,y)'.
% Metarules with abductible first-order existentially quantified
% variables. Also see abduce metarule above.
chain_abduce_x metarule 'P(X,y):- Q(X,z), R(z,y)'.
chain_abduce_y metarule 'P(x,Y):- Q(x,z), R(z,Y)'.
chain_abduce_z metarule 'P(x,y):- Q(x,Z), R(Z,y)'.
projection_21_abduce metarule 'P(X,X):- Q(X)'.
projection_12_abduce metarule 'P(X):- Q(X,X)'.
precon_abduce metarule 'P(X,y):- Q(X), R(X,y)'.
postcon_abduce metarule 'P(x,Y):- Q(x,Y), R(Y)'.

/*
% H22 metarules redundnant given chain and inverse.
% To avoid proliferation of vaguely descriptive names these are named
% by their firts-order, universally quantified variables.
% identity and switch are also in this set (but they are already named)
% TODO: convert to new format.
metarule(xy_xy_xy,P,Q,R):- m(P,X,Y), m(Q,X,Y), m(R,X,Y).
metarule(xy_xy_yx,P,Q,R):- m(P,X,Y), m(Q,X,Y), m(R,Y,X).
%metarule(xy_xz_yz,P,Q,R):- m(P,X,Y), m(Q,X,Z), m(R,Y,Z). % switch
metarule(xy_yx_xy,P,Q,R):- m(P,X,Y), m(Q,Y,X), m(R,X,Y).
metarule(xy_yx_yx,P,Q,R):- m(P,X,Y), m(Q,Y,X), m(R,Y,X).
metarule(xy_yz_xz,P,Q,R):- m(P,X,Y), m(Q,Y,Z), m(R,X,Z).
metarule(xy_yz_zx,P,Q,R):- m(P,X,Y), m(Q,Y,Z), m(R,Z,X).
metarule(xy_zx_yz,P,Q,R):- m(P,X,Y), m(Q,Z,X), m(R,Y,Z).
metarule(xy_zx_zy,P,Q,R):- m(P,X,Y), m(Q,Z,X), m(R,Z,Y).
metarule(xy_zy_xz,P,Q,R):- m(P,X,Y), m(Q,Z,Y), m(R,X,Z).
metarule(xy_zy_zx,P,Q,R):- m(P,X,Y), m(Q,Z,Y), m(R,Z,X).
*/


%!	metarule_constraints(+Metasubstitution,+Goal) is nondet.
%
%	A Goal to be called when Metasubstitution is matched.
%
%	@tbd This predicate is multifile so that it can be declared by
%	experiment files, however the definitions below are pretty
%	universally necessary to allow learning hypotheses with
%	left-recursions using dynamic learning and predicate invention.
%	So in they goes into the configuration that they might be used
%	by every experiment file. On the other hand, left-recursive
%	hypotheses may be required for some problems so this definition
%	is left commented out. This will not raise any errors because
%	metarule_constraints/2 is declared dynamic so a definition is
%	not necessary to exist in this module (or anywhere).
%
% Experiment files may or may not define metarule constraints to filter
% the Top program for unwanted clause structures (e.g. I don't like
% left-recursive clauses because they mess up evaluation).
:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.
/*
% Anti-recursion constraint - excludes recursive clauses
% Does not take into account invented or metarules with existentially
% quantified secod-order variables.
metarule_constraints(M,fail):-
	M =.. [m,Id,P|Ps]
	% Projection explicitly maps p/2 to p/1.
	,Id \= projection
	,forall(member(P1,Ps)
	       ,P1 == P).
*/

/*
% McCarthyite constraint - excludes left-recursive metasubstitutions
% Allows for invented predicates. Does not take into account existentially
% quantified secod-order variables in metarules.
metarule_constraints(M,fail):-
	M =.. [m,Id,P|Ps]
	,Id \= projection
	,left_recursive(P,Ps).

left_recursive(T,[T|_Ps]):-
	!.
left_recursive(T,[T,T|_Ps]):-
	!.
left_recursive_(T,[I,T|_Ps]):-
	atom_chars(I,['$',A])
	,atom_number(A,_N).
*/

/*
% Lexicographic order constraint - imposes total ordering on the Herbrand base.
% Allows for invented predicates and metarules with existentially
% quantified first-order variables.
% Remember to change #TARGET_PREDICATE/ARITY# with an actual symbol/arity.
configuration:metarule_constraints(M,fail):-
	debug_clauses(dynamic,'Testing constraint for metasub:',M)
	,configuration:max_invented(I)
	,M =.. [m,_Id|Ps]
	,predicate_signature(#TARGET_PREDICATE/ARITY#, Ss)
	,findall(P/A
		,(member(P,Ps)
		 ,(   memberchk(P/A,Ss)
		  ;   atomic(P)
		     ,invented_symbol(I,A,P)
		  )
		 )
		,Ps_)
	,\+ ordered_sublist(Ps_, Ss)
	,debug(dynamic,'Constraint test passed',[]).


%!	ordered_sublist(?Sublist,+Ordering) is det.
%
%	A Sublist order according to a total Ordering of its elements.
%
ordered_sublist([X,Y],Os):-
	above(X,Y,Os).
ordered_sublist([X,Y|Ls],Os):-
	above(X,Y,Os)
	,ordered_sublist([Y|Ls],Os).


%!	above(?Above,+Below,+Ordering) is det.
%
%	True when Above is above Below in a total Ordering.
%
above(S1,S2,Ss):-
	previous(S1,S2,Ss).
above(S1,S3,Ss):-
	previous(S1,S2,Ss)
	,above(S2,S3,Ss).
above(S1,S2,[_|Ss]):-
	above(S1,S2,Ss).


%!	previous(?First,?Next,?List) is det.
%
%	True when First and Next are the first two elements of List.
%
previous(S1,S2,[S1,S2|_Ss]).
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
%reduction(none).
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


%!	symbol_range(?Type,?Symbols) is semidet.
%
%	A list of Symbols to pretty-print predicates or variables.
%
%	Type is one of [predicate,variable], denoting the type of
%	symbols in the currenr range.
%
%	Symbols is a list of symbols of the given Type.
%
%	The atoms in list Symbols is used to assign names to the
%	variables in a metarule for pretty-printing.
%
%	Warning:
%	--------
%
%	symbol_range/2 must have exactly two clauses: one for the
%	symbols to be used as names for second-order existentially
%	quantified variables, and one to be used as names for
%	first-order existentially and universally quantified variables.
%
%	You can change each Symbols list as you see fit, but _do not
%	remove or add clauses_ to symbol_range/2!
%
%	Used by
%	-------
%
%	This predicate is used by predicates in the transitive closure
%	of print_metarules/1 and print_metarule/1, in particular,
%	numbered_symbol/3, which uses this to generate lists of
%	predicate symbols to be assigned to variables in metarules
%	according with their (first- or second-) order.
%
symbol_range(predicate, ['P','Q','R','S','T']).
symbol_range(variable, ['X','Y','Z','U','V','W']).
% Silly. Don't use.
%symbol_range(predicate, ['Alice','Bob','Carol']).
%symbol_range(variable, ['Smith','Brown','Carpenter','Miller','Green']).


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
