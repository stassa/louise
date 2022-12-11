:-module(minimal_program, [learn_minimal/1
			  ,learn_minimal/2
			  ,learn_minimal/5
			  ,reduced_examples/3
			  ]).

:-use_module(project_root(configuration)).
:-use_module(src(louise)).
:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).

/** <module> Simultaneous construction and reduction of the Top program.

This is the general idea:

a) Take a positive example
b) Generalise it (construct a clause that entails it)
c) Specialise it (drop it if it entails any negative examples)
d) Find the positive examples the new clause entails
e) Remove them from the set of positives
f) Continue with a new positive example

This is essentially a coverage test peformed on each new clause of the
Top program. The purpose however is to avoid deriving multiple clauses
that entail the same examples, rather than testing whether a clause
belongs to a correct hypothesis - the latter task is handled by the
genealisation and specialisation steps, as in ordinary Top program
construction.

The learning predicates in this module will backtrack over all clauses
generalising a positive example thereby generating multiple correct
hypotheses. Although there is no guarantee of minimality (despite the
optimistic naming of the learning predicates) and no attempt to order
the returned hypotheses by size, it should generally be the case that
shorter hypotheses are found before longer ones.

*/

%!	learn_minimal(+Targets) is nondet.
%
%	Learn a minimal definition of one or more learning Targets.
%
learn_minimal(Ts):-
	learn_minimal(Ts,Ps)
	,print_clauses(Ps).



%!	learn_minimal(+Targets,+Program) is det.
%
%	Learn a minimal definition of one or more learning Targets.
%
%	The elements of the MIL problem are taken from the current
%	experiment file, via tp_safe_experiment_data/5.
%
learn_minimal(Ts,_Ps):-
	(   \+ ground(Ts)
	->  throw('learn_minimal/2: non-ground target symbol!')
	;   fail
	).
learn_minimal(Ts,Ps):-
	tp_safe_experiment_data(Ts,Pos,Neg,BK,MS)
	,learn_minimal(Pos,Neg,BK,MS,Ps).



%!	learn(+Pos,+Neg,+BK,+Metarules,-Progam) is det.
%
%	Learn a minimal Progam from the elements of a MIL problem.
%
learn_minimal([],_Neg,_BK,_MS,_Ts):-
	throw('learn_minimal/5: No positive examples found. Cannot train.').
learn_minimal(Pos,Neg,BK,MS,_Ts):-
	(   var(Pos)
	->  throw('learn_minimal/5: unbound positive examples list!')
	;   var(Neg)
	->  throw('learn_minimal/5: unbound negative examples list!')
	;   var(BK)
	->  throw('learn_minimal/5: unbound background symbols list!')
	;   var(MS)
	->  throw('learn_minimal/5: unbound metarule IDs list!')
	;   fail
	).
learn_minimal(Pos,Neg,BK,MS,Ps):-
	debug(minimal_program,'Encapsulating problem',[])
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,debug(minimal_program,'Constructing minimal program...',[])
	,minimal_program(Pos_,Neg_,BK_,MS_,Ms)
	,examples_targets(Pos,Ss)
	,debug(minimal_program,'Excapsulating hypothesis',[])
	,excapsulated_clauses(Ss,Ms,Ps).


%!	minimal_program(+Pos,+Neg,+BK,+MS,-Program) is nondet.
%
%	Construct a minimal Program from the elements of a MIL problem.
%
minimal_program(_Pos,_Neg,_BK,_MS,_Ts):-
	configuration:theorem_prover(tp)
	,throw('theorem_prover(tp) not working with minimal program learning').
minimal_program(Pos,Neg,BK,MS,Ts):-
	configuration:theorem_prover(resolution)
	,configuration:max_error(L_H,L_C)
	,C_H = c(0)
	,C_C = c(0)
	,S = write_problem(user,[BK,Pos],Refs)
	,G = minimal_program_(Pos,Neg,MS,[L_H,L_C,C_H,C_C],[],Ts)
	,C = erase_program_clauses(Refs)
	,setup_call_cleanup(S,G,C).
minimal_program(_Pos,_Neg,_BK,_MS,[]):-
% If Top program construction fails return an empty program.
	debug(minimal_program,'INSUFFICIENT DATA FOR MEANINGFUL ANSWER',[]).


%!	minimal_program_(+Pos,+Neg,+Metarules,+Errors,+Acc,-Program) is
%!	nondet.
%
%	Business end of minimal_program/5.
%
%       minimal_program_/5 performs the following steps in sequence:
%
%       a) Take the next positive example, e+ in Pos
%       b) Generalise e+ to a metasubstitution, S
%	c) Test S against constraints
%	d) If S passes the test, specialise S against the negative
%	examples; else, repeat from (b)
%	e) If S entails any negative examples, repeat from (b)
%	f) Else, apply S to its corresponding metarule, yielding a
%	definite clause, C (encapsulated)
%	g) Remove from Pos those examples entailed by C
%	h) Add C to the minimal program
%	i) If e+ cannot be generalised, continue with a new positive
%	example, e+' in Pos
%	j) Return when there are no more positive examples left.
%
%	Step (g), the reduction of the positive examples is performed by
%	reduced_examples/3. That predicate removes from the list Pos,
%	all those (positive example) atoms entailed by the latest
%	derived clause C. Learning continues with the examples remaining
%	in Pos after this reduction step.
%
%	The end result is a set of clauses that entail each positive
%	example and none of the positive examples, such that no two
%	clauses in the set entail the same positive examples. In this
%	sense, the returned program is irredundant, or even minimal.
%
%	@bug If generalising a positive example fails, the example atom
%	is added to the Program as an "exception" (or "atomic residue").
%	However, because generalisation proceeds nondeterministicaly
%	until an example cannot be generalised any more, failure to
%	generalis an example will happen exactly once for _every_
%	positive example. Which means that, even given a set of examples
%	for which there _does_ exist a minimal program entailing each
%	example in the set, this predicate will eventually generate all
%	hypotheses including sub-sets of the positive examples as
%	"exceptions". More work needed.
%
minimal_program_([],_Neg,_MS,_Es,Acc,Cs):-
% Sorting in descending order to move atomic residue to the top of the
% program. Other clauses will generally not be sorted, e.g.
% alphabetically- at least not unless they are fully ground. That's
% because the standard order of terms orders variables according to
% their age (i.e. the order in which they were initialised in Prolog's
% memory). For this reason, we're grounding or otherwise jumping through
% hoops to sort, non-ground Prolog terms elsewhere in the project. See
% for example program_results/4 in lib/evaluation/evaluation.pl etc.
	!
       ,minimal_program_constraints(Acc)
       ,sort(0,@>,Acc,Cs).
minimal_program_([E|Pos],Neg,MS,[L_H,L_C,C_H,C_C],Acc,Bind):-
	generalise_minimal(E,MS,M,Sub)
	,constraints(Sub)
	,specialise_minimal(Neg,M,[L_H,L_C,C_H,C_C],Sub)
	,metarule_application(Sub,M,C)
	,\+ tautology(C)
	,reduced_examples(Pos,C,Pos_)
	,minimal_program_(Pos_,Neg,MS,[L_H,L_C,C_H,C_C],[C|Acc],Bind).
/*minimal_program_([_E|Pos],Neg,MS,Acc,Bind):-
% Causes multiple backtrackings over the same clauses.
% Probably backtracking over generalise_minimal/4 after
% bouncing off minimal_program_constraints/1.
	minimal_program_(Pos,Neg,MS,Acc,Bind).
*/
minimal_program_([E|Pos],Neg,MS,[L_H,L_C,C_H,C_C],Acc,Bind):-
% If E cannot be generalised, add it to the program as an "exception".
	minimal_program_(Pos,Neg,MS,[L_H,L_C,C_H,C_C],[E|Acc],Bind).


%!	generalise_minimal(+Example,+Metarules,-Metarule,-Metasubstitution)
%!	is nondet.
%
%	Generalisation step of minimal Top program construction.
%
%	Example is a single positive example atom. Metarules is the list
%	of encapsulated and expanded metarules in the current MIL
%	problem.
%
%	Metarule is the metarule used to perform a metasubstitution. We
%	output this so it can be passed to specialise_minimal/3 which
%	simplifies testing the constructed Metasubstitution.
%
%	Metasubstitution is a metasubstitution of Metarule that entails
%	Example.
%
%	genearlise_minimal/4 will generate all metasubstitutions
%	entailing Example on backtracking.
%
generalise_minimal(Ep,MS,M,Sub):-
	copy_term(MS,MS_)
	,member(M,MS_)
	,copy_term(M,M_)
	,metasubstitution(Ep,M_,Sub).


%!	specialise_minimal(+Neg,+Metarule,+Errors,+Metasubstitution) is
%!	nondet.
%
%	Specialisation step of minimal Top program construction.
%
%	This is the same as the specialisation step in Louise: it tests
%	a Metasubstitution derived in the genearlisation step against
%	the negative examples and fails if there is any negative example
%	that is entailed by Metasubstitution.
%
%	@tbd Is Metarule really necessary to be passed here? I'm not
%	sure.
%
specialise_minimal(Neg,M,[L_H,L_C,C_H,C_C],Sub):-
	nb_setarg(1,C_C,0)
	,copy_term(M,M_)
	,\+((member(:-En,Neg)
	    ,metasubstitution(En,M_,Sub)
	    ,(   louise:error_exceeded(L_H,C_H,hypothesis)
	     ;   louise:error_exceeded(L_C,C_C,clause)
	     )
	    ,debug(errors,'Maximum Errors exceeded!',[])
	    )).


%!	metasubstitution(+Example,+Metarule,?Metasubstitution) is
%!	nondet.
%
%	Derive a Metasubstitution of Metarule entailing an Example.
%
%	Example is either a positive example or a negative example. A
%	positive example is a ground definite unit clause, while a
%	negative example is a ground definite goal (i.e. a clause of the
%	form :-Example).
%
%	Metasubstitution may also be bound on entry, in which case this
%	predicate verifies the entailment of Example by
%	Metasubstitution.
%
%	Copied from Louise module.
%
metasubstitution(E,M,Sub):-
	bind_head_literal(E,M,(Sub:-(E,Ls)))
	,user:call(Ls).


%!	bind_head_literal(+Example,+Metarule,-Head) is det.
%
%	Bind an Example to the encapsulated Head literal of a Metarule.
%
%	Abstracts the complex patterns of binding examples to the heads
%	of metarules with and without body literals.
%
%	Copied from Louise module.
%
bind_head_literal(E,M,(H:-(E,Ls))):-
	M = (H:-(E,Ls))
	,!.
bind_head_literal(E,M,(H:-(E,true))):-
	M = (H:-E).


%!	metarule_application(+Metasubstitution,+Metarule,-Clause) is det.
%
%	Apply a Metasubstitution to its corresponding Metarule.
%
%	Copied from dynamic_learning module.
%
metarule_application(Sub,Sub:-(H,B),H:-B).


%!	reduced_examples(+Positives,+Clause,-Free) is det.
%
%	Collect positive examples not entailed by a Clause.
%
%	Positives is the list of all free Positive examples. Clause is a
%	clause in the minimal program. Free is the list of examples in
%	Positives that are not entailed by Clause.
%
reduced_examples(Pos,C,Pos_):-
	free_examples(Pos,C,[],Pos_).

%!	free_examples(+Pos,Clause,+Acc,-Free) is det.
%
%	Business end of reduced_examples/3.
%
free_examples([],_C,Fs,Fs):-
	!.
free_examples([E|Pos],C,Acc,Bind):-
	copy_term(C,E:-B)
	,call(B)
	,!
	,free_examples(Pos,C,Acc,Bind).
free_examples([E|Pos],C,Acc,Bind):-
	free_examples(Pos,C,[E|Acc],Bind).


%!	minimal_program_constraints(+Program) is det.
%
%	True when Program does not violate minimal program constraints.
%
%	Cureently checked constraints:
%
%	a) Program must be a set of clauses of length n where k =< n
%	=< j, and k, j are the minimum and maximum cardinality of
%	minimum programs, specified in the configuration option
%	minimal_program_size/1.
%
minimal_program_constraints(Ps):-
	configuration:minimal_program_size(Min,Max)
       ,length(Ps,N)
       ,Min =< N
       ,N =< Max.
