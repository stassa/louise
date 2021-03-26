:-module(subhypothesis_selection, [subhypothesis_dynamic/4
				  ,subhypothesis/4
				  ]).

:-use_module(project_root(configuration)).
:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).

/** <module> Predicates for subhypothesis selection.

Predicates in this module are used to reduce the Top program by
selecting one or more of its subsets of clauses as _subhypotheses_.

Subhypotheses selected by predicates in this module are _inductively_
irredundant, as opposed to _logically_ irredundant as the reductions of
the Top program by Plotkin's algorithm.

A clause C is inductively redundant with respect to another clause D,
iff the set of positive examples entailed by C is a subset of the set of
positive examples entailed by D.

However, note that there is no guarantee that any of the subhypotheses
selected by predicates in this module will also be _logically_
irredundant. Logical redundancy can be reduced using Plotkin's program
reduction algorithm implemented in the primary module of
lib(program_reduction).
*/


%!	subhypotheses_dynamic(+Pos,+BK,+Hypothesis,-Subhypotheses) is
%!	nondet.
%
%	Select a Subhypothesis that is a subset of Hypothesis.
%
%	Use this predicate to generate subhypotheses from an
%	over-hypothesis (i.e. the Top program) learned by a dynamic
%	learning predicate: learn_dynamic/1, learn_dynamic/2, or
%	learn_dynamic/5.
%
%	Pos is a list of the positive examples of a MIL problem.
%
%	BK is a list of predicate symbols and arities of the background
%	knowledge in the MIL problem.
%
%	Hypothesis is a hypothesis learned for the MIL problem.
%	Normally, Hypothesis should be the Top program for the MIL
%	problem, but it could also be already reduced (e.g. by Plotkin's
%	algorithm). Hypothesis may include invented predicates.
%
%	Subhypothesis is a list of clauses that is a subset of
%	Hypothesis and such that no clauses in Subhypothesis are
%	inductively redundant with respect to each other.
%
%	Like subhypothesis/4 this predicate uses setup_call_cleanup/3 to
%	ensure the positive examples and background knowledge are
%	written to the dynamic database before subhypothesis selection,
%	and that they are removed after the last successful result.
%
%	Additionally this predicate writes the _hypothesis_ to the
%	database, including any invented predicates. Predicates learned
%	dynamically may have dependent clauses, including recursive
%	clauses, in which case their call targets must be in the program
%	database for subhypothesis selection to correctly find the set
%	of examples entailed by each clause.
%
subhypothesis_dynamic(Pos,BK,Hs,Ss):-
% BK is a list of symbols and arities.
% Pos, Hs should not be encapsulated.
% TODO: test this clause properly.
	BK = [_F/_A|_]
	,encapsulated_problem(Pos,[],BK,[],[Pos_e,[],BK_e,[]])
	,clauses_invented(Hs,Cs,Is)
	,sort(Cs,Cs_)
	,maplist(encapsulated_clauses,[Cs_,Is],[Cs_e,Is_e])
	,flatten([Pos_e,BK_e,Cs_e,Is_e],Ps_e)
	,subhypothesis_safe(Pos_e,Ps_e,Cs_e,Ss_e)
	,examples_targets(Pos,Ts)
	,excapsulated_clauses(Ts,Ss_e,Ss_)
	,append(Ss_,Is,Ss).
subhypothesis_dynamic(Pos,BK,Hs,Ss):-
% Otherwise BK should be a list of clauses
% and Pos, Hs should be encapsulated.
	BK \= [_F/_A|_]
	,clauses_invented(Hs,Cs,Is)
	,sort(Cs,Cs_)
	,flatten([Pos,BK,Cs_,Is],Ps)
	,subhypothesis_safe(Pos,Ps,Cs_,Ss_)
	,append(Ss_,Is,Ss).



%!	clauses_invented(+Hypothesis,-Clauses,-Invented) is det.
%
%	Separate the Clauses of a Hypothesis from Invented clauses.
%
%	Hypothesis is a set of clauses consituting the definition of one
%	or more target predicates and possibly including definitions of
%	invented predicates.
%
%	Clauses is the list of all clauses in Hypothesis whose head
%	literal's predicate symbol is not the symbol of an invented
%	predicate.
%
%	Invented is the list of clauses in Hypotheses whose head
%	literal's predicate is the symbol of an invented predicate.
%
%	Invented predicate symbols are of the form '$N' i.e. an atomic
%	term where N is a positive integer.
%
clauses_invented(Hs,Cs,Is):-
	clauses_invented(Hs,[],Cs,[],Is).

clauses_invented([],Acc_Cs,Cs,Acc_Is,Is):-
	!
	,maplist(reverse,[Acc_Cs,Acc_Is],[Cs,Is]).
clauses_invented([H:-B|Hs],Acc_Cs,Bind_Cs,Acc_Is,Bind_Is):-
	invented_literal(H)
	,!
	,clauses_invented(Hs,Acc_Cs,Bind_Cs,[H:-B|Acc_Is],Bind_Is).
clauses_invented([C|Hs],Acc_Cs,Bind_Cs,Acc_Is,Bind_Is):-
	clauses_invented(Hs,[C|Acc_Cs],Bind_Cs,Acc_Is,Bind_Is).


%!	invented_literal(+Literal) is det.
%
%	True when Literal has an invented predicate symbol.
%
invented_literal(L):-
	L =.. [m,S|_As]
	,!
	,atom_chars(S,['$',I])
	,atom_number(I,_N).
invented_literal(L):-
	functor(L,S,_A)
	,atom_chars(S,['$',I])
	,atom_number(I,_N).



%!	subhypothesis(+Pos,+BK,+Hypothesis,-Subhypothesis) is det.
%
%	Select a Subhypothesis, a sub-set of the clauses in Hypothesis.
%
%	Pos is a list of the positive examples of a MIL problem.
%
%	BK is a list of predicate symbols and arities of the background
%	knowledge in the MIL problem.
%
%	Hypothesis is a hypothesis learned for the MIL problem.
%	Normally, Hypothesis should be the Top program for the MIL
%	problem, but it could also be already reduced (e.g. by Plotkin's
%	algorithm).
%
%	Subhypothesis is a list of clauses that is a subset of
%	Hypothesis and such that no clauses in Subhypothesis are
%	inductively redundant with respect to each other.
%
%	A clause C is inductively redundant with respect to another
%	clause D, iff the set of positive examples entailed by C is a
%	subset of the the set of positive examples entailed by D.
%
%	Note that subhypothesis/4 does not guarantee that clauses in the
%	selected Subhypothesis are not _logically_ redundant. For that
%	there is Plotkin's algorithm.
%
subhypothesis(Pos,BK,Hs,Ss):-
	BK = [_F/_A|_]
	,encapsulated_problem(Pos,[],BK,[],[Pos_e,[],BK_e,[]])
	,sort(Hs,Hs_)
	,encapsulated_clauses(Hs_, Hs_e)
	,flatten([Pos_e,BK_e],Ps_e)
	,subhypothesis_safe(Pos_e,Ps_e,Hs_e,Ss_e)
	,examples_targets(Pos,Ts)
	,excapsulated_clauses(Ts,Ss_e,Ss).
subhypothesis(Pos,BK,Hs,Ss):-
	BK \= [_F/_A|_]
	,sort(Hs,Hs_)
	,flatten([Pos,BK],Ps)
	,subhypothesis_safe(Pos,Ps,Hs_,Ss).


%!	subhypothesis_safe(+Pos,+Theory,+Hypothesis,-Subhypothesis) is
%!	det.
%
%	Business end of subhypothesis/4.
%
%	Calls subhypothesis_selection/3 via setup_call_cleanup/3
%	ensuring that clauses asserted to the database are removed after
%	nondeterministic success.
%
%	Raises permission error if a static term is passed in as part of
%	the Theory to be asserted to the dynamic database.
%
subhypothesis_safe(Pos,Ps,Hs,Ss):-
	S = assert_program(user,Ps,Refs)
	,G = subhypothesis_selection(Pos,Hs,Ss)
	,C = erase_program_clauses(Refs)
	,Call = setup_call_cleanup(S,G,C)
	% Expects permission error when trying to write static terms.
	,catch(Call
	      ,E
	      ,(writeln(E)
	       ,fail)
	      ).


%!	subhypothesis_selection(+Pos,+Hypothesis,-Subhypothesis) is
%!	nondet.
%
%	Select a Subhypothesis of Hypothesis.
%
subhypothesis_selection(Pos,Hs,Ss):-
	subhypothesis_selection(Pos,Hs,[],[],Ss_)
	,reverse(Ss_,Ss).

%!	subhypothesis_selection(+Pos,+Hypothesis,+Entailed,+Acc,-Subhypothesis)
%!	is nondet.
%
%	Business end of subhypothesis_selection/3.
%
%	@tbd Document procedure.
%
subhypothesis_selection([],_Hs,_Es,Ss,Ss):-
	!.
subhypothesis_selection(Pos,[],_Es,Acc,Ss):-
	!
	,append(Acc,Pos,Ss).
subhypothesis_selection(Pos,Hs,Es,[_Ci|Acc],Bind):-
	select(Ck,Hs,Hs_)
	,\+ tautology(Ck)
	% Here I could just get Es_ entailed by Ck
	% And then check that Es is a subset of Es_
	,clause_entailed(Ck,Es,Es,[])
	,clause_entailed(Ck,Pos,Es_,Pos_)
	,!
	,subhypothesis_selection(Pos_,Hs_,Es_,[Ck|Acc],Bind).
subhypothesis_selection(Pos,Hs,_Es,Acc,Bind):-
	select(C,Hs,Hs_)
	,\+ tautology(C)
	,clause_entailed(C,Pos,Es,Pos_)
	%,!
	,subhypothesis_selection(Pos_,Hs_,Es,[C|Acc],Bind).
subhypothesis_selection_0(Pos,_,_Es,Acc,Ss):-
	append(Acc,Pos,Ss).


%!	clause_entailed(+Clause,+Atoms,-Entailed,-Remaining) is det.
%
%	Collect Atoms Entailed by a Clause.
%
%	Remainging is the list of elements of Atoms that are not
%	entailed by Clause.
%
clause_entailed(H:-B,As,Es,Rs):-
	clause_entailed(H:-B,As,[],Es,[],Rs).

clause_entailed(_C,[],Es,Es,Rs,Rs):-
	!.
clause_entailed(C,[A|As],Es_Acc,Es_Bind,Rs_Acc,Rs_Bind):-
	entails(C,A)
	,!
	,clause_entailed(C,As,[A|Es_Acc],Es_Bind,Rs_Acc,Rs_Bind).
clause_entailed(C,[A|As],Es_Acc,Es_Bind,Rs_Acc,Rs_Bind):-
	clause_entailed(C,As,Es_Acc,Es_Bind,[A|Rs_Acc],Rs_Bind).


%!	entails(+Clause,+Atom) is det.
%
%	True when Clause entails Atom.
%
%	Entailment is decided by copying Clause, binding Atom to the
%	head of the copy and proving the copy's body literals.
%
%	Clause is copied to avoid propagating bindings to the rest of
%	the program. I like that terminology, it sounds so computer-
%	scientific: propagating bindings! Wow. I'm like, the Little
%	Computer Scientist!
%
entails(C,A):-
	copy_term(C,A:-B)
	,prove_body_literals(B).

%!	prove_body_literals(+Literals) is det.
%
%	Business end of entails/2.
%
%	Recursively proves the body Litearls of a clause whose head has
%	been ground to a positive example in entails/2.
%
prove_body_literals((L,Ls)):-
	safe_call(L)
	,prove_body_literals(Ls).
prove_body_literals(L):-
	L \= (_,_)
	,safe_call(L).


%!	safe_call(+Literal) is det.
%
%	Call a Literal, if it exists.
%
%	Defensive structure to avoid raising an error if a literal does
%	not exist.
%
%	For example, it can happen that if a projection metarule is
%	used, an atom of a predicate that does not exist with that arity
%	is called. This avoids that kind of error.
%
safe_call(L):-
	predicate_property(L,number_of_clauses(N))
	,N > 0
	,user:call(L).
