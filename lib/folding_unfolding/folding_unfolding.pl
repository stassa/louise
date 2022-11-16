:-module(folding_unfolding, [fold_recursive/2
			    ,unfold_invented/4
			    ,pseudo_unfold_invented/3
			    ,reduce_unordered/2
                            ]).

:-use_module(src(auxiliaries)).
:-use_module(lib(lifting/lifting)).

/** <module> Folding and unfolding predicates.

There are two groups of predicates in this module.

1. fold_recursive/2

This predicate can fold an over-specialised program to introduce
recursion.

For an example, see the query below, taken from the experiment file in
data/examples/recursive_folding.pl:

==
?- learn(list_last/2, _Ps), print_clauses('Learned', _Ps), fold_recursive(_Ps, _Fs), nl, print_clauses('Folded', _Fs).
Learned
list_last(A,B):-tail(A,C),empty(C),head(A,B).
list_last(A,B):-tail(A,C),tail(C,D),empty(D),head(C,B).
list_last(A,B):-tail(A,C),tail(C,D),tail(D,E),empty(E),head(D,B).

Folded
list_last(A,B):-tail(A,C),empty(C),head(A,B).
list_last(A,B):-tail(A,C),list_last(C,B).
true.
==

2. unfold_invented/4, pseudo_unfold_invented/3 and reduce_unfolded/2.

These three predicates unfold hypotheses with invented predicates,
learned by dynamic learning or Thelma, to remove invented symbols.

unfold_invented/4 is automatically applied to hypotheses learned with
learning predicates capable of predicate invention, such as
learn/[1,2,5] or thelma/[1,2,5], when the configuration option
unfold_invened/1 is set to "true".

For an example of unfolding a learned program to remove invented
predicates see the query below, taken from the experiment file in
data/examples/anbn.pl:

==
?- learn(s/2), nl, unfold_invented(Unfolded).
'$1'(A,B):-a(A,C),a(C,B).
'$1'(A,B):-a(A,C),s(C,B).
'$1'(A,B):-b(A,C),b(C,B).
'$1'(A,B):-s(A,C),b(C,B).
s(A,B):-'$1'(A,C),'$1'(C,B).
s(A,B):-'$1'(A,C),b(C,B).
s(A,B):-a(A,C),'$1'(C,B).
s(A,B):-a(A,C),b(C,B).

Unfolded = false.

?- learn(s/2), nl, unfold_invented(Unfolded).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),a(C,D),b(D,E),b(E,B).
s(A,B):-a(A,C),s(C,D),b(D,B).

Unfolded = true.
==

*/


%!	fold_recursive(+Clauses,-Folded) is det.
%
%	Fold a set of Clauses into recursive variants, if possible.
%
%	Clauses is a set of clauses. Folded is a set of clauses such
%	that each clause C in Clauses is folded into a clause C' by
%	replacing any sequence of literals L1, ..., Ln in C by the head
%	of a clause in Clauses.
%
fold_recursive(Cs,Fs):-
	program_reduction(Cs,Rs,_)
	,sort_clauses(Rs, Rs_)
	,fold_recursive_all(Rs_,Fs).


%!	fold_recursive_all(+Clauses,-Folded) is det.
%
%	Fold a set of clauses until no redundancies are left.
%
%	Ensures every clause that can be folded into another, is.
%
fold_recursive_all(Cs,Rs):-
	fold_clauses(Cs,Fs)
	,program_reduction(Fs, Fs_r,_)
	,sort_clauses(Fs_r, Fs_)
	,Fs_ \= Cs
	,!
	,fold_recursive_all(Fs_,Rs).
fold_recursive_all(Rs,Rs).


%!	sort_clauses(+Clauses,-Sorted) is det.
%
%	Sort a set of clauses, ignoring variable ages.
%
sort_clauses(Cs,Cs_s):-
	setof(C
              ,Cs^(member(C,Cs)
		  ,numbervars(C)
               )
              ,Cs_)
	,findall(C_1
		,(member(C_,Cs_)
		 ,varnumbers(C_,C_1)
		 )
		,Cs_s).


%!	fold_clauses(+Clauses,-Folded) is det.
%
%	Fold a set of Clauses to add recursion, if possible.
%
fold_clauses(Cs,Fs):-
        fold_clauses(Cs,Cs,[],Fs).


%!	fold_clauses(+ToFold,+Clauses,+Acc,-Folded) is det.
%
%	Business end of fold_clauses/2.
%
%	ToFold and Clauses begin as the same list of clauses to be
%	folded. Each clause in ToFold is attempted to be folded by
%	replacing some of its body literals with the head literal of a
%	head in Clauses, if possible.
%
fold_clauses([],_,Fs,Fs):-
        !.
fold_clauses([C|Cs],Cs_,Acc_1,Bind):-
        fold_clause(C,Cs_,Acc_1,Acc_2)
        ,fold_clauses(Cs,Cs_,Acc_2,Bind).


%!	fold_clause(+Clause,+Clauses,+Acc,-Folded) is det.
%
%	Fold a Clause to add recursion, if possible.
%
%	Folds each fold given as the first argument in fold_clauses/4
%	with each of the clauses in the list given as the first argument
%	in fold_clauses/4, replacing Clause with a folded clause with a
%	recursive call, if possible.
%
fold_clause(_C1,[],Fs,Fs):-
        !.
fold_clause(C1,[C2|Cs],Acc,Bind):-
        folding(C1,C2,C3)
        ,fold_clause(C1,Cs,[C3|Acc],Bind).


%!	folding(+Clause1,+Clause2,-Folded) is det.
%
%	Fold two clauses to introduce recursion, if possible.
%
%	Let Clause1 = H1:- L11, ..., Li, ..., Lk, ..., Ln and let
%	Clause2 = H2:- L21, ..., L2m. Then, if H1 and H2 have the same
%	predicate symbol and arity AND L21 ,..., L2m = Li, ..., Lk,
%	Clause3 is the clause H2:- L11,..., H2, ..., Ln.
%
%	Note that in the process of derving Clause3 from Clause2, first
%	L21 ,..., L2m = Li, ..., Lk are unified. Unification changes the
%	variables in Clause1, resulting in new variable names in
%	Clause3, but not in a new "wiring" of the variables.
%
%	The result of all this is that Clause 3 has one or more
%	recursive body literals.
%
%	If there is no sub-sequence Li, ..., Lk of the body literals in
%	Clause1 that matches the body literals of Clause2, then Clause3
%	is Clause1, unchanged.
%
folding(C1, C2, H_1:-Ls_4):-
        maplist(clause_literals,[C1,C2],[[H_1|Ls_1],[H_2|Ls_2]])
        ,replace_all(H_2,Ls_1,Ls_2,Ls_3)
        ,once(list_tree(Ls_3,Ls_4)).


%!	replace_all(+Head,+Body1,+Body2,-Replaced) is det.
%
%	Replace all occurrences of a set of literals with a new one.
%
%	Head is the head of a clause that has Body2 as its body. Body1
%	is a sequence of body literals of a possibly different clause,
%	that has Body2 as its sub-sequence. Replaced is the sequence of
%	literals in Body1, where the sub-sequence matching Body2 is
%	replaced by Head.
%
%	To replace Body2 in Body1 with Head, literals in Body1 and Body2
%	are unified, resulting in binding of variables in Head, Body1
%	and Body2. This is necessary to preserver the "wiring" of
%	literals in the original clauses and to make sure the clause in
%	Replaced is fully-connected (or "closed"), i.e. that it doesn't
%	have any free variables left dangling without sharing.
%
replace_all(S,Xs,Ys,Zs):-
        replace_all_(S,Xs,Ys,Zs).

%!	replace_all_(+Head,+Body1,+Body2,-Replaced) is det.
%
%	Business end of replace_all/4.
%
replace_all_(S,Xs,Ys,Bind):-
        replace(S,Xs,Ys,Xs_)
        ,!
        ,replace_all_(S,Xs_,Ys,Bind).
replace_all_(_S,Zs,_Ys,Zs).


%!	replace(+Head,+Body1,+Body2,-Replaced) is det.
%
%	Replace all occurrences of Body2 in Body1 with Head.
%
replace(S,Xs,Ys,Zs):-
        replace(S,Xs,Ys,[],Zs).

%!	replace(+Head,+Body1,+Body2,+Acc,-Replaced) is det.
%
%	Business end of replace/4.
%
replace(S,[X|Xs],[Y|Ys],Acc,Bind):-
% Elements don't match.
        X \= Y
        ,!
        ,replace(S,Xs,[Y|Ys],[X|Acc],Bind).
replace(S,[Y|Xs],[Y|Ys],Acc,Zs):-
% Matchine elements of a _possible_ shared sub-sequence.
        drop(Xs,Ys,Acc_2)
        ,!
        ,reverse(Acc,Acc_1)
        ,append(Acc_1,[S|Acc_2],Zs).
replace(S,[X|Xs],[Y|Ys],Acc,Zs):-
% Matching elements _not_ in a shared subsequence.
        replace(S,Xs,[Y|Ys],[X|Acc],Zs).


%!	drop(+Sequence,+Subsequence,-Dropped) is det.
%
%	Drop all elemeents of a Subsequence from a Sequence.
%
%	Sequence and Subsequence are lists, interpreted as sequences.
%	Dropped is the result of removing Subsequence from Sequence.
%
drop(Xs,[],Xs):-
        !.
drop([Y|Xs],[Y|Ys],Acc):-
        drop(Xs,Ys,Acc).



%!	unfold_invented(+Program,+Pos,+BK,-Unfolded) is det.
%
%	Unfold a Program to remove invented predicates.
%
%	Program is a set of clauses learned by one of Louise's learning
%	predicates and possibly including definitions of invented
%	predicates.
%
%	Pos and BK are the sets of positive examples and BK symbols and
%	arities of target predicates in Clauses.
%
%	Unfolded is the list of clauses in Program unfolded to remove
%	invented predicates.
%
%	It is easier to explain unfolding by means of an example:
%	==
%	?- learn(s/2), nl, unfold_invented(Unfolded).
%	'$1'(A,B):-a(A,C),a(C,B).
%	'$1'(A,B):-a(A,C),s(C,B).
%	'$1'(A,B):-b(A,C),b(C,B).
%	'$1'(A,B):-s(A,C),b(C,B).
%	s(A,B):-'$1'(A,C),'$1'(C,B).
%	s(A,B):-'$1'(A,C),b(C,B).
%	s(A,B):-a(A,C),'$1'(C,B).
%	s(A,B):-a(A,C),b(C,B).
%
%	Unfolded = false.
%
%	?- learn(s/2), nl, unfold_invented(Unfolded).
%	s(A,B):-a(A,C),b(C,B).
%	s(A,B):-a(A,C),a(C,D),b(D,E),b(E,B).
%	s(A,B):-a(A,C),s(C,D),b(D,B).
%
%	Unfolded = true.
%	==
%
%	__Motivation__
%
%	The purpose of this predicate is to make programs learned with
%	predicate invention more comprehensible by removing invented
%	predicates, while still maintaining programs' semantics.
%
%	Predicates invented by Louise have automatically assigned
%	symbols such as '$1', '$2' etc, so that their meaning is not
%	immediately obvious. Although such predicates can sometimes make
%	a program _more_ comprehensible by reducing its size, in general
%	it's safe to assume that a program without invented predicates
%	will be at least as comprehensible as a program of the same size
%	and _with_ invented predicates.
%
%	The s/2 example above is such a case. In the "Invented" form
%	of the program, it's hard to say what the predicate '$1'/2 is
%	meant to represent. Unfolding the program we obtain a version
%	without '$1'/2 that is closer to (if not identical with) a
%	proram that a human programmer would have written (indeed, the
%	proram is a classic definition of a grammar for the anbn
%	language).
%
%	__Unfolding__
%
%	Unfolding is the dual of folding, a program transformation that,
%	in logic programs, replaces goals in the bodies of clauses with
%	the body literals of the "called" clauses.
%
%	Procedurally, unfolding resolves the head literal of a clause C,
%	with a body literal, L, of another clause, D, producing a new
%	clause with the head literal of C and the body literals of C and
%	D, except for L.
%
%	For example, suppose C = p:- q, r, D = q:- s, t and L = q. Then,
%	unfolding D onto L in C would result in a new clause, E = p:-
%	s,t,r.
%
%	As the s/2 example in the prvious section demonstrates, if L
%	is the literal of an invented predicate in a clause of a target
%	predicate, unfolding will produce a new clause without any
%	literals of invented predicates.
%
%	Note that unfolding is resolution. The unfolded clause of p/0 in
%	the example above is a resolvent of the two original clauses.
%	unfold_invented/4 indeed resolves input clauses by means of a
%	meta-interpreter modified to collect resolvents of clauses of
%	target predicates with clauses of invented predicates.
%
%	__Restricted unfolding__
%
%	Unlike general unfolding this predicate does not
%	indiscriminately generate all resolvents of the clauses of
%	target predicates and invented predicates. Instead, only those
%	resolvents that entail at least one positive example with
%	respect to background knowledge are constructed, i.e. the
%	process is driven by the positive examples.
%
%	For this reason, unfold_invented/4 needs to be given the
%	positive examples and the background knowledge for a MIL problem
%	from which the input Program was learned. Example atoms and BK
%	predicate indicators are passed in the second and third
%	arguments of unfold_invented/4.
%
%	__Limitations__
%
%	This predicate does not take into account the negative examples
%	and instead generates the set of all resolvents of clauses in
%	Program that entail at least one positive example. As a result,
%	Unfolded _may_ include any number of clauses that entail some
%	negative examples. Whether this is really possible is to be
%	determined.
%
%	@tbd Currently, unfold_clauses/5 (auxiliary to this predicate
%	that does most of the actual unfolding) returns clauses ground
%	to constants obtained during a refutation of a positive example.
%	This is actually not that bad, because it incidentally forces
%	unfold_clauses/5 to succeed or fail exactly once for each
%	resolvent of a clause of a target predicate (whereas otherwise
%	it would potentially backtrack over all positive examples).
%	Unfortunately, this also requires the unfolded program to be
%	"lifted" to variabilise its constants. To achieve this, we
%	utilise a predicate theory_constants/2 to collect all constants
%	in the encapsulated input Program. The process is a little
%	fiddly and not exactly the most efficient thing in the world,
%	but the alternative is even fiddlier and includes passing around
%	metasubstitutions and instantiating their existentially
%	quantified variables. While this is doable, honestly, I don't
%	have the heart to do it. Too much work. So unfolding a program
%	will be slow if we have too many clauses with too many damn
%	literals. OK.
%
%	@bug If a clause in Program does not directly entail any
%	positive examples unfold_invented/4 will silently drop it from
%	its output. This is the case, for example, with the base-case of
%	a recursive definition learned from a single example that is not
%	an example of the base case.
%
unfold_invented(Ps,Pos,BK,Us):-
	examples_targets(Pos,Ts)
	,program_invented(Ps,Ts,Cs,Is)
	,invented_symbols_(Is,Ss)
	,closure(BK,experiment_file,Bs)
	,flatten(Bs,Bs_f)
	,!
	,S = (write_problem(unfolding,[Ps,Pos,Bs_f],Refs)
	     ,table(unfold_literals/5)
	     ,table(prove/1)
	     )
	,G = unfold_clauses(Cs,Ss,Is,[],Us_)
	,C = (erase_program_clauses(Refs)
	     ,untable(unfold_literals/5)
	     ,untable(prove/1)
	     )
	,setup_call_cleanup(S,G,C)
	,flatten(Us_,Us_f)
	,theory_constants(Ps,Ps_Cs)
	,lifted_program(Us_f,Ps_Cs,Us_1)
	,predsort(unifiable_compare,Us_1,Us_s)
	,reduced_top_program(Pos,Bs,[],Us_s,Us_r)
	,filter_targets(Ts,Us_r,Us).
unfold_invented(Ps,Pos,_BK,Ps):-
	examples_targets(Pos,Ts)
	,program_invented(Ps,Ts,_Cs,[]).


%!	program_invented(+Program,+Targets,-Clauses,-Invented) is det.
%
%	Partition a Program to clauses of target or Invented Predicates.
%
%	Program is a program learned by dynamic learning which may
%	include the definitions of one or more invented predicates.
%
%	Targets is the list of predicate symbols and arities of the
%	target predicates in Program, as F/A predicate indicators.
%
%	Clauses is the list of clauses of predicates in Targets, i.e.
%	clauses with head literals having the predicate symbol and arity
%	of a predicate indicator in Targets.
%
%	Invented is the list of clauses of invented predicates, i.e.
%	clauses with head literals having an invented predicate symbol
%	such as '$1', '$2', etc.
%
program_invented(Ps,Ts,Cs,Is):-
	program_invented(Ps,Ts,[],Cs_,[],Is_)
	,maplist(reverse,[Cs_,Is_],[Cs,Is]).

%!	program_invented(+Program,+Targets,+Acc1,-Clauses,+Acc2,-Invented)
%	is det.
%
%	Business end of program_invented/4.
%
program_invented([],_Ts,Cs,Cs,Is,Is):-
	!.
program_invented([C|Ps],Ts,Cs_Acc,Cs_Bind,Is_Acc,Is_Bind):-
	clause_of(C,Ts)
	,!
	,program_invented(Ps,Ts,[C|Cs_Acc],Cs_Bind,Is_Acc,Is_Bind).
program_invented([C|Ps],Ts,Cs_Acc,Cs_Bind,Is_Acc,Is_Bind):-
	program_invented(Ps,Ts,Cs_Acc,Cs_Bind,[C|Is_Acc],Is_Bind).


%!	clause_of(+Clause,+Signature) is det.
%
%	True when Clause is a clause of a predicate in Signature.
%
%	Clause is a definite clause. Signature is a list of predicate
%	symbols and arities.
%
%	A call clause_of(C,Ss) succeeds when the symbol and arity of the
%	head literal in clause C is S/A and S/A is in Ss.
%
clause_of(C,Ts):-
% Encapsulated clause
        clause_symbol(C,F/A)
	,memberchk(F/A,Ts).


%!	clause_symbol(+Clause,-Symbol) is det.
%
%	The predicate Symbol and arity of a Clause.
%
%	@tbd This could be useful elsewhere in the project. Consider
%	adding to auxiliaires, probably.
%
clause_symbol(H:-_B,F/N):-
% Encapsulated clause
	functor(H,m,_A)
	,H =.. [m,F|As]
	,length(As, N)
	,!.
clause_symbol(L,F/N):-
% Encapsulated unit clause
	functor(L,m,_A)
	,L =.. [m,F|As]
	,length(As,N)
	,!.
clause_symbol(H:-_B,F/A):-
% Definite clause
	functor(H,F,A)
	,!.
clause_symbol(L,F/A):-
% Unit clause
	functor(L,F,A).


%!	invented_symbols_(+Invented,-Symbols) is det.
%
%	Collect the Symbols of a list of clauses of Invented predicates.
%
%	Invented is a list of clauses of invented predicates'
%	definitions as returned by program_invented/4. Symbols is a list
%	of F/A terms where each F is the symbol of an invented predicate
%	in Invented and each A is its arity.
%
%	@tbd there is a predicate, invented_symbols/2 in the auxiliaries
%	module that does this kind of thing. The difference with this
%	program is that invented_symbols/2 generates invented symbols
%	directly from the configuration option max_invented/2 and so
%	there may be situations in which Invented may contain symbols
%	not generated by invented_symbols/2. This may happen, for
%	example, if unfold_invented/3 is called manually with some
%	arbitrary user-defined program, for testing. That said, do
%	consider using invented_symbols/2 instead of this predicate.
%
invented_symbols_(Is,Ss):-
	setof(F/A
	     ,B^Is^H^(member(H:-B,Is)
		     ,clause_symbol(H,F/A)
		     )
	     ,Ss).


%!	unfold_clauses(+Clauses,+Symbols,+Invented,+Acc,-Unfolded) is
%!	det.
%
%	Unfold a set of Clauses with a set of Invented definitions.
%
%	Clauses are the clauses of the target predicates collected from
%	a program learned by Louise with dynamic learning and that may
%	include one or more clauses with body litearls having invented
%	predicate symbols. Clauses will typically be returned by
%	program_invented/4 as its third argument.
%
%	Invented is a list of the clauses of definitions of invented
%	predicates originally in the same program as Clauses. Invented
%	will typically be the result returned by program_invented/4 as
%	its last argument.
%
%	Symbols is the set of predicate symbols and arities of
%	predicates in Invented.
%
%	Unfolded is the set of resolvents of each clause in Clauses with
%	the clauses in Invented. Unfolding results in a set of clauses
%	without any body literals of invented predicates.
%
unfold_clauses([],_Ss,_Is,Us,Us):-
	!.
unfold_clauses([C|Cs],Ss,Is,Acc,Bind):-
	findall(C_
	       ,unfold_clause(C,Ss,Is,C_)
	       ,Us)
	,unfold_clauses(Cs,Ss,Is,[Us|Acc],Bind).


%!	unfold_clause(+Clause,+Symbols,+Invented,-Unfolded) is nondet.
%
%	Auxiliary to unfold_clauses/5, operating on a single Clause.
%
%	Unfold a Clause with a list of Invented clauses.
%
unfold_clause(H:-B,Ss,Is,H:-B_):-
% The call to clause/2 binds the head of the clause to an example in the
% dynamic database. Remember that, since unfold_invented/3 is called at
% the end of a dynamic learning attempt, but _before_ the elements of a
% MIL problem are retracted from the dynamic database, the clauses of
% positive examples are still available. The clauses of the learned
% hypothesis are _not_ also still in the dynamic database - but even if
% they were, the "true" bound to the second argument of the clause
% restricts the search for clauses unifying with H only to unit clauses
% "facts"). Which are _probably_ examples.
%
% TODO: Actually, all of the above means that we can't process arbitrary
% definite clause examples. For those we'd have to modify this code
% slightly.
	must_be(nonvar,H)
	,must_be(nonvar,B)
	,clause(unfolding:H,true)
	,unfold_literals(B,Ss,Is,(H),U_)
	,treeverse(U_,(H,B_)).


%!	unfold_literals(+Literals,+Symbols,+Invented,+Acc,-Unfolded) is
%!	nondet.
%
%	Auxiliary to unfold_clause/4, operating on each of its Literals.
%
%	Unfold a literal with a list of Invented clauses.
%
unfold_literals(true,_Ss,_Is,Us,Us):-
	!.
unfold_literals((L,Ls),Ss,Is,Acc,Bind):-
	unfold_literals(L,Ss,Is,Acc,Acc1)
	,unfold_literals(Ls,Ss,Is,Acc1,Bind).
unfold_literals(L,Ss,Is,Acc,Bind):-
	L \= (_,_)
	,member(C,Is)
	,head_body(C,L,Ls)
	,unfold_literals(Ls,Ss,Is,Acc,Bind).
unfold_literals(L,Ss,Is,Acc,Bind):-
% At this point, the clause of which L is a body literal may be any
% resolvent of the original clause. We want to keep only clauses that
% entail any positive examples, so we try to prove L. The head of the
% parent clause of L has been instantiated to a positive example, so L,
% too, is instantiated accordingly, therefore prove(L) will only succeed
% if L is a literal in the refutation sequence of a positive example.
	L \= (_,_)
	,\+ clause_of(L,Ss)
	,prove(L)
	,unfold_literals(true,Ss,Is,(L,Acc),Bind).


%!	prove(?Literal) is nondet.
%
%	Vanilla Prolog meta-interpreter.
%
%	Why? Because we need to table this to ensure termination. The
%	alternative is to table every predicate symbol of every literal
%	we want to prove in unfold_literals/5. This is just more
%	convenient. It's weird though.
%
prove(true):-
	!.
prove((L,Ls)):-
	prove(L)
	,prove(Ls).
prove(L):-
	L \= (_,_)
	% Otherwise clause/2 raises errors.
	,\+ predicate_property(L,foreign)
	,\+ built_in_or_library_predicate(L)
	,clause(unfolding:L,Ls)
	,prove(Ls).
prove(L):-
	L \= (_,_)
	% No meta-interpretation for this literal.
	,(   predicate_property(L,foreign)
	 ;   built_in_or_library_predicate(L)
	 )
	,call(L).


%!	head_body(+Clause,+Literal,-Body) is det.
%
%	Auxiliary to unfold_literals/5.
%
%	Bind the head of a Clause to a Literal and return its Body.
%
%	The purpose of this is similar to the call to clause/2 in a
%	classic Prolog meta-interpreter: we want to recursively evaluate
%	the body literals of a clause, so for every body literal we find
%	its parent clause and replace the literal with the body literals
%	of the parent.
%
head_body(C,L,B):-
% Avoid binding variables in the clause to terms in the literal.
	copy_term(C,L:-B)
	,!.
head_body(A,L,true):-
	copy_term(A,L).


%!	theory_constants(+Clauses,-Signature) is det.
%
%	Collect constants in an Encapsulated program.
%
%	Clauses is a list of clauses of a program learned with dynamic
%	learning.
%
%	Signature is the list of constants of in all clauses in
%	Clauses.
%
%	Use this predicate to collect symbols and constants in a program
%	that must be excluded from variabilisation when unfolded clauses
%	are variabilised at the end of unfold_invented/3.
%
%	@tbd Note that only constants in clauses with a head and body
%	are collected. Atomic clauses should _not_ be lifted. Currently,
%	lifted_program/3 ignores atomic clauses which is a bug to solve
%	in that program. Once that bug is solved, this module will have
%	to separate atomic from other clauses and lift only the latter.
%
theory_constants(Ps,Ss):-
% Below, we collect each result in each findall/3 loop with a subsequent
% call to member/2 to avoid flattening the final list of constants after
% the outer loop exits. Flattening the list of all constants also
% flattens lists that are actually constants - we don't want that. Lists
% found as terms in a theory are constants of the theory, they should be
% kept intact.
%
% Note that the work below would look a lot better as a	recursive
% predicate but this is much easier to write and we'd have to call at
% least one member/2 anyway. So it looks awful. So sue me.
%
% Actually, it looks like a stairway to heaven. A bit.
%
% Outer loop: find all constants in all clauses.
%
	findall(Cn
	       ,(member(C,Ps)
		% We only want constants in definite clauses
		% Not unit clauses.
		,C = (_H:-_B)
		,clause_literals(C,Ls)
		% Median loop: find all constants in all literals
		,findall(Cn_
			,(member(L,Ls)
			 ,L =.. [_F|As]
			 % Inner loop: find all constants in one literal
			 ,findall(A
				 ,(member(A,As)
				  ,nonvar(A)
				  )
				 ,Cs)
			 ,member(Cn_,Cs)
			 )
			,CS)
		,member(Cn,CS)
		)
	       ,Ss_)
	,sort(Ss_,Ss).


%!	filter_targets(+Symbols,+Clauses,-Filtered) is det.
%
%	Filter a set of Clauses to keep only those of target predicates.
%
filter_targets(Ss,Ps,Ps_):-
	findall(C
	       ,(member(C,Ps)
		,head_body(C,H,_B)
		,functor(H,F,A)
		,target_or_invention(Ss,F/A)
		)
	       ,Ps_).



%!	pseudo_unfold_invented(+Program,+Targets,-Unfolded) is det.
%
%	As unfold_invented/3 but ignores positive examples.
%
%	This predicate generates all resolvents of the clauses of target
%	predicates in Program with the clauses of invented predicates
%	and returns the full set of such resolvents regardless of
%	whether such a clause entails a positive example or not.
%
%	unfold_invented/3 cannot be used as a stand-alone predicate,
%	without the elements of a MIL problem in the dynamic database.
%	This predicate addresses this limitation and can be useful for
%	testing, especially when trying to understand the behaviour of
%	unfold_invented/3, e.g. by observing the full set of resolvents
%	of clauses of target predicates and clauses of invented
%	predicates that can be generated regardless of whether they
%	entail any positive examples.
%
pseudo_unfold_invented(Ps,Ts,Us):-
	program_invented(Ps,Ts,Cs,Is)
	,invented_symbols_(Is,Ss)
	,!
	,pseudo_unfold_clauses(Cs,Ss,Is,[],Us_)
	,flatten(Us_,Us).
pseudo_unfold_invented(Ps,Ts,Ps):-
	program_invented(Ps,Ts,_Cs,[]).


%!	pseudo_unfold_clauses(+Clauses,+Symbols,+Invented,+Acc,-Unfolded) is
%!	det.
%
%	Unfold a set of Clauses with a set of Invented definitions.
%
%	Counterpart to unfold_clauses/5, but ignoring positive examples.
%
pseudo_unfold_clauses([],_Ss,_Is,Us,Us):-
	!.
pseudo_unfold_clauses([C|Cs],Ss,Is,Acc,Bind):-
	findall(C_
	       ,pseudo_unfold_clause(C,Ss,Is,C_)
	       ,Us)
	,pseudo_unfold_clauses(Cs,Ss,Is,[Us|Acc],Bind).


%!	pseudo_unfold_clause(+Clause,+Symbols,+Invented,-Unfolded) is nondet.
%
%	Auxiliary to pseudo_unfold_clauses/5, operating on a single Clause.
%
%	Counterpart to unfold_clause/4, but ignoring positive examples.
%
%	@tbd The only difference of this predicate and unfold_clause/4
%	is a missing call to clause/2 to instantiate the head of a
%	clause to a positive example. The alternatives are all fiddly
%	and would make the code harder to read so this is it.
%
pseudo_unfold_clause(H:-B,Ss,Is,H:-B_):-
	must_be(nonvar,H)
	,must_be(nonvar,B)
	,pseudo_unfold_literals(B,Ss,Is,(H),U_)
	,treeverse(U_,(H,B_)).


%!	pseudo_unfold_literals(+Literals,+Symbols,+Invented,+Acc,-Unfolded) is
%!	nondet.
%
%	Auxiliary to pseudo_unfold_clause/4, operating on each of its Literals.
%
%	Counterpart to unfold_literals/5, ignoring positive examples.
%
%	@tbd the only difference between this and unfold_literals/5 is a
%	single missing goal in their last clause. That's the goal that
%	calls a body literal before adding it to the set of body
%	literals in a clause. So this is pretty much copy/pasta but the
%	alternatives include all sorts of silly fiddly things to merge
%	the two programs that are only going to make the code harder to
%	read and understand. Let it be.
%
pseudo_unfold_literals(true,_Ss,_Is,Us,Us):-
	!.
pseudo_unfold_literals((L,Ls),Ss,Is,Acc,Bind):-
	pseudo_unfold_literals(L,Ss,Is,Acc,Acc1)
	,pseudo_unfold_literals(Ls,Ss,Is,Acc1,Bind).
pseudo_unfold_literals(L,Ss,Is,Acc,Bind):-
	L \= (_,_)
	,member(C,Is)
	,head_body(C,L,Ls)
	,pseudo_unfold_literals(Ls,Ss,Is,Acc,Bind).
pseudo_unfold_literals(L,Ss,Is,Acc,Bind):-
	L \= (_,_)
	,\+ clause_of(L,Ss)
	,pseudo_unfold_literals(true,Ss,Is,(L,Acc),Bind).



%!	reduce_unordered(+Program,-Reduced) is det.
%
%	Reduce a Program by removing duplicate sets of literals.
%
%	Program is a list of clauses, currently as returned by
%	unfold_invented/3.
%
%	Reduced is the list of unique clauses in Program. Clauses are
%	considered identical if they are equal up to a) renaming of
%	variables and b) ordering of literals.
%
%	Seen another way, this predicate considers a logic program as a
%	set of clauses and each clause as a set of literals, and forces
%	the input Program to be consistent with that definition. Sets
%	are unordered and have no duplicates, therefore all clauses that
%	are equal, up to ordering of literals are considered duplicates
%	by this predicate. Additionally, this predicate takes into
%	account unification between clauses (but _not_ subsumption, as
%	such) and removes all but one of a set of clauses that are equal
%	up to renaming of variables.
%
reduce_unordered(Ps,Us):-
	copy_term(Ps,Ps_)
	,examples_targets(Ps_,Ts)
	,maplist(clause_literals,Ps_,Ls)
	,maplist(sort,Ls,Ls_s)
	,maplist(numbervars,Ls_s)
	,sort(Ls_s,Ls_u)
	,findall(C
		,(member(Ls_i,Ls_u)
		 ,select(L,Ls_i,Bs)
		 ,mil_problem:symbol(L,S)
		 ,memberchk(S,Ts)
		 ,once(list_tree([L|Bs],(H,B)))
		 ,varnumbers((H:-B),C)
		 )
		,Us).
