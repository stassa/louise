:-module(clause_reduction, [clause_reductions/2
			   ,clause_reduction/2
			   ,theta/3
			   ,clauses_literals/2
			   ,literals_clauses/2]).

/** <module> Implementation of Plotkin's clause reduction algorithm.

*/

%!	clause_reductions(+Clauses,-Reductions) is det.
%
%	Reduce each of a list of Clauses.
%
%	Note that this is not program reduction. Each clause in Clauses
%	is processed using clause_reduction/2, then the reduced clause
%	are bound to the list Clauses.
%
clause_reductions(Cs, Ds_):-
	clauses_literals(Cs,Ls)
	,findall(D
	       ,(member(C, Ls)
		,clause_reduction(C,D)
		)
	       ,Ds)
	,literals_clauses(Ds, Ds_).


%!	clause_reduction(+Clause, -Reduction) is det.
%
%	Reduction is equivalent to Clause, but smaller.
%
%	Expects Clause to be a list of literals (not a :-/2 term).
%
clause_reduction(C, D):-
	msort(C, C_s)
	% Sort without removing duplicates
	% to search more efficiently
	,theta(C_s, Cth)
	% Sort to remove duplicates
	,sort(Cth, D).


%!	theta(+Clause, -Clause_theta) is det.
%
%	Apply a substitution to a Clause.
%
theta(D, D_):-
	theta(D, [], D_).


%!	theta(+Clause, +Acc, -Bind) is det.
%
%	Business end of theta/2.
%
%	Bind is the result of finding and applying a substitution to
%	each literal in a Clause, until no more substitutions are
%	possible.
%
%	In each step, we select two literals L1 and L2, in the clause,
%	C. If these unify, we continue with C U {L1}; otherwise, we
%	split off search to a branch for C U {L1} and another for C U
%	{L2}.
%
%	When a single literal is left int the clause, we add it to the
%	Accumulator.
%
%	When no more literals can be tested, we exit with the contents
%	of the Accumulator bound to the output-variable, Bind.
%
%	The result should be a set of literals equivalent to the
%	original clause, C, but smaller- except our process will
%	introduce some duplicate literals, that will need to be
%	sorted-out at a later step (this is done in clause_reduction/2).
%
theta([], D, D):-
	!.
theta([L], Acc, [L|Acc]):-
	!.
theta([L,L|Ls], Acc, Bind):-
	!
	,theta([L|Ls], Acc, Bind).
theta([L1,L2|Ls], Acc, Bind):-
	theta([L2|Ls], Acc, Acc0)
	,!
	,theta([L1|Ls], Acc0, Bind).

/*
The following are the two examples from Nienhuys-Cheng and de Wolf:

?- clause_reduction([p(X, X), p(X, X1), p(Y, Z)], C_).
X = X1, X1 = Y, Y = Z,
C_ = [p(Z, Z)].

?- clause_reduction([q(Y, f(X)), p(X), q(Y, f(Z)), q(a, f(X))], C_).
Y = a,
X = Z,
C_ = [p(Z), q(a, f(Z))].

The substitutions in the example are somewhat different- e.g. in the
first case, the substitition is {X1/X, Z/X, Y/X} so that the reduced
clause is {p(X,X)} rather than {p(Z,Z)}. I think this is a quirk of the
Swi-Prolog unification operator (the first rather than the last unifying
variable is substituted). In any case I don't think it should make any
difference- p(X, X) is equivalent to p(Z, Z). Constants also seem to be
substituted correctly.
*/



%!	clauses_literals(+Clauses, -Literals) is det.
%
%	Convert a list of Clauses to a list of lists of Literals.
%
clauses_literals(Hs, Ls):-
	findall(Cls
	       ,(member(C, Hs)
		,clause_literals(C, Cls)
		)
	       ,Ls).



%!	clause_litearls(+Clause, -Literals) is det.
%
%	Convert a Clause to a list of Literals.
%
clause_literals('()', []):-
	!.
clause_literals(Ts, Ls):-
	phrase(list_tree(Ts), Ls)
	,!.

%!	list_tree(?Tree) is nondet.
%
%	Business end of clause_literals/2.
%
list_tree((T,Ts)) --> [T], list_tree(Ts).
list_tree((T:-Ts)) --> [T], list_tree(Ts).
list_tree(T) --> [T].


%!	literals_clauses(+Literals, -Clauses) is det.
%
%	Convert a list of lists of Literals to a list of Clauses.
%
literals_clauses(Ls, Cs):-
	findall(C
	       ,(member(L, Ls)
		,literals_clause(L, C)
		)
	       ,Cs).

literals_clause([L], (L:-true)):-
	!.
literals_clause([H|B], :-(H,Bt)):-
	once(phrase(list_tree(Bt), B)).









