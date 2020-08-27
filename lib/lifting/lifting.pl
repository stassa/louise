:-module(lifting, [lifted_program/2
		  ,lifted_program/3
		  ]).

/** <module> Predicates to replace terms with numbered variables.

*/

:-dynamic term_var/2.

/* Helps debugging numbered vars.
:- current_prolog_flag(debugger_write_options, X)
  ,set_prolog_flag(debugger_write_options,[numbervars(false)|X]).
*/

%!	lifted_program(+P, -L) is det.
%
%	Replace constants in clauses of P with variables.
%
%	Same as lifted_program(P,[],L).
%
%	Best explained with an example:
%	==
%	?- P = [q(a,b), r(b,a), (p(X,Y) :- q(X, Z), r(Z,Y))],
%	    lifting:lifted_program(P, P_),
%	    numbervars(P_),
%	    forall(member(C, P_),(write_canonical(C),nl)).
%
%	q('$VAR'(0),'$VAR'(1))
%	r('$VAR'(1),'$VAR'(0))
%	:-(p('$VAR'(2),'$VAR'(3)),','(q('$VAR'(2),'$VAR'(4)),r('$VAR'(4),'$VAR'(3))))
%	P = [q(a, b), r(b, a),  (p(C, D):-q(C, E), r(E, D))],
%	X = C,
%	Y = D,
%	Z = E,
%	P_ = [q(A, B), r(B, A),  (p(C, D):-q(C, E), r(E, D))].
%	==
%
%	Note that variables are also replaced with new variables.
%
%	@tbd This is the result of the use of varnumbers/2 to replace
%	skolemised terms with fresh variables. It might be preferrable
%	to leave existing variables untouched. That means writing a new
%	varnumbers/2 ensuring this.
%
lifted_program(P, P_):-
	lifted_clauses(-1, P, [], [], P1)
	,varnumbers(P1,P_)
	,retractall(term_var(_,_)).


%!	lifted_program(+Program,+Constants,-Lifted) is det.
%
%	Lift a program, except for a set of Constants.
%
%	As lifted_program/2 but excludes the constants in list Constants
%	from variabilisation.
%
lifted_program(Ps,Cs,Ps_):-
	lifted_clauses(-1,Ps,Cs,[],Ps_1)
	,varnumbers(Ps_1,Ps_)
	,retractall(term_var(_,_)).


%!	lifted_clauses(+I,+Cs,+Acc,-Bind) is det.
%
%	Business end of lifted_program/2.
%
%	Lifts each clause in the list of clauses Cs.
%
%	I is an index of the last variable number assigned to a term in
%	a clause in Cs, used to maintain consistency.
%
lifted_clauses(_, [], _Es, Cs, Cs_):-
	reverse(Cs, Cs_)
	,!.
lifted_clauses(I,[C|Cs],Es,Acc,Bind):-
	lifted_clause(I,J,C,Es,C_)
	,lifted_clauses(J,Cs,Es,[C_|Acc],Bind).


%!	lifted_clause(+I,-K,+C,-C_lifted) is det.
%
%	Business end of lifted_clauses/4.
%
%	Lifts each literal in clause C.
%
%	I is the last variable number assigned to a literal before the
%	first literal in C; K is the last variable number assigned to a
%	literal by this predicate.
%
lifted_clause(I, K, (H:-B), Es, (H_:-B_)):-
	!
	,lifted_literal(I,J,H,Es,H_)
	,lifted_literals(J,K,B,Es,(H_),Ls)
	,treeverse(Ls, (H_,B_)).
lifted_clause(I, J, (L), Es, L_):-
	lifted_literal(I, J, L, Es, L_).


%!	lifted_literals(+I,-K,+Ls,+Acc,-Bind) is det.
%
%	Auxiliary to lifted_clause/4.
%
%	The purpose of this is mainly to allow construction of the list
%	of lifted literals without excess use of reversing operations.
%
%	@tbd Basically, this exists because we want to initialise the
%	accumulator variable in lifted_clause/4 with the first literal
%	in a clause, instead of with an empty variable, because that
%	leaves an empty variable at the tail of the accumulator. It's
%	probably possible to do this in lifted_clause/4 itself and avoid
%	the complication of additional predicates.
%
lifted_literals(I, K, (L,Ls), Es, Acc, Bind):-
	!
	,lifted_literal(I, J, L, Es, L_)
	,lifted_literals(J, K, Ls, Es, (L_,Acc), Bind).
lifted_literals(I, J, (L), Es, Acc, (L_,Acc)):-
	lifted_literal(I, J, L, Es, L_).


%!	lifted_literal(+I, -J, +L, -Li) is det.
%
%	Lift a single literal.
%
%	I is the number of the last variable assigned to a term in a
%	literal before L. J is the number of the last variable assigned
%	to a term in L.
%
%	Examples:
%	==
%	?- lifting:lifted_literal(-1,J,p(a,b(c,X,d),e),Vs),write_canonical(Vs).
%	p('$VAR'(0),b('$VAR'(1),_,'$VAR'(2)),'$VAR'(3))
%	J = 2,
%	Vs = p(A, b(B, X, C), D).
%
%	?- lifting:lifted_literal(0, J, p(a,b(c,X,d),e), Vs),write_canonical(Vs).
%	p('$VAR'(1),b('$VAR'(2),_,'$VAR'(3)),'$VAR'(4))
%	J = 3,
%	Vs = p(B, b(C, X, D), E).
%	==
%
lifted_literal(I, J, L, Es, L_):-
	L =.. [F|As]
	,lifted_terms(I, J, As, Es, As_)
	,L_ =.. [F|As_].


%!	lifted_terms(+I, -J, +Ts, -Ls) is det.
%
%	Business end of lifted_literal/4.
%
%	Lift each term in a literal. Ts is a list of terms in the
%	argument list of a literal to be lifted. I, J index the last
%	numbered variables in previous literals and the parent literal
%	of Ts.
%
lifted_terms(I, J, As, Es, As_):-
	lifted_terms(I, J, As, Es, [], As_).


%!	lifted_terms(+I, -J, +Ts, +Acc, -Bind) is det.
%
%	Business end of lifted_terms/4.
%
%	Lifts each term in Ts by replacing it with a numbered variable,
%	making sure that the same number is associated with the same
%	term always. Ts is a list of terms in the argument list of a
%	literal to be lifted. I, J index the last terms numbered for
%	previous literals or the parent literal of Ts.
%
%	Example:
%	==
%	?- _P = [(enc(p,X,Y):-enc(q,Y,X)), enc(r,X,Z) ], lifted_program(_P, L).
%	X = B,
%	Y = C,
%	Z = F,
%	L = [(enc(A, B, C):-enc(D, C, B)), enc(E, B, F)].
%	==
%
lifted_terms(I, I, [], _Es, Ts, Ts_):-
	reverse(Ts, Ts_)
	,!.
lifted_terms(I, J, [T|Ts], Es, Acc, Bind):-
% T is a theory constant that must be excluded from variabilisation.
	memberchk(T,Es)
	,!
	,lifted_terms(I, J, Ts, Es, [T|Acc], Bind).
lifted_terms(I,J,[T|Ts],Es,Acc,Bind):-
% TODO: I'm not sure how this makes sense. It seems to fail when T
% is a compound term that is not '$VAR'(K) at which point it will
% fall over to the next clause and variabilise the compound recursively
% (which is correct). So this clause seems to only want to match
% previously assigned numbervar terms. In that case, can't we just do:
% lifted_terms(I,J,['$VAR'(K)|Ts],Acc,Bind)?
	compound(T)
	,\+ is_list(T)
	,T = '$VAR'(K)
	,!
	,lifted_terms(I,J,Ts,Es,['$VAR'(K)|Acc],Bind).
lifted_terms(I, J, [T|Ts], Es, Acc, Bind):-
% Checking T is a compound but not a list avoids variabilising list
% elements.
	compound(T)
	,\+ is_list(T)
	,!
	,T =.. [F|As]
	% Note the new variable - else we get an earlier value of J.
	,lifted_terms(I, K, As, Es, [], As_)
	,T_ =.. [F|As_]
	,lifted_terms(K, J, Ts, Es, [T_|Acc], Bind).
lifted_terms(I, J, [T|Ts], Es, Acc, Bind):-
	var(T)
	,increment(I,I_)
	,T = '$VAR'(I_)
	,!
	,lifted_terms(I_, J, Ts, Es, [T|Acc], Bind).
lifted_terms(I,J,[T|Ts],Es,Acc,Bind):-
	term_var(T, K)
	,!
	,lifted_terms(I, J, Ts, Es, ['$VAR'(K)|Acc], Bind).
lifted_terms(I, J, [T|Ts], Es, Acc, Bind):-
	increment(I, I_)
	,asserta(term_var(T, I_))
	,lifted_terms(I_, J, Ts, Es, ['$VAR'(I_)|Acc], Bind).


%!	increment(+I, -Ipp) is det.
%
%	Increment I by one.
%
%	Simple wrapper around succ/2. Because succ/2 doesn't deal with
%	negatives (and we want to start counting from -1 for numbered
%	vars).
%
increment(-1, 0):-
	!.
increment(I, I_):-
	succ(I, I_).



%!	treeverse(+Tree, Eert) is det.
%
%	Like reverse/2 but for trees.
%
%	Alternatively, a universe of trees. Free hugs. No squares
%	allowed.
%
treeverse((X,Xs),Ys):-
	treeverse(Xs,(X),Ys).

treeverse((X,Xs),Acc,Bind):-
	!
	,treeverse(Xs,(X,Acc),Bind).
treeverse(X,Ys,(X,Ys)).
