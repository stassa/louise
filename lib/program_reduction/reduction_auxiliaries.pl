:-module(auxiliaries, [predicate_clauses/3
		      ,predicate_clauses/2
		      ,print_settings/0
		      ,reduce_tautologies/2
		      ,tautology/1
		      ,left_recursive/1
		      ,all_bound/2
		      ,bind_unbound/2
		      ]).

/** <module> Predicates to detect unbounded recursion, tautologies.

*/


%!	predicate_clauses(+Module, +Predicate_indicator, -Clauses) is
%!	det.
%
%	Find all Clauses of a program in a given Module.
%
%	Predicate_indicator should be a term F/A where F the functor and
%	A the arity of the predicate.
%
predicate_clauses(M, F/A, Cs):-
	functor(T, F, A)
	,findall((T :- B)
		,clause(M:T, B)
		,Cs).


%!	predicate_clauses(+Predicate_indicator, -Clauses) is det.
%
%	Find all Clauses of a program.
%
%	Predicate_indicator should be a term F/A where F the functor and
%	A the arity of the predicate.
%
predicate_clauses(F/A, Cs):-
	functor(T, F, A)
	,findall((T :- B)
		,clause(T, B)
		,Cs).


%!	print_settings is det.
%
%	Print out current configuration settings.
%
print_settings:-
	Cs = [call_limit(_)
	     ,depth_limit(_)
	     ,derivation_depth(_)
	     ,inference_limit(_)
	     ,meta_interpreter(_)
	     ,program_module(_)
	     ,recursion_depth(_)
	     ,resolutions(_)
	     ,time_limit(_)
	     ]
	,forall(member(C, Cs)
	       ,(C
		,C =.. Ct
		,format('~w: ~w~n', Ct)
		)
	       ).


%!	reduce_tautologies(+Program,-Reduced) is det.
%
%	Detect tautologies in Program and remove them.
%
reduce_tautologies(P, P_):-
	findall(C
	       ,(member(C, P)
		,\+ tautology(C)
		)
	       ,P_).



%!	tautology(+Clause) is det.
%
%	True when Clause is a tautology.
%
tautology((H:-B)):-
	copy_term((H:-B), (H_:-B_))
	,numbervars(B_,0,_)
	,tautology(H_,B_).

%!	tautology(+Head, +Body) is det.
%
%	Business end of tautology/1.
%
%	Body is a set of skolemised literals. tautology/2 is true when
%	each atom in Body binds to Head (a new version with fresh
%	variables for each Body literal).
%
tautology(H, (B,Bs)):-
	!
	,copy_term(H, H_)
	,H_ = B
	,tautology(H, Bs).
tautology(H, B):-
	copy_term(H, H_)
	,H_ = B.



%!	left_recursive(+Clause) is det.
%
%	True when Clause is left-recursive.
%
%	Clause should be a Horn clause of the form (H:-(B1,Bs)). If the
%	first body literal, B1 of Clause, when Skolemised unifis to the
%	head of Clause, then it's left-recursive.
%
left_recursive((H:-(B1,_))):-
	copy_term(H, H_)
	,copy_term(B1,B1_)
	,numbervars(B1_, 0, _)
	,B1_ = H_
	,!.
left_recursive((H:-(B1))):-
	copy_term(H, H_)
	,copy_term(B1,B1_)
	,numbervars(B1_, 0, _)
	,B1_ = H_.



%!	all_bound(+Clauses, -Corrected) is det.
%
%	Test a set of Clauses for unbound recursion and correct it.
%
all_bound(Cs, Cs_):-
	findall(C_
	       ,(member(C,Cs)
		,bind_unbound(C,C_)
		)
	       ,Cs_).



%!	bind_unbound(+Clause,-Corrected) is det.
%
%	Test a Clause for left-recursion and correct it if true.
%
%	Clause should be a Horn clause of the form (H:-(B1,Bs)). If the
%	first body literal, B1 of Clause can be skolemised and unify to
%	the head of Clause, Corrected is bound to the term:
%	==
%	(H :-(clause(B1,true), Bs))
%	==
%
%	This way, Clause is only true when there is a unit clause
%	B1:-true in the program database, which should eliminate
%	(some?) left recursions.
%
bind_unbound((H:-(B1,Bs)), (H:-(Bi,Bs))):-
	copy_term(H, H_)
	,copy_term(B1,B1_)
	,numbervars(B1_, 0, _)
	,B1_ = H_
	,Bi = clause(B1, true)
	,!.
bind_unbound((H:-(B1)), (H:-(Bi))):-
	copy_term(H, H_)
	,copy_term(B1,B1_)
	,numbervars(B1_, 0, _)
	,B1_ = H_
	,Bi = clause(B1, true)
	,!.
bind_unbound(P, P).
