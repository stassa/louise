:-module(program_reduction, [reduction_report/3
			    ,reduction_report/1
			    ,program_reduction/3
			    ]).

:-expects_dialect(swi).

:-use_module(meta_interpreters).

/** <module> Predicates for reducing a set of clauses to a minimal equivalent set.

*/


%!	reduction_report(+Program,+Reduction,+Redundant) is det.
%
%	Report on the Reduction of a Program.
%
reduction_report(Ps,Rs,Ds):-
	writeln('Program clauses:')
	,writeln('----------------')
	,list_clauses(Ps)
	,nl
	,writeln('Program reduction:')
	,writeln('------------------')
        ,list_clauses(Rs)
	,nl
	,writeln('Redundant clauses:')
	,writeln('------------------')
        ,list_clauses(Ds)
	,nl.



%!	reduction_report(+Program) is det.
%
%       Report on redundant clauses in Program.
%
%       Top-level interface to program_reduction/3.
%
reduction_report(Ps):-
	program_reduction(Ps,Rs,Ds)
	,reduction_report(Ps,Rs,Ds).



%!	program_reduction(+Program,-Reduction,-Redundant) is det.
%
%	Calculates a Program's Reduction set.
%
program_reduction(Ps, Rs, Ds):-
	configuration:program_module(M)
	,assert_program(M,Ps)
	,program_reduction(M,Ps,[],Rs,[],Ds)
	% Uncomment to output reduction report.
	% TODO: replace with proper debugging.
	%,reduction_report(Ps_,Rs,Ds)
	% Cleanup database between runs.
	,retract_program(M, Ps).



%!	program_reduction(+Module,+Program,+Reduction,+Reduction,+Redundant,+Redundant)
%	is det.
%
%	Business end of program_reduction/3.
%
%	Implements Plotkin's program reduction algorithm:
%	Given a set of clauses H, find a reduction H' of H:
%	1) Set H' to H
%	2) Stop if every clause in H' is marked
%	3) Choose an unmarked clause, C, in H'
%	4a) If H' \{C} =< {C}, change H' to H' \ {C}
%	4b) Otherwise, mark C
%	5) Repeat from (2)
%
%	Clauses are marked by adding a term marked(C) in the database,
%	where C the marked clause.
%
%	The '=<' relation is the subsumption relation. This is tested
%	with the predicate subsumed/2.
%
%	At each step:
%	a) First an unmarked clause, C, is selected
%	b) Then the difference of the program reduction H' and C is
%	found.
%	c) C is retracted from the database.
%	d) subsumed/2 skolemises C, asserts its body goals to the
%	database and calls its head.
%	e1) If subsumed/2 is true, the program reduction list is bound
%	to the difference of H' and C and C is added to the lsit of
%	redundant clauses.
%	e2) Otherwise, C is asserted back to the database and marked,
%	while the reduction set and list of redundant clauses are left
%	unchanged.
%	f) In each step of the recursion, the reduction set is
%	inspected; if all clauses in it are marked, the predicate exits
%	with the current contents of the reduction set and the list of
%	redundant clauses.
%
program_reduction(_M, [], R, R_, D, D_):-
	reverse(R, R_)
	,reverse(D, D_)
	,!.
program_reduction(M, [C|Cs], R_acc, R_bind, D_acc, D_bind):-
	once(retract(M:C))
	,subsumed(M, C)
	,!
	,program_reduction(M, Cs, R_acc, R_bind, [C|D_acc], D_bind).
program_reduction(M, [C|Cs], R_acc, R_bind, D_acc, D_bind):-
	assert(M:C) % Re-asserting clause retracted earlier
	,program_reduction(M, Cs, [C|R_acc], R_bind, D_acc, D_bind).



%!	subsumed(+Module, +Clause) is det.
%
%	True when Clause is true in the context of Module.
%
%	The ugly thing about this predicate is that the truth or not of
%	Clause is determined according to the clauses of the program
%	(and their body goals) currently in Module, which we found out
%	by inspecting the dynamic database. So the question can't really
%	be answered "what is Clause subsumed by?". It would be a lot
%	nicer to pass the program clauses to subsumes/2 (as a third
%	argument, a lsit) and then evaluate Clause according to that
%	list- but that takes a bit of extra work (a meta-interpreter
%	that doesn't use the dynamic database to find predicates'
%	clauses).
%
%	... this remains to be done.
%
subsumed(M,C):-
	configuration:derivation_depth(D)
	,configuration:recursion_depth(R)
	,configuration:resolutions(S)
	,configuration:call_limit(L)
	,(   C = (H:-B)
	 ->  true
	 ;   (H:-B) = (C:-true)
	 )
	,copy_term((H:-B),(H_:-B_))
	,numbervars((H_:-B_))
	,once(assert_goals(M,B_))
	,(   configuration:meta_interpreter(solve_to_depth)
	 ->  G = solve_to_depth(M,H_,D, S)
	 ;   configuration:meta_interpreter(solve_to_limit)
	 ->  G = solve_to_limit(M,H_,D,R)
	 ;   configuration:meta_interpreter(solve)
	 ->  G = solve(M, H_)
	 ;   configuration:meta_interpreter(call)
	 ->  G = call(M:H_)
	 ;   configuration:meta_interpreter(MI)
	    ,retract_goals(M, B_)
	    ,throw('Unknown meta-interpreter':MI)
	 )
	,limited_subsumed(L, M, G, B_).


%!	limited_subsumed(+Limit,+Module,+Goal,+Body) is det.
%
%	Business end of subsumed/2.
%
%	Clause are selected according to the value of Limit- the
%	Swi-Prolog call/2 variant used to limit processinb by recursion
%	depth, number of inferences or time, as determined by
%	call_limit/1.
%
limited_subsumed(none, M, G, B):-
	!
       ,(   G
	->  retract_goals(M, B)
	;   retract_goals(M, B)
	   ,!
	   ,fail
	 ).

limited_subsumed(time, M, G, B):-
	!
	,configuration:time_limit(L)
	,Wr = call_with_time_limit(L, G)
	% If the time-limited call succeeds before hitting
	% the time limit the first branch is true; if the
	% time-limited call fails, or does not succeed before
	% hitting the time limit, then the second branch is true.
	,(   catch(Wr,time_limit_exceeded,false)
	->   retract_goals(M, B)
	    %,format('Subsumed: ~w~n',[(H_:-B_)])
	 ;   retract_goals(M, B)
	    %,format('Not subsumed: ~w~n',[(H_:-B_)])
	    ,!
	    ,fail
	 ).

limited_subsumed(L, M, G, B):-
	configuration:depth_limit(D)
	,configuration:inference_limit(I)
	,(   L == depth
	 ->  Wr = call_with_depth_limit(G, D, R)
	    ,R_ = depth_limit_exceeded
	 ;   L == inference
	 ->  Wr = call_with_inference_limit(G, I, R)
	    , R_ = inference_limit_exceeded
	 ;   throw('Unknown limit':L)
	)
	% If the goal succeeds and limit is not exceeded,
	% first branch is true; if goal fails or if limit is
	% exceeded, second branch is true.
	,(   Wr
	    ,R \= R_
	->   retract_goals(M, B)
	    %,format('Subsumed: ~w after ~w recursions~n',[(H_:-B_),R])
	 ;   retract_goals(M, B)
	    %,format('Not subsumed: ~w~n',[(H_:-B_)])
	    ,!
	    ,fail
	 ).


%!	assert_program(+Module, +Program) is det.
%
%	Assert a set of clauses into a Module.
%
assert_program(M,P):-
	forall(member(C, P)
	      ,assert(M:C
		     )
	      ).


%!	retract_program(+Module,+Program) is det.
%
%	Retract	all clauses of a Program from a Module.
%
retract_program(M,P):-
	forall(member(C, P)
	      ,(( retract(M:C)
		; true
		)
	       )
	      ).


%!	assert_goals(+Module, +Goals) is det.
%
%	Add a set of Goals to a Module.
%
assert_goals(M,(Atom,Atoms)) :-
	(   predicate_property(M:Atom, built_in)
	->  true
	;   asserta(M:Atom)
	),
	assert_goals(M,Atoms).
assert_goals(M,Atom):-
	(   predicate_property(M:Atom, built_in)
	->  true
	;   asserta(M:Atom)
	).


%!	retract_goals(+Module, +Literals) is det.
%
%	Retract each of a clause's Literals from Module.
%
retract_goals(M,(Atom,Atoms)) :-
	(   predicate_property(M:Atom, built_in)
	->  true
	;   retract(M:Atom)
	),
	retract_goals(M,Atoms).
retract_goals(M,Atom):-
	(   predicate_property(M:Atom, built_in)
	->  true
	;   retract(M:Atom)
	).


%!	list_clauses(+Clauses) is det.
%
%	Print out a list of Clauses.
%
%	Variables in Clauses are numbered with numbervar, for
%	legibility.
%
list_clauses(Cs):-
	forall(member(C, Cs)
	      ,(duplicate_term(C,C_)
	       ,numbervars((C_),0,_)
	       ,writeln(C_))
	      ).
