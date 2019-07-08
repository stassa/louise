:-module(meta_interpreters, [solve/2
			    ,solve_to_depth/4
			    ,solve_to_limit/4
			    ]).

/** <module> Prolog interpreters in Prolog.

*/

%!	solve(+Module, +Clause) is det.
%
%	True when Clause is true in Module.
%
solve(_M, true):-
	!.
solve(M, (L1,Ls)):-
	solve(M, L1)
	,!
	,solve(M, Ls).
solve(M, (L1)):-
	clause(M:L1, B)
	,!
	,solve(M, B).



%!	solve_to_depth(+Module,+Clause,+Derivations,+Resolutions) is
%!	det.
%
%	True when Clause is True in Module.
%
%	Depth is the maximum derivation depth to pursue. Resolutions is
%	the maximum number of resolutions allowed during the proof of a
%	single literal.
%
%	solve_to_depth/[4,5] implement an iterative-deepening solver,
%	although a single search "iteration" is performed - the search
%	reaches the specified depth, then backtracks to find sibling
%	branches, then fails finally when no more backtracking is
%	possible, without attempting to increase the depth for a new
%	iteration.
%
%	In practice, the Resolution limit is used to limit uncontrolled
%	backtracking into clause/2, which happens everytime the
%	derivation length is hit and can cause the solver to go
%	infinite (by bouncing off the same clause/2 result forever).
%
solve_to_depth(M, Cs, D, R):-
	G = g(0)
	,solve_to_depth(M, Cs, 0, D, G, R).


%!	solve_to_depth(+Module,+Clause,+Di,+Depth,+Resolutions) is det.
%
%	Business end of solve_to_depth/4.
%
solve_to_depth(_M, true, _, _, _, _):-
	!.
solve_to_depth(_M, _, D, D, _, _):-
        !
       ,fail.
solve_to_depth(M, (L1,L2), Di, D, G, R):-
	! % Avoid binding (L1,L2) in next clause
	,solve_to_depth(M, L1, Di, D, G, R)
	,solve_to_depth(M, L2, Di, D, G, R).
solve_to_depth(M, (L1), Di, D, G, R):-
	%empty_nb_set(S)
	%clause(M:L1, B)
	guarded_clause(M:L1, B, G, R)
	%,add_nb_set(B, S, true)
	,succ(Di, Dk)
	% Try using peano numbers instead of arithmetic.
	,solve_to_depth(M, B, Dk, D, G, R).


%!	guarded_clause(+Head, -Body, +Guard, +Resolutions) is det.
%
%	Find a Body literal for	Head.
%
%	Resolutions is the number of times this can backtrack into
%	clause/2. Guard is the term used to keep track of this number
%	using non-backtrackable, destructive assignment.
%
guarded_clause(M:L1,true,G,R):-
% Attempting to call clause/2 on a built-in raise errors.
% So, we hand over to Prolog and let it do its thing.
	predicate_property(L1, built_in)
	,!
	,call(M:L1)
	,arg(1, G, I)
	,I < R
	,succ(I, I_)
	,nb_setarg(1, G, I_).
guarded_clause(M:L1, B, G, R):-
	clause(M:L1, B)
	,arg(1, G, I)
	,I < R
	,succ(I, I_)
	,nb_setarg(1, G, I_).



%!	solve_to_limit(+Module, +Clause, +Depth, +Length) is det.
%
%	True when Clause is True in Module.
%
%	Depth is the maximum recursion depth at which to follow a goal
%	around. Length is the maximum length of a proof, i.e. the number
%	of successive successful goal proofs.
%
%	When Depth or Length are exceeded, solving backtracks to a
%	sibling branch of the proof tree.
%
%
solve_to_limit(M, Cs, I, K):-
	solve_to_limit(M, Cs, 0, I, 0, K).

% Erased a clause.
solve_to_limit(_M, true, _, _, _, _):-
	!.
% Hit recursion limit.
solve_to_limit(_M, _, _, _, K, K):-
	fail.
% Hit derivation limit.
solve_to_limit(_M, _, D, D, _, _):-
	!
       ,fail.
solve_to_limit(M, (L1,L2), Di, D, Ki, K):-
	! % Avoid binding (L1,L2) in next clause
	,succ(Ki, Kj)
	,solve_to_limit(M, L1, Di, D, Kj, K)
	,succ(Kj, Kk)
	,solve_to_limit(M, L2, Di, D, Kk, K).
solve_to_limit(M, (L1), Di, D, Ki, K):-
	clause(M:L1, B)
	,succ(Di, Dk)
	,succ(Ki, Kj)
	,solve_to_limit(M, B, Dk, D, Kj, K).
