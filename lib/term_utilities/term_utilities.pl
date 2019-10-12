:-module(term_utilities,[herbrand_universe/2
			,predicate_atoms/3
			,predicate_atoms/2
			,top_goal/2
			,random_term/4
			,random_args/3
			,random_args/4
			,random_atom/2
			,extend_term/3
			,clauses_literals/2
			,clause_literals/2
			,list_tree/2
			,treeverse/2
			,treeppend/3
			]).

/** <modules> Prolog term inspection and manipulation.
*/


%!	herbrand_universe(+Terms, -Base) is det.
%
%	Collect constants from a list of Terms.
%
herbrand_universe(Ts,Bs):-
	findall(As
	       ,(member(T,Ts)
		,T =.. [_|As]
		)
	       ,AS)
	,flatten(AS,AS_flat)
	,sort(AS_flat, Bs).



%!	predicate_atoms(+Module,+Predicate,-Atoms) is det.
%
%	Collect all Atoms of a Predicate declared in a Module.
%
%	Predicate is a compound term, F/A, where F the functor and A the
%	arity of a predicate of interest. Atoms is a list of all atoms
%	of F/A currently loaded in the program database.
%
%	Raises existence error if Predicate does not have a definition.
%	I mean, duh.
%
predicate_atoms(M,F/A,As):-
	functor(T,F,A)
	,findall(T
		,M:call(T)
		,As).



%!	predicate_atoms(+Predicate,-Atoms) is det.
%
%	Collect all Atoms of a Predicate.
%
%	Predicate is a compound term, F/A, where F the functor and A the
%	arity of a predicate of interest. Atoms is a list of all atoms
%	of F/A currently loaded in the program database.
%
%	Raises existence error if Predicate does not have a definition.
%	I mean, duh.
%
predicate_atoms(F/A,As):-
	functor(T,F,A)
	,findall(T
		,T
		,As).



%!	top_goal(+Program,-Goal) is det.
%
%	True when Goal is the top-level goal in Program.
%
%	Use this to extract the target concept from a learned
%	hypothesis, when this is not otherwise easy to obtain (e.g. from
%	a top-level query).
%
%	Goal is a predicate indicator, F/A.
%
top_goal(Hs,F/A):-
	Hs = [H:-_|_]
	,functor(H,F,A).



%!	random_term(+Min,+Max,+Arity,-Term) is det.
%
%	Generate a Term with the given Arity and a random functor.
%
%	Min, Max are the minimum and maximum length of the generated
%	functor.
%
random_term(I,K,A,B):-
	random_between(I,K,J)
	,random_atom(J,F)
	,functor(B,F,A).



%!	random_args(+Base,+N,-Arguments) is det.
%
%	Randomly choose N terms from the Herbrand Base.
%
%	Base is a list of atomic terms, the Herbrand Base for some
%	universe of discourse. N is the (exact) number of terms required
%	(this should match the arity of a term to be populated by the
%	selected Arguments). Arguments is a list of N terms randomly
%	selected from Base.
%
%	Used in background_noise/6 to randomly instantiate randomly
%	generated terms with elements from a given Herbrand base.
%
random_args(Cs,N,As):-
	findall(C
	       ,(between(1,N,_)
		,random_member(C,Cs)
		)
	       ,As).



%!	random_args(+Min,+Max,+N,-Constants) is det.
%
%	Randomly generate N atomic Constants.
%
%	Min, Max are the minimum and maximum length of the atoms to be
%	generated. N is the number of constants in the output list,
%	Constatns. Constants is a list of those randomly generated
%	atoms.
%
random_args(I,K,N,Cs):-
	findall(C
	       ,(between(1,N,_)
		 ,random_between(I,K,M)
		 ,random_atom(M,C)
		 )
	       ,Cs).



%!	random_atom(+Length,-Atom) is det.
%
%	Generate a random alphabetic atom of the given length.
%
random_atom(K,F):-
	findall(C
	       ,(between(1,K,_)
		,random_between(97,122,C)
		)
	       ,Cs)
	,atom_codes(F,Cs).



%!	extend_term(+K,+Term,-Extended) is det.
%
%	Extend a term by K times its original arity.
%
%	K is an integer. Term is a compound term with functor F and
%	arity N. Extended is a new term with functor F, arity K * N, its
%	first N arguments bound to the N arguments of Term and the
%	remaining arguments unbound.
%
extend_term(K, T1, T3):-
	functor(T1, F, A)
	,A_ is A * K
	,functor(T2,F,A_)
	,T1 =.. [F|As_1]
	,T2 =.. [F|As_2]
	,diff_list(As_1,Df_1,Tl_1)
	,diff_list(As_2,Df_2,Tl_2)
	,diff_append(Df_1-Tl_1,Df_2-Tl_2,Df_3-Tl_2)
	,Tl_2 = []
	,T3 =.. [F|Df_3].


%!	diff_append(T1, T2, T3) is det.
%
%	Append for difference lists.
%
diff_append(A-B, B-T, A-T).


%!	diff_list(+List,-Difference,-Tail) is det.
%
%	Convert a List to a Difference list with a Tail.
%
diff_list(L, Diff, Tail):-
	once(phrase(diff_list_0(L), Diff, Tail)).

%!	diff_list_0 is det.
%
%	Business end of diff_list/3.
%
diff_list_0(L) --> L.



%!	clauses_literals(+Clauses, -Literals) is det.
%
%	Convert a list of Clauses to a list of Literals.
%
clauses_literals(Hs, Ls):-
	findall(Cls
	       ,(member(C, Hs)
		,clause_literals(C, Cls)
		)
	       ,Ls_)
	,flatten(Ls_, Ls).


%!	clause_litearls(+Clause, -Literals) is det.
%
%	Convert a Clause to a list of Literals.
%
clause_literals('()', []):-
	!.
clause_literals(Ts, Ls):-
	list_tree(Ls, Ts)
	,!.



%!	list_tree(?List, ?Tree) is det.
%
%	Convert between a list and a Prolog binary tree.
%
%	Tree is a Prolog term (L1,...,Ln). A binary tree, yes?
%
list_tree(Ls, Ts):-
	phrase(list_tree(Ts), Ls).


%!	list_tree(?Tree) is nondet.
%
%	Business end of clause_literals/2.
%
list_tree((T,Ts)) --> [T], list_tree(Ts).
list_tree((T:-Ts)) --> [T], list_tree(Ts).
list_tree(T) --> [T].



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



%!	treeppend(?Tree1,?Tree2,?Tree3) is nondet.
%
%	Like append/3 but for trees.
%
%
treeppend((X,Xs),Ys,(X,Zs)):-
	treeppend(Xs,Ys,Zs).
treeppend(X,Ys,(X,Ys)):-
	X \= (_,_).
