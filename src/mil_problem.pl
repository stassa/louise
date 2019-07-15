:-module(mil_problem, [expanded_metarules/2
		      ,metarule_expansion/2
		      ,extended_metarules/2
		      ,metarule_extension/3
		      ,encapsulated_problem/5
		      ,encapsulated_bk/2
		      ,predicate_signature/3
		      ,examples_target/2
		      ,encapsulated_clauses/2
		      ,unfolded_metasubs/2
		      ,metarule_projection/2
		      ,excapsulated_clauses/3
		      ]).

:-use_module(configuration).

/** <module> Predicates to transform a MIL problem.

*/

%!	expanded_metarules(+Ids,-Encapsulated) is det.
%
%	Encapsulate a set of metarules.
%
%	Metarules is a list of metarule definitions to be expanded by
%	metarule_expansion/2.
%
%	If Ids is a list of metarule names, only definitions of the
%	named metarules are bound to Metarules.
%
%	If Ids is a free variable, it is bound to a list of the names of
%	all Metarules known to the system.
%
expanded_metarules(Ids,Ms):-
	var(Ids)
	,!
	,findall(Id-M
	       ,metarule_expansion(Id,M)
	       ,Ids_Ms)
	,pairs_keys_values(Ids_Ms,Ids,Ms).
expanded_metarules(Ids,Ms):-
	is_list(Ids)
	,findall(M
	       ,(member(Id,Ids)
		,metarule_expansion(Id,M)
		)
	       ,Ms).



%!	metarule_expansion(?Id,-Metarule) is nondet.
%
%	Expand a Metarule with the given Id.
%
%	Expansion adds a vector of Prolog terms s(P), s(Q),... s(V) to
%	the body of the metarule, wrapping the predicate symbols. This
%	is to constraint the search for bindings of existentially
%	quantified variables to predicates in the predicate signature.
%
%	@tbd This will not allow the use of existentially quantified
%	terms that are not predicate symbols, but hypothesis constants.
%
metarule_expansion(Id,Mh_:-(Es_,Mb)):-
	configuration:current_predicate(metarule,Mh)
	,Mh =.. [metarule,Id|Ps]
	,Mh_ =.. [m,Id|Ps]
	,clause(Mh,Mb)
	,maplist(existential_variables,Ps,Ps_)
	,once(list_tree(Ps_,Es_)).

%!	existential_variables(+Variable, -Encapsulated) is det.
%
%	Encapsulate an existentially quantified Variable in a metarule.
%
%	Wrapper around =../2 to allow it to be passed to maplist/3.
%
existential_variables(Ls,L_):-
	L_ =.. [s,Ls].



%!	extended_metarules(+Metarules,-Extended) is det.
%
%	Extend a set of Metarules.
%
%	@tbd This is an expensive predicate. First, it collects lists of
%	metarules, two expanded metarules and their extension, as
%	key-value pairs, where the keys are the names of the metarules;
%	then, it flattens this list and sorts it to remove elements with
%	duplicate keys; finally, it splits the resulting sorted list to
%	keys and values lists and returns the values. The reason for all
%	this is the usual problem with sorting definite clauses with
%	variables: terms that are identical up to renaming of variables
%	appear different to sort/4 because of the "age" of variables
%	(when they were created). Rather than implementing a sorting
%	procedure that ignores variable age, it's much simpler to do
%	what is done here. The high cost is mitigated by the fact that
%	this predicate only needs to be called once at the start of
%	learning.
%
extended_metarules(MS,Es):-
	findall([M1-M1_,M2-M2_,N-M]
	       ,(member(M1,MS)
		,member(M2,MS)
		,metarule_expansion(M1,M1_)
		,metarule_expansion(M2,M2_)
		,metarule_extension(M1_,M2_,M)
		,M = (H:-_B)
		,H =.. [m,N|_As]
		)
	       ,Es_)
	,flatten(Es_,Es_f)
	,sort(1,@<,Es_f,Es_s)
	,pairs_keys_values(Es_s,_Ns,Es).



%!	metarule_extension(+Metarule1,+Metarule2,-Metarule3) is det.
%
%	Extend two metarules by unfolding them on each other.
%
%	Exending two metarules results in a new metarule with the head
%	literal of the first metarule and the body literals of both
%	minus the literal used to unfold one metarule on the other.
%
%	Unfolding two metarules involves unifying the last body literal
%	of one metarule with the head literal of the second metarule and
%	then concatenating their body literals minus the body literal
%	unified with the head literal of the second metarule.
%
metarule_extension((M1:-Ss1,B1),(M2:-Ss2,B2),(M3:-Ss3,B3)):-
	metasubs_extension(M1,M2,M3)
	,unfold(B1,B2,B3)
	,signature_extension(Ss1,Ss2,Ss3).


%!	metasubs_extension(+Metasub1,+Metasub2,+Metasub3) is det.
%
%	Create a metasubstitution for a metarule extension.
%
metasubs_extension(M1,M2,M3):-
	M1 =.. [m,N1|Ps1]
	,M2 =.. [m,N2|Ps2]
	,atomic_list_concat([N1,N2],'_',N3)
	,list_merge(Ps1,Ps2,Ps3)
	,M3 =.. [m,N3|Ps3].


%!	list_merge(+Xs,+Ys,-Zs) is det.
%
%	Merge two lists.
%
%	Merging here means that we discard the element at the end of the
%	first list, the element at the start of the second list, and
%	append the remaining elements of the two lists.
%
list_merge([_],[_|Xs],Xs).
list_merge([X|Xs],Ys,[X|Zs]):-
	list_merge(Xs,Ys,Zs).


%!	signature_extension(+Sig1, +Sig2, -Sig3) is det.
%
%	Construct the signature of a metarule extension.
%
signature_extension(Ss1,(_,Ss2),Ss3):-
	tree_merge(Ss1,Ss2,Ss3).


%!	tree_merge(+Xs,+Ys,-Zs) is det.
%
%	Tree version of merge/3.
%
%	Used to merge (in the sense of merge/3) the signature terms in
%	two metarules extending each other.
%
tree_merge((X,Xs),Ys,(X,Zs)):-
	!
	,tree_merge(Xs,Ys,Zs).
tree_merge((L),Xs,Xs):-
	L \== (_,_).


%!	unfold(+Tree1,+Tree2,-Unfolded) is det.
%
%	Unfold two trees on one another.
%
%	Unfolding here means that we find the last literal of Tree1, the
%	first literal of Tree2, unify them and return the concatenation
%	of remaining literals in both trees.
%
unfold((X,Xs),Ys,(X,Zs)):-
	unfold(Xs,Ys,Zs).
unfold((X),(X,Xs),Xs):-
	X \== (_,_).



%!	encapsulated_problem(+Pos,+Neg,+BK,+MS,-Ps)
%!	is det.
%
%	Encapsualte a MIL problem.
%
%	Pos and Neg are lists of example atoms; Pos are positive
%	examples and Neg are negative examples, of the form :-E, where E
%	an atom.
%
%	BK is a list of predicate symbols and arities of BK predicates.
%
%	Metarules is a list of constants, the names of metarules in the
%	problem.
%
%	Ps is a list [Pos_, Neg_, BK_, MS_, Ss] where elements are the
%	encapsulations of the positive and negative examples, BK
%	definitions, and Metarules, and the predicate signature,
%	respectively.
%
%	@tbd Encapsulated forms need documentation.
%
encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_,Ss]):-
	configuration:extend_metarules(E)
	,encapsulated_bk(BK,BK_)
	,(   E == true
	 ->  extended_metarules(MS, MS_)
	 ;   expanded_metarules(MS,MS_)
	 )
	,encapsulated_clauses(Pos,Pos_)
	,encapsulated_clauses(Neg,Neg_)
	,predicate_signature(Pos,BK,Ss).



%!	encapsulated_bk(+Background,-Encapsulated) is det.
%
%	Encapsulate a list of Background definitions.
%
encapsulated_bk(BK,BK_flat):-
	findall(Cs_
	       ,(member(P, BK)
		,program(P,user,Cs)
		,encapsulated_clauses(Cs,Cs_)
		)
	       ,BK_)
	,flatten(BK_, BK_flat).



%!	predicate_signature(+Examples,+BK,-Signature) is det.
%
%	Find the predicate Signature for a problem.
%
predicate_signature(Es,BK,[s(T)|Ps]):-
	findall(s(F)
	       ,member(F/_,BK)
	       ,Ps)
	,examples_target(Es,T/_).



%!	examples_target(+Examples,-Target) is det.
%
%	Extract the symbol and arity from Examples of a Target.
%
examples_target([E|_Es],F/A):-
	E =.. [m,F|As]
	,!
	,length(As,A).
examples_target([E|_Es],F/A):-
	functor(E,F,A).



%!	encapsulated_clauses(+Clauses, -Encapsulated) is det.
%
%	Encapsulate a list of Clauses.
%
encapsulated_clauses(Cs,Cs_):-
	encapsulated_clauses(Cs, [], Cs_).

%!	encapsulated_clauses(+Clauses,+Acc,-Encapsulated) is det.
%
%	Business end of encapsulated_clauses/2.
%
encapsulated_clauses([],Acc,Cs):-
	reverse(Acc, Cs)
	,!.
encapsulated_clauses([C|Cs], Acc, Bind):-
	encapsulated_clause(C,C_)
	,encapsulated_clauses(Cs,[C_|Acc],Bind).


%!	encapsulated_clause(+Clause, -Encapsulated) is det.
%
%	Encapsulate a Clause.
%
encapsulated_clause(C, C_):-
	encapsulated_clause(C, [], C_).

%!	encapsulated_clause(+Clause, +Acc, -Encapsulated) is det.
%
%	Business end of encapsulated_clause/2.
%
encapsulated_clause(:-((L,Ls)),Acc,C_):-
% Definite goal; L is the first literal.
	!
	,L =.. [F|As]
	,L_ =.. [m|[F|As]]
	,encapsulated_clause(:-(Ls),[:-L_|Acc],C_).
encapsulated_clause(:-(L),Acc,C):-
% Definite goal: L is the single remaining literal.
	!
	,L =.. [F|As]
	,L_ =.. [m|[F|As]]
	,reverse([:-L_|Acc],Ls)
	,once(list_tree(Ls,C)).
encapsulated_clause(H:-B,[],H:-B):-
% Definite clause; H is the head of a built-in predicate.
	predicate_property(H,built_in)
	,!.
encapsulated_clause((L,Ls),Acc,C_):-
% Definite clause; L is an atom of a built-in predicate.
	predicate_property(L,built_in)
	,!
	,encapsulated_clause(Ls,[L,Acc],C_).
encapsulated_clause(L,Acc,(H:-Bs)):-
% Definite clause; L is an atom of a built-in predicate.
	L \= (_,_)
	,predicate_property(L,built_in)
	,!
	,reverse([L|Acc], Ls)
	,once(list_tree(Ls,(H,Bs))).
encapsulated_clause(L:-Ls,Acc,C_):-
% Definite clause; L is the head literal.
	!
	,L =.. [F|As]
	,L_ =.. [m|[F|As]]
	,encapsulated_clause(Ls,[L_|Acc],C_).
encapsulated_clause((L,Ls),Acc,C_):-
% Definite clause; L is the next body literal.
	!
	,L =.. [F|As]
	,L_ =.. [m|[F|As]]
	,encapsulated_clause(Ls,[L_|Acc],C_).
encapsulated_clause(L,[],L_):-
% Unit clause: the accumulator is empty.
	!
	,L =.. [F|As]
	,L_ =.. [m|[F|As]].
encapsulated_clause(L,Acc,(H:-Bs)):-
% Definite clause; L is the last body literal.
	L =.. [F|As]
	,L_ =.. [m|[F|As]]
	,reverse([L_|Acc], Ls)
	,once(list_tree(Ls,(H,Bs))).



%!	unfolded_metasubs(+Metasubstitutions,-Unfolded) is det.
%
%	Project a list of Metasubstitutions onto fitting metarules.
%
%	The list of Metasubstitutions is normally the specialised Top
%	program.
%
unfolded_metasubs(Ss,Ms):-
	setof(H:-B
		,S^Ss^B_^(member(S,Ss)
			 ,metarule_projection(S,H:-B)
			 ,copy_term(B,B_)
			 ,user:call(B_)
			 ,numbervars(B)
		 )
		,Ms_)
	,findall(C_
		,(member(C,Ms_)
		 ,varnumbers(C,C_)
		 )
		,Ms).



%!	metarule_projection(+Metasubstitution,-Projection) is det.
%
%	Project a Metasubstitution onto a fitting metarule.
%
metarule_projection(S,H:-B):-
	S =.. [m,Id|Ps]
	,Mh =.. [metarule,Id|Ps]
	,clause(Mh,(H,B))
	,!.
metarule_projection(S,H:-B):-
% Expanded metarules don't have metarule/n heads!
% They're only in the database as m/n clauses.
	S =.. [m,_Id|_Ps]
	,clause(S,Ls)
	,literals_clause(Ls,(H,B)).


%!	literals_clause(+Literals,-Clause) is det.
%
%	Remove signature from an expended metarule body.
%
%	The signature terms of extended metarules are not conveniently
%	wrapped in ()'s and so they're fiddlier to get rid of than for
%	ordinary metarules. This handles the necessary fiddling.
%
literals_clause((L,Ls),(L,Ls)):-
	L \= s(_)
	,!.
literals_clause((s(_),Ls),C):-
	literals_clause(Ls, C).



%!	excapsulated_clauses(+Target, +Clauses, -Excapsulated) is det.
%
%	Remove encapsulation from a list of Clauses.
%
%	Only clauses of the Target predicate are processed- clauses of
%	metarules and background definitions are dropped silently.
%
excapsulated_clauses(T, Cs, Cs_):-
	excapsulated_clauses(T,Cs,[],Cs_).

%!	excapsulated_clauses(+Target,+Acc,-Excapsulated) is det.
%
%	Business end of excapsulated_clauses/3.
%
excapsulated_clauses(_T,[],Acc,Es):-
	reverse(Acc, Es)
	,!.
excapsulated_clauses(T,[C|Cs],Acc,Bind):-
	excapsulated_clause(T,C,C_)
	,!
	,excapsulated_clauses(T,Cs,[C_|Acc],Bind).
excapsulated_clauses(T,[_C|Cs],Acc,Bind):-
	excapsulated_clauses(T,Cs,Acc,Bind).


%!	excapsulated_clause(+Target,+Clause,-Excapsulated) is det.
%
%	Excapsulate a single Clause of the Target predicate.
%
excapsulated_clause(T,C,C_):-
	excapsulated_clause(T,C,[],C_).

%!	excapsulated_clause(+Target,+Clause,+Acc,-Excapsulated) is det.
%
%	Business end of excapsulated_clause/3.
%
excapsulated_clause(T/A,H:-Bs,Acc,Bind):-
% Definite clause; H is the head literal.
	H =.. [m|[T|As]]
	,length(As,A)
	,!
	,H_ =.. [T|As]
	,excapsulated_clause(T,Bs,[H_|Acc],Bind).
excapsulated_clause(T,(L,Ls),Acc,Bind):-
% Definite clause: L is the next body literal.
	!
	,L =.. [m|[F|As]]
	,L_ =.. [F|As]
	,excapsulated_clause(T,Ls,[L_|Acc],Bind).
excapsulated_clause(T/A,L,[],L_):-
% Unit clause: the accumulator is empty.
	!
        ,L =.. [m|[T|As]]
	,length(As, A)
	,ground(T)
	,L_ =.. [T|As].
excapsulated_clause(_T,(L),Acc,(H:-Bs)):-
% Definite clause: L is the last literal.
	L =.. [m|[F|As]]
	,L_ =.. [F|As]
	,reverse([L_|Acc],Ls)
	,once(list_tree(Ls,(H,Bs))).

