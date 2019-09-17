:-module(mil_problem, [metarule_parts/5
		      ,expanded_metarules/2
		      ,metarule_expansion/2
		      ,extended_metarules/2
		      ,metarule_extension/3
		      ,encapsulated_problem/5
		      ,encapsulated_bk/2
		      ,examples_target/2
		      ,encapsulated_clauses/2
		      ,unfolded_metasubs/2
		      ,metarule_projection/2
		      ,excapsulated_clauses/3
		      ]).

:-use_module(configuration).
:-use_module(lib(term_utilities/term_utilities)).

/** <module> Predicates to transform a MIL problem.

*/

%!	metarule_parts(?Metarule,?Id,?Sub,?Head,?Body) is nondet.
%
%	Extract the parts of a Metarule in the dynamic database.
%
%	Metarule is an encapsulated metarule in the dynamic database. It
%	is expanded, as a clause of m/n, where n one of the arities of
%	metarule and expanded metarules in the db.
%
%	Id is the ide of Metarule- a name, number, etc atomic id to help
%	find the metarule in the program database.
%
%	Sub is the substitution atom at the head of the encapsulated
%	metarule.
%
%	Head and Body are the encapsulated head and body literals of the
%	Metarule, respectively.
%
%	Note that one of Id, Metarule or Sub must be at least partly
%	instantiated or metarule_parts/6 will fail silently.
%
%	Use this predicate to access encapsulated and expanded metarules
%	in their internal representation, as clauses of m/n in the
%	dynamic database.
%
%	_DO NOT USE_ metarule_expansion/2 to access extended metarules
%	in the dynamic database - for one thing, metarule_expansion/2
%	cannot access _extended_ metarules (because they are
%	automatically created and named and have no corresponding
%	metarule/n declaration in the configuration). For another, it is
%	only there to transform metarules from their user-friendly
%	configuration representation to the internal m/n representation.
%
metarule_parts(Id,M,Sub,H,B):-
	ground(Id)
	,!
	,metarule_parts_(Id,M,Sub,H,B).
metarule_parts(Id,M,Sub,H,B):-
	nonvar(M)
	,!
	,M = (Sub:-_)
	,Sub =.. [m,Id|_Ps]
	,metarule_parts_(Id,M,Sub,H,B).
metarule_parts(Id,M,Sub,H,B):-
	ground(Sub)
	,Sub =.. [m,Id|_Ps]
	,metarule_parts_(Id,M,Sub,H,B).

%!	metarule_parts_(+Id,?Metarule,?Sub,-Head,-Body) is det.
%
%	Business end of metarule_parts/5.
%
%	metarule_parts/5 is responsible for grounding Id, depending on
%	its own bindings.
%
metarule_parts_(Id,M,Sub,H,B):-
	ground(Id)
	,user:current_predicate(m,Sub)
	,Sub =.. [m,Id|_Ps]
	,clause(Sub,(H,B))
	,M = (Sub:-(H,B)).



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
%	@tbd In line 5 of this predicate strip_module/3 is called to
%	allow metarules defined in experiment files to be used without
%	stumbling over complicated patterns of module qualifiers (which
%	will be different in their head and body literals).
%
%	@tbd After the removeal of signature atoms this predicate now
%	only changes the metarule/n symbol of metarules declared in the
%	configuration module and experiment files to m/n (and strips the
%	module qualifiers from it). In other words, it transforms
%	metarules from the user-friendly (-er) representation used in
%	configuration and experiment files to Louise's internal
%	representation that is more appropriate for learning.
%
metarule_expansion(Id,Mh_:-Mb_):-
	configuration:current_predicate(metarule,Mh)
	,Mh =.. [metarule,Id|Ps]
	,Mh_ =.. [m,Id|Ps]
	,clause(Mh,Mb)
	,strip_module(Mb,_M,Mb_).


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
metarule_extension((M1:-B1),(M2:-B2),(M3:-B3)):-
	metasubs_extension(M1,M2,M3)
	,unfold(B1,B2,B3).


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
%	Ps is a list [Pos_, Neg_, BK_, MS_] where elements are the
%	encapsulations of the positive and negative examples, BK
%	definitions, and Metarules, respectively.
%
%	@tbd Encapsulated forms need documentation.
%
encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_]):-
	configuration:extend_metarules(E)
	,encapsulated_bk(BK,BK_)
	,(   E == true
	 ->  extended_metarules(MS, MS_)
	 ;   expanded_metarules(MS,MS_)
	 )
	,encapsulated_clauses(Pos,Pos_)
	,encapsulated_clauses(Neg,Neg_).



%!	encapsulated_bk(+Background,-Encapsulated) is det.
%
%	Encapsulate a list of Background definitions.
%
encapsulated_bk(BK,BK_flat):-
	closure(BK, user, Ps)
	,findall(Cs_
	       ,(member(Cs, Ps)
		,encapsulated_clauses(Cs,Cs_)
		)
	       ,BK_)
	,flatten(BK_, BK_flat).



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
	,encapsulated_clause(Ls,[L|Acc],C_).
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
	findall(P
		,(member(S,Ss)
		 ,metarule_projection(S,P)
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
% Extended metarules don't have metarule/n heads!
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
%	@tbd This operates on the predicate signature that is currently
%	being removed. Needs more work.
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

