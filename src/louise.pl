:-module(louise, [learn/1
		 ,learn/2
		 ,learn/5
		 ,projected_metasubs/4
		 ,projected_metasubs/3
		 ,metasubstitutions/5
		 ,encapsulated_problem/5
		 ,encapsulated_bk/2
		 ,encapsulated_clauses/2
		 ,predicate_signature/3
		 ,encapsulated_metarules/2
		 ,named_metarules/2
		 ,metarule_expansion/2
		 ,excapsulated_clauses/3
		 ]).

:-use_module(configuration).


%!	learn(+Target) is det.
%
%	Learn a deafinition of a Target predicate.
%
learn(T):-
	learn(T,Ps)
	,print_clauses(Ps).



%!	learn(+Target,-Definition) is det.
%
%	Learn a definition of a Target predicate.
%
learn(T,Ps):-
	experiment_data(T,Pos,Neg,BK,MS)
	,learn(Pos,Neg,BK,MS,Ps).



%!	learn(+Pos,+Neg,+BK,+Metarules,-Progam) is det.
%
%	Learn a Progam from a MIL problem.
%
learn(Pos,Neg,BK,MS,Ps):-
	metasubstitutions(Pos,Neg,BK,MS,Ms)
	,projected_metasubs(Ms,Pos,BK,Ms_)
	%,reduction_report(Ms_)
	,program_reduction(Ms_,Rs,_)
	,ord_subtract(Rs,Pos,Rs_)
	,examples_target(Pos,T)
	,excapsulated_clauses(T,Rs_,Ps).



%!	projected_metasubs(+Metasubstitutions,+Pos,+BK,-Projected) is det.
%
%	Project a list of Metasubstitutions onto fitting metarules.
%
%	As projected_metasubs/3 but only projects metasubstitutions that
%	correspond to clauses that are correct with respect to the
%	positive examples in Pos.
%
projected_metasubs(Ds,Pos,BK,Us):-
	encapsulated_bk(BK,BK_)
	,encapsulated_clauses(Pos,Pos_)
	,assert_program(user,BK_,Rs_BK)
	,assert_program(user,Pos_,Rs_Pos)
	,setof(H:-B
		,D^Ds^B_^(member(D,Ds)
			 ,metarule_projection(D,H:-B)
			 ,copy_term(B,B_)
			 ,user:call(B_)
			 ,numbervars(B)
		 )
		,Us_)
	,erase_program_clauses(Rs_BK)
	,erase_program_clauses(Rs_Pos)
	,findall(C_
		,(member(C,Us_)
		 ,varnumbers(C,C_)
		 )
		,Us_s)
	,append(BK_,Pos_,Ps)
	,append(Ps, Us_s, Us).



%!	projected_metasubs(+Metasubstitutions,+BK,-Projected) is det.
%
%	Project a list of Metasubstitutions onto fitting metarules.
%
projected_metasubs(Ds,BK,Us):-
	encapsulated_bk(BK,BK_)
	,assert_program(user,BK_,Rs)
	,setof(H:-B
		,D^Ds^B_^(member(D,Ds)
			 ,metarule_projection(D,H:-B)
			 ,copy_term(B,B_)
			 ,user:call(B_)
			 ,numbervars(B)
		 )
		,Us_)
	,erase_program_clauses(Rs)
	,findall(C_
		,(member(C,Us_)
		 ,varnumbers(C,C_)
		 )
		,Us_s)
	,append(BK_, Us_s, Us).



%!	metasubstitutions(+Positive,+Negative,+BK,+Metarules,-Metasubstitutions)
%!	is det.
%
%	Collect all correct Metasubstitutions.
%
metasubstitutions(Pos,Neg,BK,MS,Ss):-
	encapsulated_problem(Pos,Neg,BK,MS,Pos_,Neg_,Es)
	,assert_program(user,Es,Rs)
	,setof(H
	      ,M^MS^Ep^Pos^(member(M,MS)
			   ,member(Ep,Pos_)
			   ,metasubstitution(Ep,M,H)
			   )
	      ,Ss_Pos)
	,setof(H
	      ,Ss_Pos^En^Neg_^M^
	       (member(H,Ss_Pos)
	       ,\+((member(En,Neg_)
		   ,metasubstitution(En,M,H)
		   )
		  )
	       )
	      ,Ss)
	,erase_program_clauses(Rs).


%!	metasubstitution(+Example,+Metarule,-Metasubstitution) is
%!	nondet.
%
%	Perform one Metasubstutition of Metarule initialised to Example.
%
%	Example is either a positive example or a negative example. A
%	positive example is a ground definite unit clause, while a
%	negative example is a ground definite goal (i.e. a clause of the
%	form :-Example).
%
metasubstitution(:-E,M,H):-
	!
	,encapsulated_metarule(M,(H:-(_Ps,(E,Ls))))
	,user:call(Ls).
metasubstitution(E,M,H):-
	encapsulated_metarule(M,(H:-(_Ps,(E,Ls))))
	,user:call(Ls).



%!	encapsulated_problem(+Pos,+Neg,+BK,+Metarules,-Encapsulated) is
%!	det.
%
%	Encapsulate a MIL problem.
%
%	Pos and Neg are lists of example atoms; Pos are negative
%	examples and Neg are negative examples, of the form :-E, where
%	E an atom.
%
%	BK is a list of predicate symbols and arities of BK predicates.
%
%	Metarules is a list of constants, the names of metarules in the
%	problem.
%
%	Encapsulated is an encapsulation of the BK definitions, Positive
%	and negative examples and Metarules.
%
%	@tbd Encapsulated forms need documentation.
%
encapsulated_problem(Pos,Neg,BK,MS,Ps):-
	encapsulated_problem(Pos,Neg,BK,MS,_Pos_,_Neg_,Ps).

%!	encapsulated_problem(+Pos,+Neg,+BK,+MS,-Enc_Pos,-Enc_Neg,-Enc)
%!	is det.
%
%	Business end of encapsulated_problem/5.
%
%	As encapsulated_problem/5 but also returns encapsulated positive
%	and negative examples, in Enc_Pos and Enc_Neg.
%
encapsulated_problem(Pos,Neg,BK,MS,Pos_,Neg_,Ps):-
	encapsulated_bk(BK,BK_)
	,encapsulated_metarules(MS,MS_)
	,encapsulated_clauses(Pos,Pos_)
	,encapsulated_clauses(Neg,Neg_)
	,predicate_signature(Pos,BK,Ss)
	,append(Pos_,Neg_,Es)
	,append(Ss,Es,Ps_1)
	,append(Ps_1,BK_,Ps_2)
	,append(Ps_2,MS_,Ps).



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
	functor(E,F,A).



%!	encapsulated_metarules(+Ids,-Encapsulated) is det.
%
%	Encapsulate a set of metarules.
%
encapsulated_metarules(Ids,Es):-
	named_metarules(Ids,Ms)
	,findall(H_:-B
		,(member(H:-B,Ms)
		 ,H =.. [metarule|As]
		 ,H_ =.. [m|As]
		 )
		,Es).


%!	encapsulated_metarule(+Id,-Encapsulated) is det.
%
%	Encapsulate a metarule.
%
encapsulated_metarule(Id,H_:-B):-
	metarule_expansion(Id,H:-B)
	,H =.. [metarule|As]
	,H_ =.. [m|As].



%!	named_metarules(?Ids,-Metarules) is det.
%
%	Collect the definitions of all Metarules named in Ids.
%
%	Metarules is a list ofmetarule definitions expanded by
%	metarule_expansion/2.
%
%	If Ids is a list of metarule names, only definitions of the
%	named metarules are bound to Metarules.
%
%	If Ids is a free variable, it is bound to a list of the names of
%	all Metarules known to the system.
%
named_metarules(Ids,Ms):-
	var(Ids)
	,!
	,findall(Id-M
	       ,metarule_expansion(Id,M)
	       ,Ids_Ms)
	,pairs_keys_values(Ids_Ms,Ids,Ms).
named_metarules(Ids,Ms):-
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
metarule_expansion(Id,Mh:-(Es_,Mb)):-
	configuration:metarule_language(Min,Max)
	,between(Min,Max,N)
	,length(Ps,N)
	,Mh =.. [metarule,Id|Ps]
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


%!	metarule_projection(+Metasubstitution,-Projection) is det.
%
%	Project a Metasubstitution onto a fitting metarule.
%
metarule_projection(S,H:-B):-
	S =.. [m,Id|Ps]
	,Mh =.. [metarule,Id|Ps]
	,clause(Mh,(H,B)).



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
