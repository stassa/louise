:-module(metagen, [encapsulated_metarules/3
		  ,encapsulated_metarule/3
		  ,generate_metarules/3
		  ,generate_metarule/3
		  ]).

/** <module> Generator for chain and inverse and their extensions.
*/

%!	encapsulated_metarules(+Ids,Literals,-Metarules) is det.
%
%	Generate a list of the named encapsulated Metarules.
%
encapsulated_metarules(IDs,N,MS):-
	findall(M
	       ,(member(Id,IDs)
		,between(2,N,N_)
		,encapsulated_metarule(Id,N_,M)
		)
	       ,MS).



%!	encapsulated_metarule(+Id,+Literals,-Metarule) is semidet.
%
%	Generate an encapsulated Metarule.
%
encapsulated_metarule(Id,N,(A:-H,B)):-
	generate_metarule(Id,N,H:-B)
	,existential_vars((H,B),[],Es)
	%,atomic_list_concat([Id,N],'_',Id_N)
	%,A =.. [m,Id_N|Es]
	,A =.. [m,Id|Es].



%!	generate_metarules(+Ids,+N,-Metarules) is det.
%
%	Generate a list of the named Metarules with up to N literals.
%
generate_metarules(Ids,N,MS):-
	findall(M
	       ,(member(Id,Ids)
		,between(2,N,N_)
		,generate_metarule(Id,N_,M)
		)
	       ,MS).


%!	existential_vars(+Literals,+Acc,-Variables) is det.
%
%	Collect existentially qualified variables in a set of Literals.
%
existential_vars((L,Ls),Acc,Bind):-
	L =.. [m,P|_]
	,existential_vars(Ls,[P|Acc],Bind).
existential_vars((L),Acc,Es):-
	L \== (_,_)
	,L =.. [m,P|_]
	,reverse([P|Acc],Es).



%!	generate_metarule(+Id,+Literals,-Metarule) is semidet.
%
%	Generate a Metarule with the given number of Literals.
%
%	Id is the name of the metarule to create, one of: [chain,
%	inverse, identity].
%
%	Note that it doesn't make sense to start chain with fewer than 3
%	Literals or identity with fewer than 2.
%
generate_metarule(chain,N,_):-
	N < 3
	,!
	,fail.
generate_metarule(Id,N,H_:-B_):-
	N_ is N - 1
	,head_literal(Id,N_,Vs,H)
	,add_literals(Id,1,N,Vs,[H],Ls)
	,once(list_tree(Ls,T))
	,varnumbers(T,(H_,B_)).


%!	head_literal(+Id,+Literals,+Vars,-Head) is semidet.
%
%	Create a Head literal for the named metarule.
%
head_literal(chain,N,[N,0,1],H):-
	Max is N
	,new_literal([N,0,Max],H).
head_literal(Id,_N,[1,0,1],H):-
	memberchk(Id,[inverse,identity])
	,new_literal([1,0,1],H).


%!	new_literal(+Variables,-Literal) is det.
%
%	Create a new literal from the given set of Variables.
%
new_literal([P,A,B],m('$VAR'(Q),'$VAR'(A),'$VAR'(B))):-
	predicate_variable(P,Q).


%!	predicate_variable(+Current,-New) is det.
%
%	Create a new existentially quantified second order Variable.
%
predicate_variable(P,Q):-
	succ(P,Q).


%!	add_literals(+Id,+Current,+Length,+Vars,+Acc,-Literals) is
%!	semidet.
%
%	Generate body Literals for the named metarule.
%
add_literals(_Id,N,N,_Vs,Acc,Ls):-
	!
	,reverse(Acc,Ls).
add_literals(Id,C,N,Vs,Acc,Bind):-
	new_variables(Id,C,Vs,Vs_)
	,new_literal(Vs_,L)
	,succ(C,C_)
	,add_literals(Id,C_,N,Vs_,[L|Acc],Bind).


%!	new_variables(+Id,+Current,+Vars,-New) is semidet.
%
%	Generate variables for a new literal added to a metarule.
%
new_variables(chain,1,[P,A,B],[Q,A,B]):-
% Chain must start with P(A,B):- Q(A,B)
	predicate_variable(P,Q).
new_variables(inverse,1,[P,A,B],[Q,B,A]):-
% Inverse must start with P(A,B):- Q(B,A)
	predicate_variable(P,Q).
new_variables(chain,I,[P,A,B],[Q,B,C]):-
	I > 1
	,A < B
	,predicate_variable(P,Q)
	,succ(B,C).
new_variables(inverse,I,[P,A,B],[Q,B,A]):-
	I > 1
	,predicate_variable(P,Q).
new_variables(identity,_,[P,A,B],[Q,A,B]):-
% Every literal in identity is P(A,B), Q(A,B), R(A,B) ...
	predicate_variable(P,Q).
