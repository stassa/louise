:-module(examples_invention, [learn_with_examples_invention/2
			     ,examples_invention/2
			     ,examples_invention/5
			     ]).

:-use_module(src(louise)).
:-use_module(src(mil_problem)).
:-use_module(lib(tp/tp)).

/** <module> Examples invention for non-bias inductive shift.

Documentation pending.

*/

%!	learn_with_examples_invention(+Target,-Program) is det.
%
%	Invent new positive examples of Target to learn a Program.
%
learn_with_examples_invention(T,Ps):-
	experiment_data(T,Pos,Neg,BK,MS)
	,debug(examples_invention,'Inventing examples',[])
	,examples_invention(Pos,Neg,BK,MS,Es)
	,debug(examples_invention,'Given and invented examples',[])
	,debug_clauses(examples_invention,Es)
	,debug(examples_invention,'Encapsulating problem',[])
	,encapsulated_problem([],Neg,BK,MS,[[],Neg_,BK_,MS_])
	,debug(examples_invention,'Constructing Top program',[])
	,top_program(Es,Neg_,BK_,MS_,Ts)
	,debug(examples_invention,'Reducing Top program',[])
	,reduced_top_program(Es,BK_,MS_,Ts,Rs)
	,debug(examples_invention,'Excapsulating hypothesis',[])
	,excapsulated_clauses(T,Rs,Ps).



%!	examples_invention(+Target,-Examples) is det.
%
%	Invent new positive Examples of a learning Target.
%
examples_invention(T,Es):-
	experiment_data(T,Pos,Neg,BK,MS)
	,examples_invention(Pos,Neg,BK,MS,Es).



%!	examples_invention(+Pos,+Neg,+BK,+MS,-Examples) is det.
%
%	Invent a new set of positive Examples from a MIL problem.
%
examples_invention(Pos,Neg,BK,MS,Es):-
	examples_target(Pos, T/A)
	,functor(E,T,A)
	,partial_examples(T/A,Pos,Es_)
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,append(Es_,Pos_,Es_Pos)
	,top_program(Es_Pos,Neg_,BK_,MS_,Ts)
	,debug(examples_invention,'Generalised partial examples:',[])
	,debug_clauses(examples_invention,Ts)
	,flatten([Pos_,Neg_,BK_,Ts], Rs)
	,lfp_query(Rs,[],As)
	,encapsulated_clauses([E],[E_])
	,setof(E_
	      ,As^(member(E_,As)
		  )
	      ,Es).


%!	partial_examples(+Target,+Positive,-Examples) is det.
%
%	Construct partial examples for examples invention.
%
%	Positive is a list of positive examples of Target. Examples is
%	a list of partial examples of Target in Pos. A partial example
%	is a a partially insantiated member of Positive, with a single
%	argument ground and all others as free variables.
%
%	For example, if Positive includes an atom path(a,f), Examples
%	will include two atoms, path(a,X) and path(Y,f), each a partial
%	example derived from path(a,f).
%
partial_examples(T/A,Pos,Es):-
	findall(E_
		,(member(E,Pos)
		 ,E =.. [F|As]
		 ,functor(E_,T,A)
		 ,E_ =.. [F|As_]
		 ,nth1(I,As,Ai)
		 ,nth1(I,As_,Ai)
		 )
		,Es_)
	,encapsulated_clauses(Es_,Es).

