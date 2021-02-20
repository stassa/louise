:-module(examples_invention, [learn_with_examples_invention/1
			     ,learn_with_examples_invention/2
			     ,learn_with_examples_invention/5
			     ,examples_invention/1
			     ,examples_invention/2
			     ,examples_invention/5
			     ]).

:-use_module(src(louise)).
:-use_module(src(mil_problem)).
:-use_module(lib(tp/tp)).

/** <module> Examples invention.

Documentation pending.

*/


%!	learn_with_examples_invention(+Targets) is det.
%
%	Learn a program for one or more Targets with invented examples.
%
%	Prints the results to the console. Elements of the MIL problem
%	for Target are obtained from the currently loaded experiment
%	file.
%
learn_with_examples_invention(Ts):-
	learn_with_examples_invention(Ts,Ps)
	,print_clauses(Ps).



%!	learn_with_examples_invention(+Targets,-Program) is det.
%
%	Invent new positive examples of Targets to learn a Program.
%
%	Elements of the MIL problem for each target predicate in Targets
%	are obtained from the currently loaded experiment file.
%
learn_with_examples_invention(T,Ps):-
	experiment_data(T,Pos,Neg,BK,MS)
	,learn_with_examples_invention(Pos,Neg,BK,MS,Ps).



%!	learn_with_examples_invention(+Pos,+Neg,+BK,+Metarules,-Program)
%!	is det.
%
%	Invent new positive examples to learn a Program.
%
learn_with_examples_invention(Pos,Neg,BK,MS,Ps):-
	debug(examples_invention,'Inventing examples',[])
	,examples_invention(Pos,Neg,BK,MS,Es)
	,debug(examples_invention,'Given and invented examples',[])
	,debug_clauses(examples_invention,Es)
	,debug(examples_invention,'Encapsulating problem',[])
	,encapsulated_problem(Es,Neg,BK,MS,[Es_,Neg_,BK_,MS_])
	,debug(examples_invention,'Constructing Top program...',[])
	,top_program(Es_,Neg_,BK_,MS_,Ts)
	,debug(examples_invention,'Reducing Top program',[])
	,reduced_top_program(Es_,BK_,MS_,Ts,Rs)
	,debug(examples_invention,'Excapsulating hypothesis',[])
	,examples_targets(Pos,Ss)
	,excapsulated_clauses(Ss,Rs,Ps).



%!	examples_invention(+Targets) is det.
%
%	Invent new positive examples of one or more learning Targets.
%
examples_invention(Ts):-
	experiment_data(Ts,Pos,Neg,BK,MS)
	,examples_invention(Pos,Neg,BK,MS,Es)
	,print_clauses(Es).



%!	examples_invention(+Targets,-Examples) is det.
%
%	Invent new positive Examples of one or more learning Targets.
%
examples_invention(Ts,Es):-
	experiment_data(Ts,Pos,Neg,BK,MS)
	,examples_invention(Pos,Neg,BK,MS,Es).



%!	examples_invention(+Pos,+Neg,+BK,+MS,-Examples) is det.
%
%	Invent a new set of positive Examples from a MIL problem.
%
examples_invention(Pos,Neg,BK,MS,Es):-
	partial_examples(Pos,Es_)
	,encapsulated_problem(Es_,Neg,BK,MS,[Es_e,Neg_,BK_,MS_])
	% TODO: should Pos also be added in?
	% LATER: It's not included in Es_.
	%,append(Es_,Pos_,Es_Pos)
	%,top_program(Es_Pos,Neg_,BK_,MS_,Ts)
	,debug(examples_invention,'Learning with partial examples...',[])
	,top_program(Es_e,Neg_,BK_,MS_,Ts)
	,debug_clauses(examples_invention,'Top Program for partial examples:',Ts)
	,encapsulated_clauses(Pos,Pos_e)
	,least_herbrand_model(Pos_e,Neg_,BK_,Ts,Ls)
	,examples_targets(Pos,Ss)
	,excapsulated_clauses(Ss,Ls,Es).


%!	partial_examples(+Examples,-Partial) is det.
%
%	Construct Partial examples for examples invention.
%
%	Examples is a list of positive examples of each target predicate
%	in a MIL problem. Partial is a list of partial examples of each
%	target predicate in Examples. A partial example is a partially
%	insantiated member of Examples, with a single argument ground
%	and all others as free variables.
%
%	For example, if Examples includes an atom path(a,f), Partial
%	will include two atoms, path(a,X) and path(Y,f), each a partial
%	example derived from path(a,f).
%
partial_examples(Pos,Es):-
	generalised_examples(Pos, [], Es)
	,debug_clauses(examples_invention,'Partial examples',Es).

%!	generalised_examples(+Es,+Acc,-Generalised) is det.
%
%	Business end of partial_examples/2.
%
generalised_examples([],Acc,Gs):-
	!
	,flatten(Acc,Gs_f)
	,reverse(Gs_f,Gs).
generalised_examples([E|Es],Acc,Bind):-
	generalised_example(E,Gs)
	,generalised_examples(Es,[Gs|Acc],Bind).

generalised_example(E,E_):-
	E =.. [F|[_]]
	,!
	,functor(E_,F,1).
generalised_example(E,Gs):-
	E =.. [F|As]
	,length(As,A)
	,findall(E_
		,(functor(E_,F,A)
		 ,E_ =.. [F|As_]
		 ,nth1(I,As,Ai)
		 ,nth1(I,As_,Ai)
		 )
		,Gs).


%!	least_herbrand_model(+Pos,+Neg,+BK,+Top,-LHM) is det.
%
%	Derive the Least Herbrand Model of a MIL problem.
%
%	Pos, Neg, BK are elements of a MIL problem and Top its Top
%	Program, all encapsulated.
%
%	LHM is the LHM of the inputs, evaluated bottom-up (primarily to
%	avoid infinite recursions- but remember that this guarantee only
%	holds with datalog, i.e. no functions. With functions, it's
%	all up in the air) and restricted to atoms of the target
%	predicates in Pos.
%
least_herbrand_model(Pos,Neg,BK,Ts,LHM):-
	flatten([Pos,Neg,BK,Ts], Ps)
	,debug_clauses(examples_invention,'Deriving Least Herbrand Model of:',Ps)
	,lfp_query(Ps,[],As)
	,debug_clauses(examples_invention,'Derived LHM:',As)
	,examples_targets(Pos, Ss)
	% Kind of ugly. Can we beautify?
	% Restricts LHM to clauses of target preds in As.
	,findall(E
		,(member(T/A,Ss)
		 ,functor(E,T,A)
		 )
		,Fs)
	,encapsulated_clauses(Fs,Fs_)
	,setof(E_
	      ,As^Fs_^(member(E_,Fs_)
		      ,member(E_,As)
		      )
	      ,LHM).
