:-module(examples_invention, [learn_with_partial_examples/1
			     ,learn_with_partial_examples/2
			     ,learn_with_partial_examples/5
			     ,learn_with_examples_invention/1
			     ,learn_with_examples_invention/2
			     ,learn_with_examples_invention/5
			     ,examples_invention/1
			     ,examples_invention/2
			     ,examples_invention/5
			     ,partial_examples/2
			     ]).

:-use_module(src(louise)).
:-use_module(src(mil_problem)).
:-use_module(lib(tp/tp)).

/** <module> Examples invention.

Predicates in this module learn programs by generalising positive
examples. A generalised example is entailed by more clauses in the
Hypothesis Language, therefore more general programs can be learned than
with a set of fully-ground examples.

Generalising examples of course introduces a risk of learning
over-general programs. To combat this, negative examples or strictly
relevant background knowledge and metarules are needed.

There are three groups of predicates defined in this module.

1. learn_with_partial_examples/[1,2,5]

These learning predicates partially generalise examples producing a set
of atoms of the target predicates where a constant is replaced with a
variable. For example a positive example p(a,b) yields two partial
examples, p(X,b) and p(a,Y), etc. learn_with_partial_examples/[1,2,5]
then learn a Top Program from the set of partial examples. The Top
Program is reduced _with the original examples_. Reduction with the
partial examples may be over-strong (because the partial examples may be
over-general).

Learning with partial examples can produce clauses that entail no
positive examples. These are removed from the Top Program before
reductin.

2. learn_with_examples_invention/[1,2,5]

These learning predicates first invent a new set of ground examples,
then learn a Top Program with those examples. The learned Top Program is
reduced with respect to the learned ground examples.

3. examples_invention/[1,2,5]

These three predicates perform examples invention. To invent examples,
first a set of partial examples is generated, in the same way described
in section 1 above. Then, a Top Program is constructed from the partial
examples. Finally, the Least Herbrand Model of the Top Program for the
partial examples is derived in a bottom-up fashion. The result is a set
of ground atoms of the learning targets.

4. partial_examples/2

This predicate is responsible for generalising examples by partially
variabilising them.

*/


%!	learn_with_partial_examples(+Targets) is det.
%
%	Learn a program from partial examples of a set of Targets.
%
learn_with_partial_examples(Ts):-
	learn_with_partial_examples(Ts,Ps)
	,print_clauses(Ps).



%!	learn_with_partial_examples(+Targets,-Program) is det.
%
%	Learn a program from partial examples of a set of Targets.
%
learn_with_partial_examples(Ts,Ps):-
	experiment_data(Ts,Pos,Neg,BK,MS)
	,learn_with_partial_examples(Pos,Neg,BK,MS,Ps).



%!	learn_with_partial_examples(+Pos,+Neg,+BK,+MS,-Program) is det.
%
%	Learn a program from partial examples.
%
%	Pos, Neg, BK and MS are the elements of a MIL problem (positive
%	and negative examples, BK and metarules, respectively).
%
%	Positive examples in Pos are generalised by a call to
%	partial_examples/2. A Top Program is constructed from the
%	partial examples and reduced _with the original examples_ (lest
%	it be over-reduced when partial examples are maximally general).
%
%	Program is the Top Program learned with partial examples and
%	reduced with the original, assumed fully ground, examples.
%
learn_with_partial_examples(Pos,Neg,BK,MS,Ps):-
	partial_examples(Pos,Es)
	,encapsulated_problem(Es,Neg,BK,MS,[Es_e,Neg_,BK_,MS_])
	,top_program(Es_e,Neg_,BK_,MS_,Ps_)
	,subtract(Ps_,Es_e,Ps_s)
	,examples_targets(Pos,Ss)
	,encapsulated_clauses(Pos,Pos_e)
	,remove_null(Ps_s,Pos_e,BK_,Ss,Ps_n)
	,reduced_top_program(Pos_e,BK_,MS_,Ps_n,Rs)
	,excapsulated_clauses(Ss,Rs,Ps).


%!	remove_null(+Top,+Pos,+BK,+Symbols,-New_Top) is det.
%
%	Remove Top-Null clauses from a Top Program.
%
%	Top-Null is the set of clauses in the Hypothesis Language of a
%	MIL problem that entail 0 positive examples. A Top Program
%	learned from partially generalised examples, as in
%	learn_with_partial_examples/5, can include clauses in Top Null.
%	This predicate removes those clauses to return a Top Program
%	where each clause entails at least one positive example.
%
%	Symbols is the set of predicate symbols in Pos, used to table
%	and untable encapsulated examples.
%
remove_null(Ts,Pos,BK,Ss,Ts_):-
	assert_program('$remove_null',Ts,Rs_1)
	,S = (assert_program('$remove_null',BK,Rs_2)
	     ,table_encapsulated(Ss)
	     )
	,G = remove_null_(Ts,Pos,[],Ts_,[],Rs_3)
	,C = (eraseall_program_clauses(Rs_1)
	     ,eraseall_program_clauses(Rs_2)
	     ,eraseall_program_clauses(Rs_3)
	     ,untable_encapsulated(Ss)
	     )
	,setup_call_cleanup(S,G,C).

%!	remove_null_(+Top,+Pos,+Acc,Acc_Refs,-New,-Refs) is det.
%
%	Business end of remove_null/4.
%
%	Acc_Refs is the accumulator of clause references of re-asserted
%	clauses. Refs is the output variable. These are needed to clean
%	up the dynamic database after manipulating it. Evil.
%
remove_null_([],_,Acc_Ts,Ts,Rs,Rs):-
	!
       ,reverse(Acc_Ts,Ts).
remove_null_([C|Ts],Pos,Acc_Ts,Bind_Ts,Acc_Rs,Bind_Rs):-
	retract('$remove_null':C)
	,debug_clauses(remove_null,'Retracted clause',[C])
	,\+ prove_examples(Pos)
	,!
	,assert('$remove_null':C, R)
	,debug_clauses(remove_null,'Re-asserted clause',[C])
	,remove_null_(Ts,Pos,[C|Acc_Ts],Bind_Ts,[R|Acc_Rs],Bind_Rs).
remove_null_([C|Ts],Pos,Acc_Ts,Bind_Ts,Acc_Rs,Bind_Rs):-
	debug_clauses(remove_null,'Leaving clause out',[C])
	,remove_null_(Ts,Pos,Acc_Ts,Bind_Ts,Acc_Rs,Bind_Rs).


%!	prove_examples(+Pos) is det.
%
%	Prove a set of positive examples against a Top Program.
%
prove_examples([]):-
	!.
prove_examples([E|Pos]):-
% Not strictly necessary eh?
% But we may end up calling this with generalised examples...
	copy_term(E, E_)
	,debug_clauses(remove_null,'Proving example',[E_])
	,call('$remove_null':E_)
	,debug_clauses(remove_null,'Proved example',[E_])
	,prove_examples(Pos).


%!	table_encapsulated(+Targets) is det.
%
%	Table each target predicate defined in a Top Program.
%
%	Targets is the set of predicate symbols and arities of the
%	learning targets defined in a Top Program learned from partial
%	examples.
%
%	Variant of dynamic_learning:table_encapsulated/1 (now removed)
%	that also declares target predicates dynamic and incremental,
%	allowing incremental tables to be updated when a tabled
%	predicate depends on itself.
%
%	The motivation for this is that if we have a target predicate
%	f/2 in Top and this is defined in terms of itself, tabled
%	execution will store the first set of resulst returned from the
%	first query to an example of f/2, then continue returning the
%	same result _even when we remove clauses of Top from the dynamic
%	database_. This would completely defeat the purpose of removing
%	clauses to test their necessity to the Top Program's
%	correctness. Declaring target predicates as dynamic and
%	incremental forces Swi-Prolog's tabling mechanism to re-evaluate
%	tables when a clause of the Top Program is added to or removed
%	from the dynamic database during execution of remove_null_/4.
%
table_encapsulated(Ts):-
	forall(member(_F/A,Ts)
	      ,(succ(A,A_)
	       ,table('$remove_null':m/A_ as incremental)
	       ,(dynamic(['$remove_null':m/A_], [incremental(true)]))
	       ,debug_clauses(remove_null,'Tabled predicate',[m/A_])
	       )
	      ).


%!	untable_encapsulated(+Targets) is det.
%
%	Untable tabled predicates of learning targets in a Top Program.
%
%	Counterpart of table_encapsulated/1.
%
untable_encapsulated(Ts):-
	forall(member(_F/A,Ts)
	      ,(succ(A,A_)
	       ,untable('$remove_null':m/A_)
	       ,debug_clauses(remove_null,'Untabled predicate',[m/A_])
	       )
	      ).



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
