:-module(thelma, [thelma_tpc/1
		 ,thelma_tpc/2
		 ,thelma_tpc/5
		 ,thelma_complete/2
		 ,thelma_complete/3
		 ,thelma_complete/6
		 ,thelma/1
		 ,thelma/2
		 ,thelma/5
		 ]).

:-use_module(project_root(configuration)).
:-use_module(project_root(src/mil_problem)).

/** <module> A Meta-Interpretive Learning system.
*/


%!	thelma_tpc(+Targets) is det.
%
%	Use Thelma to construct a Top Program for a set of Targets.
%
%	Prints the learned Top Program to the Swi-Prolog command line.
%
thelma_tpc(T):-
	thelma_tpc(T,Ps)
	,print_clauses(Ps).



%!	thelma_tpc(+Targets,-Program) is det.
%
%	Use Thelma to construct a Top Program for a set of Targets.
%
thelma_tpc(T,Prog):-
	experiment_data(T,Pos,Neg,BK,MS)
	,thelma_tpc(Pos,Neg,BK,MS,Prog).



%!	thelma_tpc(+Pos,+Neg,+BK,+Metarules,-Program) is det.
%
%	Use Thelma to construct a Top Program for a set of Targets.
%
%	This is a version of the Top Program Construction algorithm
%	(TPC) implemented by means of Thelma, itself an implementation
%	of Metagol.
%
%	The main difference between Thelma (Metagol) and TPC is that
%	Metagol constructs a hypothesis by proving the conjunction of
%	the positive examples while TPC constructs each clause in
%	isolation. There are tradeoffs between the two methods, most
%	notably the efficiency of TPC and the ability of Metagol to
%	learn multiple, dependent clauses from a single example, such as
%	the clauses of a recursive program.
%
%	This predicate implements TPC as a variant of Metagol that
%	learns a Top Program by nondeterministically running Thelma's
%	meta-interpreter on each example. Constructing a Top Program in
%	this way takes advantage of Thelma's ability to learn complex
%	programs with dependent clauses from single examples, while
%	preserving some of the efficiency of TPC.
%
%	TPC is efficient because it avoids a search of the MIL
%	Hypothesis Space, that can grow very large when the target
%	theory is large and when there are many competing hypotheses
%	that resemble the target theory. Thelma learns by searching the
%	Hypothesis Space, but training it on a single example at a time
%	keeps the Hypothesis Space to a manageable size, more often than
%	not. Yet, there are no guarantees and there are going to be
%	problems for which the Hypothesis Space increases at an
%	exponential rate (exponential to the number of constructible
%	clauses) even given a single example.
%
thelma_tpc(Pos,Neg,BK,MS,Ps):-
	configuration:unfold_invented(U)
	,configuration:fold_recursive(F)
	,debug(thelma_tpc,'Converting examples...',[])
	,convert_examples(pos,Pos,Pos_c)
	,top_program_thelma(Pos_c,Neg,BK,MS,Prog)
	,closure(BK,experiment_file,Bs)
	,flatten(Bs,Bs_f)
	,reduced_top_program(Pos,Bs_f,[],Prog,Rs_)
	,examples_targets(Pos,Ss)
	,filter_targets(Ss,Rs_,Ps_1)
	,(   U ==  true
	 ->  debug(learn,'Unfolding invented...',[])
	    ,unfold_invented(Ps_1,Pos,BK,Ps_2)
	 ;   Ps_2 = Ps_1
	 )
	,(   F == true
	 ->  debug(learn,'Folding to introduce recursion...',[])
	    ,fold_recursive(Ps_2,Ps)
	 ;   Ps = Ps_2
	 ).


%!	filter_targets(+Targets,+Clauses,-Filtered) is det.
%
%	Remove non-target predicates from a set of Clauses.
%
%	Used to clear BK out of the learned Top Program before
%	reduction.
%
filter_targets(Ss,Ps,Ps_):-
	findall(C
	       ,(member(C,Ps)
		,symbol(C,S)
		,target_or_invention(Ss,S)
		)
	       ,Ps_).


%!	symbol(+Clause,-Symbol) is det.
%
%	Extract the predicate symbol at the head of Clause.
%
symbol(C,F/A):-
	head_body(C,H,_B)
	,functor(H,F,A).


%!	head_body(+Clause,-Head,-Body) is semidet.
%
%	Split the Head and Body of a Clause.
%
head_body(H:-B,H,B).
head_body(H,H,true):-
	H \= (_:-_).


%!	top_program_thelma(+Pos,+Neg,+BK,+Metarules,-Top) is det.
%
%	Construct a Top Program for a MIL problem using Thelma.
%
top_program_thelma(Pos_c,Neg,BK,MS,Ps):-
	S = (debug(thelma_tpc,'Transforming metarules...',[])
             ,transform_metarules(MS)
	     )
	,G = (debug(thelma_tpc,'Proving examples...',[])
	     ,generalise_specialise(Pos_c,Neg,BK,MS,Ps)
	     )
	,Cl = (debug(thelma_tpc,'Cleaning up metarules...',[]),
	       cleanup_metarules
	      )
	,setup_call_cleanup(S,G,Cl).


%!	generalise_specialise(+Pos,+Neg,+BK,+Metarules,-Top) is det.
%
%	Combined Top Program Construction specialise, generalise steps.
%
generalise_specialise(Pos,Neg,BK,MS,Ss):-
	findall(C
	       ,(member(Ep,Pos)
		,debug_clauses(thelma_tpc,'Positive example:',[Ep])
		,prove([Ep],BK,MS,Subs)
		,debug_clauses(thelma_tpc,'Learned metasubs:',Subs)
		,project_metasubs(Subs,Cs)
		,debug_clauses(thelma_tpc,'Disproving metasub:',Cs)
		,disprove(Neg,BK,Cs)
		,debug_clauses(thelma_tpc,'Keeping metasub:',Cs)
		,Cs \= []
		,member(C,Cs)
		% Skolemise to sort ignoring variable ages.
		,numbervars(C)
		)
	       ,Ss_)
	,sort(Ss_, Ss_s)
	% Unskolemise.
	,varnumbers(Ss_s,Ss)
	,debug_clauses(thelma_tpc,'Learned Top Program:',Ss).



%!	thelma(+Target) is nondet.
%
%	Learn a Definition of a Target predicate.
%
%	As thelma/2 but prints each learned program to the Prolog
%	top-level rather than binding it to an output variable.
%
%	Use this predicate when you don't need to process a learned
%	program any further, i.e. by passing it to another predicate,
%	and so on.
%
thelma(T):-
	thelma(T,Ps)
	,print_clauses(Ps).



%!	thelma(+Target,-Definition) is nondet.
%
%	Learn a Definition of a Target predicate.
%
%	Target is the predicate symbol and arity of the predicate to be
%	learned. Target is used to collect positive and negative
%	examples, background knowledge and metarules from the current
%	experiment file.
%
%	Definition is a list of definite datalog clauses, the
%	learned definition of target.
%
%	thelma/2 is a high level interface to thelma/5 with data taken
%	from experiment_data/5, by passing Predicate as the target.
%
%	Use this predicate to learn from the examples, background
%	knowledge and metarules declared for Target in the current
%	experiment file. Use thelma/5 to learn from an arbitrary set of
%	examples, metarules and background knowledge.
%
thelma(T,Prog):-
	experiment_data(T,Pos,Neg,BK,MS)
	,thelma(Pos,Neg,BK,MS,Prog).



%!	thelma(+Pos,+Neg,+Background,+Metarules,-Program) is nondet.
%
%	Learn a Program from the given data.
%
%	Pos, Neg are lists of unit clauses representing positive and
%	negative examples, respectively, of the predicate to be
%	learned: the _target predicate_.
%
%	Background is a list of predicate symbols and arities, S/N, of
%	the predicates to be used as background knowledge. Each
%	predicate in the list Background must have a definition in
%	module user or a module exporting to module user (e.g. the
%	current experiment file).
%
%	Metarules is a list of atoms, the names of metarules to be used
%	in learning.
%
%	Program is a list of definite Datalog clauses, the learned
%	definition of the target predicate. Traditionally a program
%	learned by an ILP algorithm is known as a _hypothesis_.
%
%	thelma/5 returns each Program possible to learn from the given
%	data on successive backtracking.
%
%	Use this predicate to learn from arbitrary lists of examples,
%	background knowledge and metarules. However, note that
%	background predicates must be defined and accessible to this
%	module, i.e. they must be defined in module user, or a module
%	exporting to module user. The current experiment file exports to
%	module user.
%
thelma(Pos,Neg,BK,MS,Prog):-
	configuration:unfold_invented(U)
	,debug(thelma,'Converting examples...',[])
	,convert_examples(pos,Pos,Pos_c)
	,S = (debug(thelma,'Transforming metarules...',[])
             ,transform_metarules(MS)
	     )
	,G = (debug(thelma,'Proving examples...',[])
	     ,prove(Pos_c,BK,MS,Ps)
	     ,debug(thelma,'Disproving examples...',[])
	     ,project_metasubs(Ps, Prog_)
	     ,disprove(Neg,BK,Prog_)
	     )
	,Cl = (debug(thelma,'Cleaning up metarules...',[]),
	       cleanup_metarules
	      )
	,setup_call_cleanup(S,G,Cl)
	,(   U == true
	 ->  debug(thelma,'Unfolding invented predicates...',[])
	    ,unfold_invented(Prog_,Pos,BK,Prog)
	 ;   Prog_ = Prog
	 ).
thelma(_Pos,_Neg,_BK,_MS,[]).




%!	thelma_complete(+Target,+Partial) is nondet.
%
%	Complete a Partial theory with Thelma.
%
%	As thelma_complete/3 but outputs the completed theory to the
%	Prolog console instead of binding it to an output variable.
%
thelma_complete(T,Hs):-
	thelma_complete(T,Hs,Ps)
	,print_clauses(Ps).



%!	thelma_complete(+Target,+Partial,-Completed) is nondet.
%
%	Complete a Partial theory with Thelma.
%
%	Partial is a list of clauses in a logic program that entails
%	some subset of the positive examples for Target with respect to
%	background knowledge. This predicate completes Partial by
%	learning a set of clauses that all entail the positive examples
%	of Target with respect to background knowledge and outputs the
%	result in Completed.
%
thelma_complete(T,Hs,Prog):-
	experiment_data(T,Pos,Neg,BK,MS)
	,thelma_complete(Pos,Neg,BK,MS,Hs,Prog).



%!	thelma_complete(+Pos,+Neg,+BK,+Metarules,+Partial,-Complete) is
%!	nondet.
%
%	Complete a Partial theory with Thelma.
%
%	As thelma/5, but Partial is a list of clauses representing a
%	partial theory, which is completed, and output in Complete.
%
%	Partial is completed by learning additional clauses such that
%	the union of Partial and Complete entails all the positive
%	examples in Pos, and none of the negative examples in Neg, with
%	respect to the definitions of predicates listed in BK.
%
thelma_complete(Pos,Neg,BK,MS,Hs,Prog):-
	configuration:unfold_invented(U)
	,debug(thelma,'Converting examples...',[])
	,convert_examples(pos,Pos,Pos_c)
	,convert_examples(neg,Neg,Neg_c)
	,S = (debug(thelma,'Transforming metarules...',[])
             ,transform_metarules(MS)
	     )
	,G = (debug(thelma,'Proving examples...',[])
	     ,prove(Pos_c,BK,MS,Hs,Ps)
	     ,debug(thelma,'Disproving examples...',[])
	     ,disprove(Neg_c,BK,Ps)
	     ,project_metasubs(Ps, Prog_)
	     )
	,Cl = (debug(thelma,'Cleaning up metarules...',[]),
	       cleanup_metarules
	      )
	,setup_call_cleanup(S,G,Cl)
	,(   U == true
	 ->  debug(thelma,'Unfolding invented predicates...',[])
	    ,unfold_invented(Prog_,Pos,BK,Prog)
	 ;   Prog_ = Prog
	 ).
thelma_complete(_Pos,_Neg,_BK,_MS,_Hs,[]).



%!	convert_examples(+Examples,-Converted) is det.
%
%	Convert Examples to Thelma's internal represenation.
%
%	Examples is a set of example atoms, possibly negated.
%
%	Convereted is a list of lists where each sub-list is the functor
%	and arguments of an atom in Examples. Negated atoms have their
%	negation removed.
%
convert_examples(pos,Es,Es_):-
	findall(E_
	       ,(member(E,Es)
		,E =.. E_
		)
	       ,Es_).
convert_examples(neg,Es,Es_):-
	findall(E_
	       ,(member(:-E,Es)
		,E =.. E_
		)
	       ,Es_).


%!	prove(+Atoms,+BK,+Metarules,-Metasubstitutions)
%!	is nondet.
%
%	Prove a list of Atoms and derive a list of Metasubstitutions.
%
%	Atoms is a list of positive examples of the learning target. It
%	is a list of lists where each sublist is an atom in the form of
%	a list [F|As], where F the symbol of the target predicate and As
%	the list of the atom's terms.
%
%	BK is a list of predicate symbols and arities of the background
%	knowledge predicates for the learning target.
%
%	Metarules is a list of atoms, the names of metarules for the
%	learning target.
%
%	Metasubstitutions is a list of metasubstitutions. Each
%	metasubstitution is a Prolog coumpound, sub(Id, Ps). Id is the
%	name of a metarule in Metarules and Ps is a list of symbols and
%	arities of predicates in the program signature.
%
%	When prove/4 exits, each sub/2 term in the list of
%	Metasubstitutions is projected onto the corresponding metarule
%	to form a clause in a hypothesis. The hypothesis is later tested
%	for consistency with the negative examples in disprove/2 and
%	returned if it is consistent. This happens in thelma/5.
%
%	On backtracking, each list of metasubstitutions representing a
%	hypothesis that is correct with respect to the positive examples
%	are returned.
%
prove(Pos,BK,MS,Ss):-
	configuration:depth_limits(C,I)
	,target_predicate(Pos,T)
	,depth_level(C,I,C_,I_)
	,debug(depth_level,'Clauses: ~w; Invented: ~w',[C_,I_])
	,program_signature(I_,T,BK,Po,Co)
	,debug(program_signature,'Program signature: ~w',[Po-Co])
	,prove(C_,Pos,BK,MS,Po-Co,[],Ss_)
	,reverse(Ss_,Ss).


%!	prove(+Pos,+BK,+Metarules,+Partial,-Completed) is nondet.
%
%	As prove/4 but also completes a Partial theory.
%
prove(Pos,BK,MS,Hs,Ss):-
	configuration:depth_limits(C,I)
	,target_predicate(Pos,T)
	,depth_level(C,I,C_,I_)
	,debug(depth_level,'Clauses: ~w; Invented: ~w',[C_,I_])
	,program_signature(I_,T,BK,Po,Co)
	,debug(program_signature,'Program signature: ~w',[Po-Co])
	,clauses_metasubs(Hs,MS,Po-Co,Hs_)
	,prove(C_,Pos,BK,MS,Po-Co,Hs_,Ss_)
	,reverse(Ss_,Ss).


%!	target_predicate(+Examples,-Target) is det.
%
%	Determine the Target predicate from a set of Examples.
%
%	Examples is a list of lists, where each sublist is an atom in
%	the form of a list, [F|As], such that F is the predicate symbol
%	of the atom and As is the list of its terms.
%
%	Target is the symbol and arity of the learning target. The
%	predicate symbol and arity of Target are the predicate symbol
%	and number of terms in the first sub-list of Examples.
%
%	target_predicate/2 makes no attempt to check that Examples are
%	all atoms of the same predicate, etc.
%
target_predicate([[F|Args]|_Es],F/A):-
	length(Args,A).


%!	depth_level(+Clause_Max,+Invented_Max,-Clauses,-Invented) is
%!	nondet.
%
%	Increment the search depth level.
%
%	Clause_Max, Invented_Max, Clauses and Invented are integers.
%	Clauses and Invented increase by one on backtracking. Clauses
%	ranges from 1 to Clauses_Max. Invented ranges from 0 to
%	Invented_Max or Clause-Max - 1, whichever is lower.
%
%	Explanation
%	===========
%
%	Thelma performs an iterative deepening search for a _hypothesis_
%	that explains its training examples.
%
%	A hypothesis is a list of definite datalog clauses that may
%	include definitions of one or more invented predicates. The
%	depth of the search at each iteration corresponds to the _size_
%	of the hypothesis, defined as the number of all clauses in the
%	hypothesis.
%
%	This predicate is responsible for controlling the depth of the
%	search by incrementing the values of Clauses and Invented.
%
%	Clauses is the maximum hypothesis size at the current search
%	depth. Invented is the maximum number of clauses in all invented
%	definitions in the hypothesis at the current search depth.
%
%	Clause_Max and Invented_Max are the maximum number of all
%	clauses in the hypothesis and the maximum number of all clauses
%	of invented predicates in the hypothesis, in the entire search.
%
%	Clauses and Invented are first bound to Clauses=1, Invented=0
%	and then to that number plus 1 on each successive backtracking
%	into depth_level/4. The search exits without further
%	backtracking when Clauses is equal to Clause_Max and Invented
%	equal to Invented_Max, or when no more hypotheses can be found.
%
depth_level(1,0,1,0):-
	!.
depth_level(C,I,C_,I_):-
	between(1,C,C_)
	,between(0,I,I_)
	,I_ < C_.


%!	program_signature(+Invented,+Target,+BK,-Predicates,-Constants)
%!	is det.
%
%	Construct the program signature for a learning attempt.
%
%	The _program signature_ is a list of predicate symbols that can
%	be used to form a hypothesis. This list consists of:
%	a) The symbol and arity of the target predicate, at its head.
%	b) The symbols and arities of invented predicates, following.
%	c) The symbols and arities of background predicates ordered by
%	lexicographic order, at the end.
%
%	Invented is the maximum number of all clauses in invented
%	definitions in a hypothesis in the current search depth.
%
%	Target is the symbol and arity of the target predicate.
%
%	BK is a list of symbols and arities of predicates to be used as
%	background knowledge for Target.
%
%	Predicates is the program signature. The order of symbols and
%	arities of background knowledge predicates in BK is ordered by a
%	call to order_constraints/3.
%
%	Constants is the list of constants in the Herbrand universe of
%	the background knowledge predicates, also ordered by a call to
%	order_constraints/3.
%
%	@tbd It is a little strange that this predicate returns the list
%	of constants, alongside the program signature. Perhaps the call
%	to order_constraints/3 can be be moved outside this predicate,
%	in the body of thelma/5. Then program_signature/3 can take the
%	resutling ordered list of BK predicates, add the symbols of the
%	target predicate and invented predicates to it, and return it.
%
program_signature(K,F/A,BK,Ps,Cs):-
	order_constraints(BK,Ps_,Cs)
	,invented_symbols(K,F/A,Ss)
	,append([F/A|Ss],Ps_,Ps).


%!	invented_symbols(+Symbols,+Target,-Invented) is det.
%
%	Create new symbols for Invented predicates.
%
%	Symbols is an integer, the number of invented predicates that
%	may be defined in the current search iteration.
%
%	Target is the symbol and arity of the target predicate.
%
%	Invented is a list of symbols and arities of invented predicates
%	that may be defined in the current search iteration. Each
%	invented predicate has the symbol of Target indexed by an
%	integer from 1 to Symbols and the same arity as Target.
%
%	@tbd What happens if a predicate must be invented with an arity
%	different than the arity of Target?
%
invented_symbols(K,_F/A,Ss):-
	findall(S_/A
	       ,(between(1,K,I)
		%,atomic_list_concat([F,I],'_',S_)
		,atom_concat('$',I,S_)
		)
	       ,Ss).


%!	clauses_metasubs(+Clauses,+Metarules,+Ordering,-Metasubs) is
%!	det.
%
%	Convert a list of Clauses to ground metasubstitutions.
%
%	This predicate is used to convert the clauses of a partial
%	theory to a list of ground metasubstitutions. These are later
%	used by prove/7 when selecting an existing metasubstitution to
%	prove an atom.
%
clauses_metasubs(Cs,MS,Os,Ms):-
	maplist(clause_literals,Cs,Cs_Ls)
	,findall(Msub
	       ,(member(Ls,Cs_Ls)
		,findall(L_
			,(member(L,Ls)
			 ,L =.. L_
			 )
			,[Hs|Bs])
		,metasubstitution(MS,Hs,Os,Msub,Bs)
		)
	       ,Ms).


%!	prove(+Depth,+Atoms,+BK,+Metarules,+Orders,+Acc,-Metasubs)
%!	is nondet.
%
%	Business end of prove/4 and prove/5.
%
%	This is the implementation of the main meta-intperetive learning
%	procedure in Thelma.
%
%	Depth is the maximum depth for the iterative deepening search.
%	It's the maximum size of a hypothesis, i.e. the maximum number
%	of elements in the list Metasubstitutions.
%
%	Orders is a term Ps-Cs, where Ps is the program signature and Cs
%	is the _constant signature_ a list of all constants in the
%	Herbrand universe of the background predicates ordered by
%	interval inclusion order.
%
prove(_K,[],_BK,_MS,_PS,Ss,Ss):-
	!.
prove(K,[A|As],BK,MS,Os,Acc,Bind):-
        debug(prove,'Proving atom (BK): ~w', [A])
        ,background_predicate(BK,A)
	,!
	,prove_atom(A)
	,prove(K,As,BK,MS,Os,Acc,Bind).
prove(K,[A|As],BK,MS,Os,Acc1,Bind):-
	debug(prove,'Proving atom (select metasub): ~w', [A])
        ,select_metasub(Acc1,MS,A,Os,Bs)
	,prove(K,Bs,BK,MS,Os,Acc1,Acc2)
	,! % Very red cut. Avoids adding (many!)
	% redundant clauses- but will it cut
	% out necessary ones, also?
	,prove(K,As,BK,MS,Os,Acc2,Bind).
prove(K,[A|As],BK,MS,Os,Acc1,Bind):-
	debug(prove,'Proving atom (new metasub): ~w', [A])
        ,new_metasub(K,Acc1,A,MS,Os,Acc2,Bs)
	,prove(K,Bs,BK,MS,Os,Acc2,Acc3)
	,prove(K,As,BK,MS,Os,Acc3,Bind).


%!	prove_atom(+Atom) is det.
%
%	Prove an Atom of a predicate in the Background knowledge.
%
%	Atom is an atom represented as a list, [F|As], where F the
%	symbol of the atom's predicate and As the atom's list of terms.
%
%	Atom is proved by calling it with call/1.
%
prove_atom(A):-
	A_ =.. A
	,predicate_property(experiment_file:A_,number_of_clauses(M))
	,M > 0
	,experiment_file:call(A_).


%!	background_predicate(+BK,+Atom) is det.
%
%	True when Atom is an atom of a predicate in BK.
%
%	BK is a list of symbols and arities of predicates given as
%	background knowledge.
%
%	Atom is an atom represented as a list, [F|As], where F the
%	symbol of the atom's predicate and As the atom's list of terms.
%
background_predicate(BK,[F|Args]):-
	length(Args, A)
	,memberchk(F/A, BK).


%!	select_metasub(+Metasubs,+Metarules,+Atom,+Orders,-Body) is
%!	nondet.
%
%	Get the next known metasubstitution.
%
%	Metasubs is a list of metasubstitutions, the accumulator built
%	by prove/7 during learning.
%
%	Metarules is a list of constants, the names of metarules for the
%	learning target.
%
%	Atom is the atom currently being proved.
%
%	Orders is the association Ps-Cs of the predicate signature and
%	the constant signature, ordered by lexicographic and interval
%	orders.
%
%	Body is the list of body literals in a metasubstitution in
%	Metasubs. It is a list of lists, where each sub-list is of the
%	form [F|As], representing an atom of a predicate with symbol F
%	and where As are the terms of the atom.
%
select_metasub(Msubs,MS,A,Os,Bs):-
	member(Msub,Msubs)
	,once(metasubstitution(MS,A,Os,Msub,Bs))
	,debug(select_metasub, 'Selected metasub: ~w', [Msub]).


%!	new_metasub(+Depth,+Metasubs,+Atom,+Metarules,+Orders,-New,-Body)
%!	is nondet.
%
%	Create a new metasubstitution.
%
%	Depth is an integer, the maximum depth for the iterative
%	deepening search in prove/7.
%
%	Metasubs is a list of metasubstitutions, the accumulator built
%	by prove/7 during learning.
%
%	Atom is the atom currently being proved.
%
%	Metarules is a list of constants, the names of metarules for the
%	learning target.
%
%	Orders is the association Ps-Cs of the predicate signature and
%	the constant signature, ordered by lexicographic and interval
%	orders.
%
%	New is the accumulator of metasubstitutions extended by the
%	addition of a new metasubstitution, created by this predicate.
%
%	Body is the list of body literals in the new metasubstitution.
%	It is a list of lists, where each sub-list is of the form
%	[F|As], representing an atom of a predicate with symbol F and
%	where As are the terms of the atom.
%
%	new_metasub/7 fails if the length of Metasubs is equal to (or
%	larger than) Depth, which is when the learned hypothesis (i.e.
%	the accumulator of metasubstitutions) has reached the maximum
%	hypothesis size for the current search iteration.
%
new_metasub(K,Msubs,A,MS,Os,Msubs_,Bs):-
	length(Msubs,N)
	,N < K
	,metasubstitution(MS,A,Os,Msub,Bs)
	,save_metasub(Msub,Msubs,Msubs_)
	,debug(new_metasub, 'Saved new metasub: ~w', [Msub]).


%!	metasubstitution(+Metarules,+Atom,+Signature,?Metasub,-Body) is
%!	nondet.
%
%	Perform a second-order metasubstitution.
%
%	Metarules is a list of constants, the names of metarules for a
%	learning target.
%
%	Atom is an atom represented as a list, [F|As], where F the
%	symbol of the atom's predicate and As the atom's list of terms.
%
%	Signature is the program signature.
%
%	Metasub is a Prolog compound sub(Id,Hs), where Id is the id of a
%	metarule and Hs is a list of second-order variables, to be
%	eventually bound to the predicate symbols and arities in
%	Signature.
%
%	Body is a list of atoms in list form, [F|As], the body literals
%	of the clause resulting from the projection fo Metasub to one of
%	the metarules named in Metarules.
%
%	See project_metasubs/2 for an explanation of how a
%	metasubstitution is projected onto a metarule to produce a
%	clause.
%
metasubstitution(MS,[S|Args],PS-Cs,sub(Id,[S/A|Ss]),Bs):-
	atom_symbol_arity([S|Args],S/A)
	,next_metarule(MS,[Id,[S/A|Ss],Fs,[[S|Args]|Bs]])
	,second_order_bindings(PS,Fs,Ss)
	,configuration:order_constraints(Id,[S/A|Ss],Fs,STs,FTs)
	,order_tests(PS,Cs,STs,FTs).


%!	next_metarule(+Metarules,-Metarule) is nondet.
%
%	Select the next Metarule for Target.
%
%	Metarules is a list of constants, the names of metarules for a
%	learning target.
%
%	Metarule is a Prolog compound of the form:
%	==
%	F(Id, Ss, Fs, Bs)
%	==
%
%	Where F is the metarule symbol defined in metarule_functor/1; Id
%	is the name of a metarule in Metarules; Ss is the list of
%	existentially quantified terms in the metarule; Fs is the list
%	of universally quantified terms in the metarule; and Bs is a
%	list of atoms in list form, [F|As], the body literals in the
%	metarule.
%
next_metarule(MS,[Id,Ss,Fs,Bs]):-
	metarule_functor(F)
	,member(Id,MS)
	,M =.. [F,Id,Ss,Fs,Bs]
	,thelma:call(M).


%!	second_order_bindings(+Signature,+Constants,?Bindings) is
%!	nondet.
%
%	Bind second order terms to symbols in the Signature.
%
%	Signature is the program signature.
%
%	Constants is the constant signature, the list of constants in
%	the Herbrand universe of predicates in the background knowledge
%	ordered by interval inclusion order.
%
%	Bindings is a list of the existentially quantified terms in a
%	metarule. Each can be one of the following:
%	a) A term S/A, where both F and A are variables
%	b) A constant, or,
%	c) A single variable.
%
%	When a member of Bindings is a term S/A, S is bound to a symbol
%	and arity of a predicate in Signature. All possible bindings are
%	generated on backtracking.
%
%	When a member of Bindings is a constant, it is left alone.
%
%	When a member of Bindings is a single variable, if that variable
%	is also in the list Constants, it is understood to be a constant
%	in the learned hypothesis. All such variables must be together
%	at the end of the list Bindings.
%
%	The identity of two variables is tested in bound_constant/2 by
%	first skolemising all variables in Bindings and then checking
%	whether two skolemised terms match.
%
second_order_bindings(_,_,[]):-
	!.
second_order_bindings(PS,Fs,[C|_Ss]):-
% C is to be bound to a constant and so are all remaining terms.
	bound_constant(C,Fs)
	,!
	,second_order_bindings(PS,Fs,[]).
second_order_bindings(PS,Fs,[S/A|Ss]):-
	member(S/A,PS)
	,second_order_bindings(PS,Fs,Ss).


%!	bound_constant(+Term,+First_order)  is det.
%
%	True when a Term is a constant in the hypothesis.
%
%	Term is taken from the set of existentially quantified terms in
%	a metarule. Such a term is a constant in the hypothesis if it
%	is an atomic Prolog term, or if it is a variable that is also in
%	the set of First_order variables in the metarule.
%
%	If Term is a variable, to test that it is included in
%	First_order, first Term and First_order are copied to a list
%	[Term_1|First_order_1]. Then, this list is skolemised (by
%	numbervars/3). Finally, First_order_1 is searched for Term_1.
%	bound_constant/2 succeeds only if Term is a constant, or if
%	Term_1 is in First_order_1.
%
bound_constant(C,_Fs):-
	atomic(C)
	,!.
bound_constant(C,Fs):-
	copy_term([C|Fs],[C_|Fs_])
	,numbervars([C_|Fs_])
	,memberchk(C_,Fs_).


%!	atom_symbol_arity(+Atom,-Predicate) is det.
%
%	Figure out the symbol and arity of an Atom given as a list.
%
%	Atom is an atom represented as a list, [F|As], where F the
%	symbol of the atom's predicate and As the atom's list of terms.
%
%	Predicate is a predicate indicator, F/A, where the predicate
%	symbol, F, is the predicate symbol in Atom and the arity, A, is
%	the length of the list of terms, As, in Atom.
%
atom_symbol_arity([A|As],A/N):-
	length(As,N).


%!	order_tests(+Predicates,+Constants,+First_Order,+Second_Order)
%!	is det.
%
%	Test the order constraints associated with a metarule.
%
%	Predicates is the program signature.
%
%	Constants is the constant signature.
%
%	First_order and Second_order are the lexicographic and interval
%	inclusion order constraints imposed by a metarule.
%
order_tests(_,_,[],[]):-
	!.
order_tests(PS,_,STs,_):-
	STs \= []
	,!
	,ordered_list(STs,PS).
order_tests(_,CS,_,FTs):-
	FTs \= []
	,ordered_list(FTs,CS).


%!	ordered_list(?List,+Ordering) is det.
%
%	A Sublist order according to a total Ordering of its elements.
%
ordered_list([X>Y],Os):-
	above(X,Y,Os)
	,!.
ordered_list([X>Y|Ls],Os):-
	above(X,Y,Os)
	,ordered_list(Ls,Os).


%!	above(?Above,+Below,+Ordering) is det.
%
%	True when Above is above Below in a total Ordering.
%
above(S1,S2,Ss):-
	previous(S1,S2,Ss)
	,!.
above(S1,S3,Ss):-
	previous(S1,S2,Ss)
	,!
	,above(S2,S3,Ss).
above(S1,S2,[_|Ss]):-
	above(S1,S2,Ss).


%!	previous(?First,?Next,?List) is det.
%
%	True when First and Next are the first two elements of List.
%
previous(S1,S2,[S1,S2|_Ss]).


%!	save_metasub(+Metasub,+Metasubs,-Metasubs_new) is det.
%
%	Add a new Metasubstitution to the list of Metasubstitutions.
%
%	Metasub is a metasubstitution in the form sub(Id, Ps), where Id
%	is the name of a metarule and Ps a list of symbols and arities
%	of predicates in the program signature.
%
%	Metasubs is the list of metasubstitutions accumulated so far by
%	prove/7.
%
%	Metasubs_new is the list [Metasub|Metasubs], if Metasub is not
%	already in Metasubs; otherwise, save_metasub/3 fails.
%
save_metasub(MS,Prog,[MS|Prog]):-
	save_metasub(MS,Prog).

%!	save_metasub(+Metasub,+Metasubs) is det.
%
%	Business end of save_metasub/3.
%
%	True when Metaub is not in Metasubs.
%
save_metasub(_,[]):-
	!.
save_metasub(MS1,[MS2|Prog]):-
	MS1 \== MS2
	,save_metasub(MS1,Prog).



%!	disprove(+Atoms,+BK,+Program) is det.
%
%	True when a Program does not cover negative examples.
%
%       Atoms is a list of negative examples of one or more learning
%       targets. It is a list of negated atoms, :-E1, :-E2, etc.
%
%       BK is a list of predicate indicators declared as bakcground
%       knowledge for the predicates of the negative examples.
%
%	Program is a list of definite datalog clauses, a hypothesis
%	formed by a call to prove/5.
%
%	disprove/2 fails iff an atom in Atoms is entailed by Program.
%	This is tested by first asserting Program to the dynamic
%	database and then proving each atom in Atoms with call/1.
%	Program is retracted from the database after the proof
%	completes.
%
disprove([],_BK,_Ms):-
% Skip further processing if there are no negative examples.
	!.
disprove(Neg,BK,Ps):-
	(   closure(BK,user,Bs)
	->  true
	;   closure(BK,experiment_file,Bs)
	;   throw('Missing BK definition of predicate in':BK)
	)
	,flatten(Bs,Bs_f)
	,examples_targets(Neg,Ts)
	,S  = (assert_program(program,Ps,Refs_Ps)
	      ,assert_program(program,Bs_f,Refs_Bs)
	      ,maplist(table_program,[Ts,Bs_f])
              )
        % Succeed if the program fails, otherwise fail;
	% Erase the newly asserted clauses eitherwise.
        ,G = forall(member(:-A,Neg)
                    ,(debug(thelma,'Disproving atom: ~w',[:-A])
                     ,\+ once(call(program:A))
                     )
                   )
        ,C = (erase_program_clauses(Refs_Ps)
	     ,erase_program_clauses(Refs_Bs)
	      ,maplist(untable_program,[Ts,Bs_f])
             )
	% Don't backtrack over disproofs
	% Particularly after erasing BK and program clauses.
	,!
        ,setup_call_cleanup(S,G,C).


%!	table_program(+Symbols) is det.
%
%	Table each of a list of predicates given their Symbols.
%
%	Used to prevent BK and target predicates from going infinite
%	while trying to disprove negative examples.
%
table_program(Ss):-
	forall(member(F/A,Ss)
	      ,table(program:F/A)
	      ).


%!	untable_program(+Symbols) is det.
%
%	Untable a list of predicates given their Symbols.
%
%	Counterpart to table_program/1, untabling BK and target
%	predicates after we're done disproving them.
%
untable_program(Ss):-
	forall(member(F/A,Ss)
	      ,untable(program:F/A)
	      ).



%!	project_metasubs(+Metasubstitutions,-Program) is det.
%
%	Project a list of Metasubstitutions to a Program.
%
%	Metasubstitutions is a list of metasubstitutions accumulated by
%	prove/7, in the form sub(Id, Ps), where Id is the name of a
%	metarule and Ps is a list of existentially quantified terms
%	in the metarule. Ps may include the symbols and arities of
%	predicates in the signature, or constants.
%
project_metasubs(Ms,Prog):-
	findall(C
		,(member(Mi,Ms)
		 ,project_metasub(Mi,C)
		 )
	       ,Prog).


%!	project_metasub(+Metasubstitution,-Clause) is det.
%
%	Porject a second-order Metasubstitution to a definite Clause.
%
%	Metasubstitution is a metasubstitution in the form sub(Id, Ps),
%	where Id is the name of a metarule and Ps is a list of
%	existentially quantified terms in the metarule. Ps may include
%	the symbols and arities of predicates in the signature, or
%	constants.
%
%	Clause is the projection of Metasubstitution onto the named
%	metarule, binding the members of Ps to the existentially
%	quantified terms in the metarule.
%
%	For example, suppose Metasubstitution is as follows:
%	==
%	sub(chain, [grandfather/2,father/2,parent/2]).
%	==
%
%	Projecting this metasubstitution onto the chain metarule will
%	produce the following clause:
%	==
%	grandfather(A,B):- father(A,C), parent(C,B).
%	==
%
project_metasub(sub(Id,Ss),C):-
	metarule_functor(F)
	,M =.. [F,Id,Ss,_Fs,Bs]
	,call(thelma:M)
	,project_metasub(Bs,[],Ls)
	,literals_list_to_clause(Ls,C).


%!	project_metasub(+Literals,+Acc,-Atoms) is det.
%
%	Business end of project_metasub/2.
%
%	Literals is a list of lists representing literals.
%
%	Atoms is a list of atoms, derived from the literals in the list.
%
project_metasub([],Ps,Ps_):-
	reverse(Ps, Ps_).
project_metasub([L|Ls],Acc,Ps):-
	L_ =.. L
	,project_metasub(Ls,[L_|Acc],Ps).


%!	literals_list_to_clause(+Literals,-Clause) is det.
%
%	Transforma  a list of Literals to a Clause.
%
%	Literals is a list of lists representing literals.
%
%	Clause is a definite clause, H:-B where the head literal, H, is
%	the first literal in Literals and the body literals, B, are the
%	remaining literals in Literals.
%
literals_list_to_clause([H|[]],H):-
% No body literals
	!.
literals_list_to_clause([H|[B]],(H:-B)):-
% One body literal
	!.
literals_list_to_clause([H|Ls],(H:-Bs)):-
% A vector of body literals that should be turned into a list.
	once(list_tree(Ls,Bs)).

