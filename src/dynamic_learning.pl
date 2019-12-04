:-module(dynamic_learning, [learn_dynamic/1
			   ,learn_dynamic/2
			   ,learn_dynamic/5
			   ]).

:-use_module(src(mil_problem)).
:-use_module(src(auxiliaries)).
:-use_module(src(louise)).

/** <module> Predicates for dynamic learning with predicate invention.

Dynamic learning differs to "static" learning (implemented by
learn/[1,2,5]) in that, as soon as a new generalising metasubstitution
is found (and given that the metasubstitution satisfies any constraints)
it is applied to its corresponding metarule and written to the dynamic
database so it can be re-used by further Top program construction steps.

This allows the construction of sets of metasubstitutions that, applied
to their metarules, result in sets of connected clauses. Two clauses, C
and D, are connected, iff one or more body literals of C unify with the
head literal of D.

In order to create sets of connected clauses, dynamic learning performs
predicate invention. An invented predicate's symbol is then used in a
subsequently constructed clause, to form a connected pair of clauses.
More than two clauses can be connected in this way. It is possible both
to connect one clause to multiple other clauses, or to connect multiple
clauses in pairs in the same hypothesis.

Besides making heavy use of the dynamic database, dynamic learning
predicates also perform destructive updates of a counter used to keep
track of the number of invented predicates and also to construct their
names by indexing the atom '$' with the current highest invented
predicate index + 1.

Dynamic learning essentially updates the background knowledge by adding
each clause in the Top program (of the origional target or an invented
predicate) to the dynamic database as soon as it is constructed. Because
of this, it relies very strongly on the order in which examples are
given. For example, given the first ordering of examples, below, 'S'/2
is learnable with dynamic learning, but given the second ordering, it is
not:

==
% 'S'([a,b],[]) must be found first before all other examples otherwise
% learning 'S'/2 with dynamic learning will fail
positive_example('S'/2,E):-
	member(E, ['S'([a,b],[])
		  ,'S'([a,a,b,b],[])
		  ,'S'([a,a,a,b,b,b],[])
		  ]).

?- learn_dynamic('S'/2).
'$1'(A,B):-'S'(A,C),'B'(C,B).
'S'(A,B):-'A'(A,C),'$1'(C,B).
'S'(A,B):-'A'(A,C),'B'(C,B).
true.

% This ordering makes 'S'/2 impossible to learn precisely with dynamic
% learning:
positive_example('S'/2,E):-
	member(E, ['S'([a,a,b,b],[])
		  ,'S'([a,b],[])
		  ,'S'([a,a,a,b,b,b],[])
		  ]).

?- learn_dynamic('S'/2).
'S'([a,a,b,b],[]).
'S'([a,a,a,b,b,b],[]).
'S'(A,B):-'A'(A,C),'B'(C,B).
true.
==
*/


%!	learn_dynamic(+Target) is det.
%
%	Learn a program for a Target using dynamic learning.
%
learn_dynamic(T):-
	learn_dynamic(T,Ps)
	,print_clauses(Ps).



%!	learn_dynamic(+Target,-Program) is det.
%
%	Learn a Program for a Target using dynamic learning.
%
learn_dynamic(T,_Ps):-
	(   var(T)
	->  throw('learn_dynamic/2: unbound target symbol!')
	;   fail
	).
learn_dynamic(T, Ps):-
	experiment_data(T,Pos,Neg,BK,MS,false)
	,learn_dynamic(Pos,Neg,BK,MS,Ps).



%!	learn_dynamic(+Pos,+Neg,+BK,+MS,-Program) is det.
%
%	Learn a Program using dynamic learning.
%
%	@tbd This makes heavy use of the dynamic database and of
%	destructive updates with setarg/3. It is also terribly
%	inefficient and a bit of a mess because it's basically
%	copy/pasta from learn/5 and its auxiliaries in louise module. It
%	would really be nice to have a more efficient and less dirty
%	version. Perhaps even one so pure that SLG resolution can be
%	used with it.
%
learn_dynamic(Pos,Neg,BK,MS,_Ts):-
	(   var(Pos)
	->  throw('learn_dynamic/5: unbound positive examples list!')
	;   var(Neg)
	->  throw('learn_dynamic/5: unbound negative examples list!')
	;   var(BK)
	->  throw('learn_dynamic/5: unbound background symbols list!')
	;   var(MS)
	->  throw('learn_dynamic/5: unbound metarule IDs list!')
	;   fail
	).
learn_dynamic(Pos,Neg,BK,MS,Ps):-
	configuration:max_invented(I)
	,C = c(1,I)
	,debug(learn,'Encapsulating problem',[])
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,debug(dynamic,'Constructing dynamic Top program...',[])
	,top_program_dynamic(C,Pos_,Neg_,BK_,MS_,Ms)

	,debug(learn,'Reducing dynamic Top program...',[])
	,reduced_top_program(Pos_,BK_,MS_,Ms,Rs)
	,examples_target(Pos,T)
	,debug(learn,'Excapsulating hypothesis',[])
	,excapsulated_clauses(T,Rs,Ps).


%!	top_program_dynamic(+Counter,+Pos,+Neg,+BK,+MS,-Top_Progam) is
%!	det.
%
%	Construct the Top_Progam for a MIL problem dynamically.
%
top_program_dynamic(C,Pos,Neg,BK,MS,Ts):-
	configuration:theorem_prover(resolution)
	,!
	,write_program(Pos,BK,Refs)
	,top_program_dynamic(C,Pos,Neg,MS,Ms)
	,applied_metarules(Ms,MS,Ts)
	,erase_program_clauses(Refs).


%!	write_program(+Pos,+BK,+PS,-Refs) is det.
%
%	Write an encapsulated MIL problem to the dynamic database.
%
%	@tbd The negative examples and metarules don't need to be
%	written to the dynamic database.
%
write_program(Pos,BK,Rs):-
	findall(Rs_i
		,(member(P, [Pos,BK])
		 ,assert_program(user,P,Rs_i)
		 )
		,Rs_)
	,flatten(Rs_,Rs).


%!	top_program(+Positive,+Negative,+BK,+Metarules,-Metasubstitutions)
%!	is det.
%
%	Collect all correct Metasubstitutions in a MIL problem.
%
top_program_dynamic(C,Pos,Neg,MS,Ss):-
	generalise(C,Pos,Neg,MS,Ss_Pos)
	,specialise(Ss_Pos,Neg,Ss).


%!	generalise(+Positive,+Metarules,-Generalised) is det.
%
%	Generalisation step of Top program construction.
%
%	Generalises a set of Positive examples by finding each
%	metasubstitution of a metarule that entails a positive example.
%
%	Generalised is a set of key-value pairs where the keys are
%	ground metasubstitution atoms and the values are a copy with
%	free variables of the encapsulated head and body literals of the
%	metarule corresponding to the metasubsitution.
%
generalise(C,Pos,Neg,MS,Ss_Pos):-
	debug_clauses(dynamic,'Generalising positive examples:', Pos)
	,findall(H-M
	     ,(member(M,MS)
	      ,copy_term(M,M_)
	      ,member(Ep,Pos)
	      ,generalising_metasubstitutions(C,Ep,M_,Neg,MS,Hs)
	      ,member(H, Hs)
	      )
	     ,Ss_Pos_)
	,sort(1,@<,Ss_Pos_,Ss_Pos)
	,debug_clauses(dynamic,'Generalised Top program',Ss_Pos).


%!	specialise(+Generalised,+Negatives,-Specialised) is det.
%
%	Specialisation step of Top program construction.
%
%	Specialises a set of metasubstitutions generalising the positive
%	examples against the Negative examples by discarding each
%	metasubstitution that entails a negative example.
%
specialise(Ss_Pos,Neg,Ss_Neg):-
	setof(H
	     ,H^M^Ss_Pos^En^Neg^
	      (member(H-M,Ss_Pos)
	      ,\+((member(:-En,Neg)
		  ,specialising_metasubstitution(En,M,H)
		  )
		 )
	      )
	     ,Ss_Neg).


%!	generalising_metasubstitution(+Cntr,+Ex,+Metarule,+Neg,+Rules,-Metasubs)
%	is nondet.
%
%	Find generalising metasubstitutions of a Metarule.
%
%	Cntr is a compound c(I,K) where I is the current highest index
%	of an invented predicate's name and K is the maximum number of
%	invented predicates to attempt to, well, invent.
%
%	Ex is a positive example, a ground definite unit clause.
%
%	Metarule is a non-ground, expanded metarule.
%
%	Neg is the set of negative examples and MS the set of metarules
%	in a MIL problem.
%
%	Metasubs is a set of metasubstitutions of Metarule such that the
%	first predicate symbol in each metasubstitution in Metasubs is
%	either the predicate symbol of Ex, or an invented predicate
%	symbol.
%
%	Generalising metasubstitutions does a lot of the heavy lifting
%	(and heave it is) of predicate invention. It returns a list of
%	metasubstitutions because a list might have been found in the
%	Top program construction step that may be taken in order to
%	produce invented predicates. It also checks that each
%	metasubstitution in Metasubs conforms to any metarule
%	constraints. Finally, it applies the metasubstitutions in
%	Metasubs to their metarules and _writes the result to the
%	dynamic database_ so that it may be reused later to learn
%	connected clauses and whatnot.
%
generalising_metasubstitutions(C,E,M,Neg,MS,Ss):-
	debug_clauses(dynamic,'Generalising example',[E])
	,bind_head_literal(E,M,(Sub:-(E,Ls)))
	,prove_body_literals(C,Ls,Neg,MS,Subs)
	,findall(S
		,(member(S,[Sub|Subs])
		 ,constraints(S)
		 )
		,Ss)
	,applied_metarules(Ss,MS,Cs)
	,assert_program(user,Cs,_Refs)
	,debug_clauses(dynamic,'Asserted new clauses:',Cs).


%!	bind_head_literal(+Example,+Metarule,-Head) is det.
%
%	Bind an Example to the encapsulated Head literal of a Metarule.
%
%	Abstracts the complex patterns of binding examples to the heads
%	of metarules with and without body literals.
%
bind_head_literal(E,M,(H:-(E,Ls))):-
	M = (H:-(E,Ls))
	,!.
bind_head_literal(E,M,(H:-(E,true))):-
	M = (H:-E).


%!	prove_body_literals(+Cntr,+Literals,+Neg,+Metarules,-Metasubs)
%!	is nondet.
%
%	Prove the body Literals of a partially instantiated metarule.
%
prove_body_literals(C,Ls,Neg,MS,Ss):-
	once(list_tree(Ls_,Ls))
	,debug_clauses(dynamic,'Proving body literals:', [Ls])
	,prove_body_literals(C,Ls_,Neg,MS,[],Ss).

%!	prove_body_literals(+Cntr,+Literals,+Neg,+Metarules,+Acc,-Metasubs)
%!	is nondet.
%
%	Business end of prove_body_literals/5.
%
%	Follows more or less the following procedure:
%
%	1) For each literal, l, in Literals
%	2) Try to prove l against the BK and examples
%	3) If that fails, try to invent a new predicate
%	4) Return all invented metasubstitutions
%
%	Inventing metasubstitutions is done by Top program construction
%	where the set of positive examples contains only a single
%	literal, a literal that could not be proven given the examples
%	and background knowledge.
%
%	Note that the distantly-recursive Top program construction step
%	in the second clause of this predicate goes through
%	generalising_metasubstitutions/6, so each metasubstitution found
%	by it is _written to the dynamic database_ so that it can be
%	reused later.
%
prove_body_literals(_C,[],_Neg,_MS,Acc,Ss):-
	flatten(Acc,Ss).
prove_body_literals(C,[L|Ls],Neg,MS,Acc,Bind):-
% L is an example atom or entailed by the BK and examples.
	debug_clauses(dynamic,'Proving literal:',[L])
	,user:call(L)
	,debug_clauses(dynamic,'Proved literal:',[L])
	,prove_body_literals(C,Ls,Neg,MS,Acc,Bind).
prove_body_literals(C,[L|Ls],Neg,MS,Acc,Bind):-
% Try inventing a new predicate that entails L.
	\+ user:call(L)
	,debug_clauses(dynamic,'Failed proving literal:',[L])
	,new_atom(C,L,L_)
	,debug_clauses(dynamic,'Generating new atom:',[L_])
	,top_program_dynamic(C,[L_],Neg,MS,Ss)
	,debug_clauses(dynamic,'Invented clauses:',Ss)
	,prove_body_literals(C,Ls,Neg,MS,[Ss|Acc],Bind).


%!	new_atom(+Counter,+Atom,-New) is det.
%
%	Index an Atom to create a New, invented symbol.
%
new_atom(C,L,L_):-
	L =.. [m,P|As]
	,new_symbol(C,P)
	,L_ =.. [m,P|As].

%!	new_symbol(+Counter,?Symbol) is det.
%
%	Bind a Symbol variable to a new, invented Symbol.
%
%	Counter is _destructively updated_ by a call to setarg/3 (and
%	_not_ to nb_setarg/3) in this predicate.
%
new_symbol(C,P):-
	arg(1,C,I)
	,arg(2,C,K)
	,I =< K
	,atomic_list_concat(['$',I],'',P)
	,succ(I, I_)
	,setarg(1,C,I_).


%!	specialising_metasubstitution(+Example,-Metarule,+Metasubstitution)
%	is nondet.
%
%	Specialise a Metasubstitution against a negative Example.
%
%	Example is a negative example, a ground definite goal (i.e. a
%	clause of the form :-Example).
%
%	Metarule is a non-ground metarule. This is actually the output
%	argument (should go last; was second because this was copied
%	from metasubstitution/3 where this makes sense).
%
%	Metasubstitution is a ground metasubstitution of Metarule.
%	Metarule is ground according to the terms in Metasubstitution.
%
specialising_metasubstitution(E,M,Sub):-
	bind_head_literal(E,M,(Sub:-(E,Ls)))
	,user:call(Ls).
