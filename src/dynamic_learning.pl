:-module(dynamic_learning, [learn_dynamic/1
			   ,learn_dynamic/2
			   ,learn_dynamic/5
			   ,top_program_dynamic/4
			   ,unfold_invented/3
			   ,pseudo_unfold_invented/3
			   ,reduce_unfolded/2
			   ]).

:-use_module(configuration).
:-use_module(src(mil_problem)).
:-use_module(src(auxiliaries)).
:-use_module(src(louise)).
:-use_module(src(subhypothesis_selection)).
:-use_module(lib(lifting/lifting)).
:-use_module(lib(term_utilities/term_utilities)).

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
predicate invention. When proving a body literal of a metarule during
the generalisation step of Top program construction fails, a new Top
program is constructed, recursively, with that literal as the only
positive example and with a new, invented predicate symbol. If a
non-empty Top program is constructed this way, it is taken as a
definition of the invented predicate.

An invented predicate's symbol is then used in a subsequently
constructed clause, to form a connected pair of clauses. More than two
clauses can be connected in this way. The ability to connect clauses
allows learning hypotheses that could otherwise only be learned by
specifying long metarules, with a potentially large number of body
literals.

Besides making heavy use of the dynamic database, dynamic learning
predicates also perform destructive updates of a counter used to keep
track of the number of invented predicates and also to construct their
names by indexing the atom '$' with the current highest invented
predicate index + 1.

Dynamic learning essentially updates the background knowledge by adding
each clause in the Top program (of the original target or an invented
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


%!	learn_dynamic(+Targets) is det.
%
%	Learn a program for one or more Targets using dynamic learning.
%
learn_dynamic(Ts):-
	learn_dynamic(Ts,Ps)
	,print_clauses(Ps).



%!	learn_dynamic(+Targets,-Program) is det.
%
%	Learn a Program for one more more Targets with dynamic learning.
%
learn_dynamic(Ts,_Ps):-
	(   \+ ground(Ts)
	->  throw('learn_dynamic/2: non-ground target symbol!')
	;   fail
	).
learn_dynamic(Ts,Ps):-
	experiment_data(Ts,Pos,Neg,BK,MS)
	,learn_dynamic(Pos,Neg,BK,MS,Ps).



%!	learn_dynamic(+Pos,+Neg,+BK,+MS,-Program) is det.
%
%	Learn a Program using dynamic learning.
%
%	@tbd This makes heavy use of the dynamic database and of
%	destructive updates with setarg/3.
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
	,examples_targets(Pos,Ss)
	,table_encapsulated(Ss)
	,debug(learn,'Constructing dynamic Top program...',[])
	,top_program_dynamic(C,Ss,Pos_,Neg_,BK_,MS_,Ts)
	,untable_encapsulated(Ss)
	,debug(learn,'Reducing dynamic Top program...',[])
	,reduced_top_program_dynamic(Pos_,BK_,MS_,Ts,Rs)
	,debug(learn,'Excapsulating hypothesis',[])
	,excapsulated_clauses(Ss,Rs,Ps).


%!	table_encapsulated(+Targets) is det.
%
%	Prepare one or more encapsulated learning Targets for tabling.
%
%	Targets is a list of predicate indicators, F/A, the predicate
%	symbols and arities of a list of target predicates.
%	table_encapsulated/1 prepares for tabling each predicate m/N,
%	where N is A+1 for each target predicate in Targets. m/N is the
%	predicate encapsulating each Target (though not necessarily
%	_only_ target predicates).
%
%	Motivation
%	----------
%
%	Tabling the predicates encapsulating each learning target is a
%	very convenient way to avoid entering infinite recursion during
%	dynamic learning. Because dynamic learning adds to the dynamic
%	database clauses of the Top program learned in a previous
%	iteration, so that they may be called in subsequent iterations,
%	if a recursive clause is added to the dynamic database before a
%	suitable "base case" for the recursion is added, it's likely
%	that the learning procedure will enter an infinite recursion.
%	Tabling, a.k.a. SLG resolution, avoids this and guarantees
%	termination by precomputing the result of recursive calls and
%	storing them in a table, so that execution does not have to
%	descend infinitely, like it does with ordinary SLDNF resolution.
%
%	@tbd Tabling is a new feature in Swi-Prolog and it may change
%	substantially in the future. For the time being, this predicate
%	works as intended. If tabling changes, this may have to change
%	also.
%
table_encapsulated(Ts):-
	forall(member(_F/A,Ts)
	      ,(succ(A,A_)
	       ,table(user:m/A_)
	       )
	      ).


%!	untable_encapsulated(+Targets) is det.
%
%	Remove tabling from all encapsulated learning Targets.
%
%	This is the counterpart to table_encapsulated/1.
%
%	Targets is a list of predicate indicators, F/A, the predicate
%	symbols and arities of each target predicate.
%	untable_encapsulated/1 removes tabling from each predicate m/N,
%	where N is A+1 for each predicate in Targets. m/N is the
%	predicate encapsulating each Target (though not necessarily
%	_only_ target predicates).
%
untable_encapsulated(Ts):-
	forall(member(_F/A,Ts)
	      ,(succ(A,A_)
	       ,untable(user:m/A_)
	       )
	      ).



%!	top_program_dynamic(+Counter,Targets,+Pos,+Neg,+BK,+MS,-Top_Progam)
%!	is det.
%
%	Construct the Top_Progam for a MIL problem dynamically.
%
top_program_dynamic(C,Ss,Pos,Neg,BK,MS,Ts):-
	configuration:theorem_prover(resolution)
	,S = write_program(Pos,BK,Refs)
	,G = (top_program_dynamic(C,Pos,Neg,MS)
	     ,collect_clauses(Ss,MS,Ts_)
	     ,unfold_clauses(Ts_,Ss,Ts)
	     )
	,Cl = erase_program_clauses(Refs)
	,setup_call_cleanup(S,G,Cl)
	,!.
top_program_dynamic(_C,_Ss,_Pos,_Neg,_BK,_MS,[]):-
% If Top program construction fails return an empty program.
	debug(top_program,'INSUFFICIENT DATA FOR MEANINGFUL ANSWER',[]).


%!	top_program_dynamic(+Counter,+Positive,+Negative,Metarules)
%!	is det.
%
%	Dynamically create the Top program for a MIL problem.
%
%	This predicate combines the generalisation and specialisation
%	step that are implemented by separate predicates in "static"
%	learning, in generalise/3 and specialise/3 in louise.pl.
%
%	top_program_dynamic/4 first attempts to generalise a positive
%	example using a metarule. The generalision attempt binds the
%	head of the metarule to the example and then proves each of the
%	body literals of the metarule, in turn, by resolution with the
%	positive examples and background knowledge.
%
%	If the proof of each body literal is successful, the resulting
%	metasubstitution is tested against constraints.
%
%	If the constraint test passes, the metasubstitution is
%	specialised against the negative examples. To specialise a
%	metasubstitution, first a negative example is bound to the head
%	of the metasubstitution, then its body literals are proven by
%	resolution against the positive examples and background
%	knowledge. If any such proof succeeds, which means that the
%	metasubstitution entails a negative example, the
%	metasubstitution is discarded.
%
%	During the generalisation step, if proof of a literal fails,
%	predicate invention is performed. This happens during a call to
%	prove_body_litearls_/3. The literal that cannot be proven given
%	the positive examples and background knowledge is given a new
%	predicate symbol, of the form '$n' where n is an integer from 1
%	to the maximum number of invented predicates set in the
%	configuration option max_invented/1. Then, a new Top program is
%	constructed with that literal as the only positive example, no
%	negative examples and the same background knowledge and
%	metarules as the original Top program construction step.
%
%	When a Top program construction step, i.e. the combination of
%	generalisation, specialisation and a constraint test, is
%	completed successfully, the resulting metasubstitution is first
%	applied to its corresponding metarule and written to the dynamic
%	database. Applying the metasubstitution to a metarule results
%	in a definite clause. The clause is encapsulated and so it
%	becomes immediately available to the next Top program
%	construction step, as a background predicate.
%
%	Metasubstitutions are later collected from the dynamic database
%	with a call to collect_clauses/3 and the dynamic database
%	cleaned of all encapsulated clauses.
%
top_program_dynamic(I,Pos,Neg,MS):-
% A findall loop is the easiest, simplest way to do this.
% Seriously. Tried a forall/2 loop but it ends up horribly convoluted
% with a ton of conditionals that are also hard to read. So, forget it.
        findall(C
               ,(member(M,MS)
                ,copy_term(M,M_1)
                ,member(Ep,Pos)
		,debug_clauses(dynamic,'Generalising example:',[Ep])
                ,metasubstitution(I,Ep,M_1,MS,Sub)
		,debug_clauses(dynamic,'Derived metasub:',[Sub])
                ,constraints(Sub)
		% M_1 is now partially bound to Ep's constants.
		,copy_term(M,M_2)
                ,\+((member(En,Neg)
		    ,debug_clauses(dynamic,'Specialising metasub:',[Sub])
                    ,metasubstitution(I,En,M_2,MS,Sub)
                    ))
		,metarule_application(Sub,M,C)
		,assert_program(user,[C],_Rs)
		,debug_clauses(dynamic,'Asserted clause',[C])
                )
               ,Cs)
        %Fail if Top program is empty.
	,Cs \= []
	,debug_clauses(top_program,'Generalised and specialised Top program',Cs).


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
metasubstitution(_C,:-E,M,_MS,Sub):-
% No need for predicate invention if E is negative.
	bind_head_literal(E,M,(Sub:-(E,Ls)))
	,debug_clauses(metasubstitution,'Trying metasubstitution:',Ls)
	,user:call(Ls)
	,debug(metasubstitution,'Succeeded',[]).
metasubstitution(C,E,M,MS,Sub):-
	bind_head_literal(E,M,(Sub:-(E,Ls)))
	,debug_clauses(metasubstitution,'Trying metasubstitution:',Ls)
	,prove_body_literals(C,MS,Ls).


%!	bind_head_literal(+Example,+Metarule,-Head) is det.
%
%	Bind an Example to the encapsulated Head literal of a Metarule.
%
%	Abstracts the complex patterns of binding examples to the heads
%	of metarules with and without body literals.
%
%	@tbd Copied unchanged from louise module.
%
bind_head_literal(E,M,(H:-(E,Ls))):-
	M = (H:-(E,Ls))
	,!.
bind_head_literal(E,M,(H:-(E,true))):-
	M = (H:-E).


%!	prove_body_literals(+Counter,+Metarules,+Literals) is nondet.
%
%	Prove the body Literals of a partiallly instantiated metarule.
%
%	Counter is the invented predicate counter, counting from 0 to
%	the maximum number of invented predicates allowed for a MIL
%	problem.
%
%	Metarules is the list of expanded metarules in the current MIL
%	problem. They are used to construct a new Top program when the
%	proof of a literal in Literals fails.
%
%	Literals is a list of at least partially instantiated body
%	literals of a metarule.
%
%	This predicate performs predicate invention. First, each literal
%	is Literals is resolved with the background knowledge and
%	positive examples. If resolution fails, a new, invented symbol
%	is created and bound to the existentially quantified
%	second-order variable in the literal (if it's unbound). Then, a
%	new Top program is constructed with the new literal as its only
%	positive example, no negative examples, and with the same
%	positive examples, background knowledge and metarules as in the
%	original MIL problem (the examples and BK are already in the
%	dynamic database, so only the list of Metarules need to be
%	passed down as an argument).
%
prove_body_literals(C,MS,Ls):-
	once(list_tree(Ls_,Ls))
	,debug_clauses(dynamic,'Proving body literals:',[Ls_])
	,prove_body_literals_(C,MS,Ls_).

%!	prove_body_literals_(+Counter,+Metarules,+Literals) is nondet.
%
%	Business end of prove_body_literals/3.
%
prove_body_literals_(_C,_MS,[]).
% Has to be nondet to allow backtracking over all proofs.
prove_body_literals_(C,MS,[L|Ls]):-
	debug_clauses(dynamic,'Proving literal:',[L])
	,user:call(L)
	,debug_clauses(dynamic,'Proved literal:',[L])
	,prove_body_literals_(C,MS,Ls).
prove_body_literals_(C,MS,[L|Ls]):-
	\+ user:call(L)
	,debug_clauses(dynamic,'Failed proving literal:',[L])
	,new_atom(C,L,L_)
	,debug_clauses(dynamic,'Invented new predicate:',[L_])
	,top_program_dynamic(C,[L_],[],MS)
	,prove_body_literals_(C,MS,Ls).


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
%	Counter is a counter, c(I) counting the number of invented
%	predicates defined so far.
%
%	Symbol is a second-order existentially quantified variable to be
%	bound to an invented predicate symbol.
%
%	new_symbol/2 is responsible for creating a new invented
%	predicate symbol of the form '$I', where n is the value in
%	Counter.
%
%	Counter is _destructively updated_ by a call to setarg/3 (and
%	_not_ to nb_setarg/3) in this predicate. Invented symbols are
%	created before an attempt is made to construct a definition for
%	them and if a (non-empty) definition cannot be constructed, the
%	same symbol (i.e. a symbol with the current index in Counter as
%	an index) must be available for a subsequent attempt to define a
%	different invented predicate. Therefore, new_symbol/2 must be
%	allowed to backtrack over destructive assignment and
%	re-crate a Symbol with an index previously tried (and failed).
%
new_symbol(C,P):-
	arg(1,C,I)
	,arg(2,C,K)
	,I =< K
	,atomic_list_concat(['$',I],'',P)
	,succ(I, I_)
	,setarg(1,C,I_).


%!	metarule_application(+Metasubstitution,+Metarule,-Clause) is det.
%
%	Apply a Metasubstitution to its corresponding Metarule.
%
%	@tbd Can't we use this in the place of that cumbersome
%	applied_metarules/2, in mil_problem.pl?
%
metarule_application(Sub,Sub:-(H,B),H:-B).


%!	collect_clauses(+Targets,+Metarules,-Clauses) is det.
%
%	Collect all instances of Metarules with	a symbol in Targets.
%
%	Targets is the list of symbols and arities of each learning
%	target.
%
%	Metarules is the list of encapsulated metarules in a MIL
%	problem.
%
%	Clauses is the set of instances of the metarules in Metarules
%	where the predicate symbol is a symbol in Targets, or an
%	invented predicate.
%
%	@tbd This sorts the collected clauses at the end usig predsort/2
%	and unifiable_compare/3. This is not strictly necessary as only
%	unique clauses will be collected from the database. However, the
%	sorting orders clauses with the same predicate symbol so that
%	shorter clauses (i.e. the ones with the fewer literals) come
%	first. Ordering clauses in this way, by length, seems to make
%	program reduction easier, or at least it's possible to achieve
%	the same reduction strength with fewer resolutions. Program
%	reduction should still work regardless of the ordering of
%	clauses in hypotheses, but an inconvenient ordering will require
%	a higher number of resolutions. That's probably something to do
%	with how in recursive hypotheses the boundary condition will
%	tend to be the shortest clause. So consider the sorting a kind
%	of heuristic that will improve performance in the worst case and
%	probably not worsen it in the worst case. Also note that static
%	Top program construction sorts clauses - so the sorting also
%	ensures consistency between the two methods.
%
collect_clauses(Ts,MS,Cs):-
	findall(H:-B
	       ,(member(_A:-(H,B),MS)
		,clause(user:H,B)
		%,print_clauses('New clause',[H:-B])
		%,nl
		%,B \= true
		,H =.. [m,S|As]
		,length(As,N)
		,target_or_invention(Ts,S/N)
		,user:retract(H:-B)
		)
	       ,Cs_)
	,predsort(unifiable_compare,Cs_, Cs).


%!	unifiable_compare(-Delta,+A,+B) is det.
%
%	Comparison predicate for predsort/3.
%
%	If A and B unify Delta is =, otherwise compare(Delta,A,B) is
%	true.
%
%	Suggested by Boris on the Swi-Prolog mailing list.
%
%	@tbd Also used in evaluation module. Consider moving to
%	auxiliaries or some other library.
%
unifiable_compare(Delta, A, B) :-
    (   unifiable(A, B, _)
    ->  Delta = (=)
    ;   compare(Delta, A, B)
    ).


%!	unfold_clauses(+Clauses,+Targets,-Unfolded) is det.
%
%	Unfold Clauses to remove invented predicates.
%
%	Clauses is a set of Clauses learned by top_program_dynamic/4,
%	possibly including definitions of invented predicates.
%
%	Targets is a list of predicate symbols and arities of target
%	predicates in Clauses.
%
%	Unfolded is the list of clauses in Clauses unfolded to remove
%	invented predicates. See unfold_invented/3 for an explanation of
%	unfolding invented predicates.
%
%	The clauses in Clauses are only unfolded if the configuration
%	option unfold_invented/1 is set to "true". Otherwise, Unfolded
%	is bound to Clauses.
%
unfold_clauses(Cs,Ss,Us):-
	configuration:unfold_invented(true)
	,!
	,unfold_invented(Cs,Ss,Us).
unfold_clauses(Cs,_Ss,Cs):-
	configuration:unfold_invented(false).


%!	reduced_top_program_dynamic(+Pos,+BK,+Metarules,+Top_Program,-Redued)
%!	is det.
%
%	Reduce a dynamically learned Top_Program.
%
%	As reduced_top_program/5 but ensures that subhypothesis
%	reduction is correctly routed to subhypothesis_dynamic/4 that
%	knows how to deal with dynamically learned predicates with
%	possible invented clauses.
%
reduced_top_program_dynamic(Pos,BK,_MS,Ps,Rs):-
	configuration:reduction(subhypothesis)
	,!
	,debug(reduction,'Reducing Top program by subhypothesis selection...',[])
	,subhypothesis_dynamic(Pos,BK,Ps,Rs)
	,debug_clauses(reduction,'Reduced Top program:',Rs).
reduced_top_program_dynamic(Pos,BK,MS,Ps,Rs):-
	reduced_top_program(Pos,BK,MS,Ps,Rs).



%!	unfold_invented(+Program,+Targets,-Unfolded) is det.
%
%	Unfold a Program, removing invented predicates.
%
%	Program is a program learned by dynamic learning which may
%	include definitions of one or more invented predicates with
%	symbols such as '$1', '$2', etc. Program should be encapsulated
%	of unfold_invented/3 will fail.
%
%	Targets is the list of predicate symbols and arities of the
%	target predicates in Program, as F/A predicate indicators.
%
%	Unfolded is the set of resolvents of clauses of target
%	predicates in Programs with the clauses of invented predicates'
%	definitions. Unfolded does not include any invented predicates.
%
%	The following example should help clarify the above explanation:
%
%	==
%	?- unfold_invented(Bool).
%	Bool = false.
%
%	?- learn_dynamic('S'/2).
%	'$1'(A,B):-'S'(A,C),'B'(C,B).
%	'S'(A,B):-'A'(A,C),'$1'(C,B).
%	'S'(A,B):-'A'(A,C),'B'(C,B).
%	true.
%
%	?- unfold_invented(Bool).
%	Bool = true.
%
%	?- learn_dynamic('S'/2).
%	'S'(A,B):-'A'(A,C),'B'(C,B).
%	'S'(A,B):-'A'(A,C),'S'(C,D),'B'(D,B).
%	true.
%	==
%
%	Note that the positive examples and background knowledge _must_
%	be in the dynamic database and accessible from the dynamic
%	learning module, else unfold_invented/3 will fail.
%
%	Motivation
%	----------
%
%	The purpose of this predicate is to make programs learned with
%	dynamic learning more comprehensible by removing invented
%	predicates, while still maintaining programs' semantics.
%
%	Predicates invented by Louise with dynamic learning have
%	automatically assigned symbols such as '$1', '$2' etc, so that
%	their meaning is not immediately obvious. Although such
%	predicate can sometimes make a program _more_ comprehensible by
%	reducing its size, in general it's safe to assume that a program
%	without invented predicates will be at least as comprehensible
%	as a program of the same size and _with_ invented predicates.
%
%	The 'S'/2 example above is such a case. In the "Invented" form
%	of the program, it's hard to say what the predicate '$1'/2 is
%	meant to represent. Unfolding the program we obtain a version
%	without '$1'/2 that is closer to (if not identical with) a
%	proram that a human programmer would have written (indeed, the
%	proram is a classic definition of a grammar for the anbn
%	language).
%
%	Unfolding
%	---------
%
%	Unfolding is the dual of folding, a program transformation that,
%	in logic programs, replaces goals in the bodies of clauses with
%	the body literals of the "called" clauses.
%
%	Procedurally, unfolding resolves the head literal of a clause C,
%	with a body literal, L, of another clause, D, producing a new
%	clause with the head literal of C and the body literals of C and
%	D, except for L.
%
%	For example, suppose C = p:- q, r, D = q:- s, t and L = q. Then,
%	unfolding D onto L in C would result in a new clause, E = p:-
%	s,t,r.
%
%	As the 'S'/2 example in the prvious section demonstrates, if L
%	is the literal of an invented predicate in a clause of a target
%	predicate, unfolding will produce a new clause without any
%	literals of invented predicates.
%
%	Restricted unfolding
%	--------------------
%
%	Unlike general unfolding this predicate does not
%	indiscriminately generate all resolvents of the clauses of
%	target predicates and invented predicates. Instead, only those
%	resolvents that entail at least one positive example with
%	respect to background knowledge are constructed, i.e. the
%	process is driven by the positive examples.
%
%	For this reason, unfold_invented _cannot be used_ unless the
%	positive examples and the background knowledge for a MIL problem
%	from which Program was learned are in the dynamic database.
%
%	In other words, it is not possible to use unfold_invented/3 as a
%	stand-alone predicate. It _must_ be used as part of learning
%	with a dynamic learning predicate.
%
%	Limitations
%	-----------
%
%	This predicate does not take into account the negative examples
%	and instead generates the set of all resolvents of clauses in
%	Program that entail at least one positive example. As a result,
%	Unfolded _may_ include any number of clauses that entail some
%	negative examples. Whether this is really possible is to be
%	determined.
%
%	@tbd Currently, unfold_clauses/5 (auxiliary to this predicate
%	that does most of the actual unfolding) returns clauses ground
%	to constants obtained during a refutation of a positive example.
%	This is actually not that bad, because it incidentally forces
%	unfold_clauses/3 to succeed or fail exactly once for each
%	resolvent of a clause of a target predicate (whereas otherwise
%	it would potentially backtrack over all positive examples).
%	Unfortunately, this also requires the unfolded program to be
%	"lifted" to variabilise its constants. Since Program is
%	encapsulated, lifting it will variabilise the predicate symbols
%	in literals, which will result in a nonsensical program with
%	literals like m(P,X,Y,Z) where P should be a predicate symbol,
%	rather than a variable. To avoid this, we utilise a predicate
%	encapsulated_signature/2 to collect all constants in the
%	encapsulated input Program, which, since Program is
%	encapsulated, includes not only predicate symbols but also
%	constants of the learned theory. The whole process is a little
%	fiddly and not exactly the most efficient thing in the world,
%	but the alternative is even fiddlier and includes passing around
%	metasubstitutions and instantiating their existentially
%	quantified variables. While this is doable, honestly, I don't
%	have the heart to do it. Too much work. So unfolding a program
%	will be slow if we have too many clauses with too many damn
%	literals. OK.
%
unfold_invented(Ps,Ts,Us):-
	program_invented(Ps,Ts,Cs,Is)
	,invented_symbols_(Is,Ss)
	,!
	,unfold_clauses(Cs,Ss,Is,[],Us_)
	,flatten(Us_,Us_f)
	,encapsulated_signature(Ps,Ps_Cs)
	,lifted_program(Us_f,Ps_Cs,Us).
unfold_invented(Ps,Ts,Ps):-
	program_invented(Ps,Ts,_Cs,[]).


%!	program_invented(+Program,+Targets,-Clauses,-Invented) is det.
%
%	Partition a Program to clauses of target or Invented Predicates.
%
%	Program is a program learned by dynamic learning which may
%	include the definitions of one or more invented predicates.
%
%	Targets is the list of predicate symbols and arities of the
%	target predicates in Program, as F/A predicate indicators.
%
%	Clauses is the list of clauses of predicates in Targets, i.e.
%	clauses with head literals having the predicate symbol and arity
%	of a predicate indicator in Targets.
%
%	Invented is the list of clauses of invented predicates, i.e.
%	clauses with head literals having an invented predicate symbol
%	such as '$1', '$2', etc.
%
program_invented(Ps,Ts,Cs,Is):-
	program_invented(Ps,Ts,[],Cs_,[],Is_)
	,maplist(reverse,[Cs_,Is_],[Cs,Is]).

%!	program_invented(+Program,+Targets,+Acc1,-Clauses,+Acc2,-Invented)
%	is det.
%
%	Business end of program_invented/4.
%
program_invented([],_Ts,Cs,Cs,Is,Is):-
	!.
program_invented([C|Ps],Ts,Cs_Acc,Cs_Bind,Is_Acc,Is_Bind):-
	clause_of(C,Ts)
	,!
	,program_invented(Ps,Ts,[C|Cs_Acc],Cs_Bind,Is_Acc,Is_Bind).
program_invented([C|Ps],Ts,Cs_Acc,Cs_Bind,Is_Acc,Is_Bind):-
	program_invented(Ps,Ts,Cs_Acc,Cs_Bind,[C|Is_Acc],Is_Bind).


%!	clause_of(+Clause,+Signature) is det.
%
%	True when Clause is a clause of a predicate in Signature.
%
%	Clause is a definite clause. Signature is a list of predicate
%	symbols and arities.
%
%	A call clause_of(C,Ss) succeeds when the symbol and arity of the
%	head literal in clause C is S/A and S/A is in Ss.
%
clause_of(C,Ts):-
% Encapsulated clause
        clause_symbol(C,F/A)
	,memberchk(F/A,Ts).


%!	clause_symbol(+Clause,-Symbol) is det.
%
%	The predicate Symbol and arity of a Clause.
%
%	@tbd This could be useful elsewhere in the project. Consider
%	adding to auxiliaires, probably.
%
clause_symbol(H:-_B,F/N):-
% Encapsulated clause
	functor(H,m,_A)
	,H =.. [m,F|As]
	,length(As, N)
	,!.
clause_symbol(L,F/N):-
% Encapsulated unit clause
	functor(L,m,_A)
	,L =.. [m,F|As]
	,length(As,N)
	,!.
clause_symbol(H:-_B,F/A):-
% Definite clause
	functor(H,F,A)
	,!.
clause_symbol(L,F/A):-
% Unit clause
	functor(L,F,A).


%!	invented_symbols_(+Invented,-Symbols) is det.
%
%	Collect the Symbols of a list of clauses of Invented predicates.
%
%	Invented is a list of clauses of invented predicates'
%	definitions as returned by program_invented/4. Symbols is a list
%	of F/A terms where each F is the symbol of an invented predicate
%	in Invented and each A is its arity.
%
%	@tbd there is a predicate, invented_symbols/2 in the auxiliaries
%	module that does this kind of thing. The difference with this
%	program is that invented_symbols/2 generates invented symbols
%	directly from the configuration option max_invented/2 and so
%	there may be situations in which Invented may contain symbols
%	not generated by invented_symbols/2. This may happen, for
%	example, if unfold_invented/3 is called manually with some
%	arbitrary user-defined program, for testing. That said, do
%	consider using invented_symbols/2 instead of this predicate.
%
invented_symbols_(Is,Ss):-
	setof(F/A
	     ,B^Is^H^(member(H:-B,Is)
		     ,clause_symbol(H,F/A)
		     )
	     ,Ss).


%!	unfold_clauses(+Clauses,+Symbols,+Invented,+Acc,-Unfolded) is
%!	det.
%
%	Unfold a set of Clauses with a set of Invented definitions.
%
%	Clauses are the clauses of the target predicates collected from
%	a program learned by Louise with dynamic learning and that may
%	include one or more clauses with body litearls having invented
%	predicate symbols. Clauses will typically be returned by
%	program_invented/4 as its third argument.
%
%	Invented is a list of the clauses of definitions of invented
%	predicates originally in the same program as Clauses. Invented
%	will typically be the result returned by program_invented/4 as
%	its last argument.
%
%	Symbols is the set of predicate symbols and arities of
%	predicates in Invented.
%
%	Unfolded is the set of resolvents of each clause in Clauses with
%	the clauses in Invented. Unfolding results in a set of clauses
%	without any body literals of invented predicates.
%
unfold_clauses([],_Ss,_Is,Us,Us):-
	!.
unfold_clauses([C|Cs],Ss,Is,Acc,Bind):-
	findall(C_
	       ,unfold_clause(C,Ss,Is,C_)
	       ,Us)
	,unfold_clauses(Cs,Ss,Is,[Us|Acc],Bind).


%!	unfold_clause(+Clause,+Symbols,+Invented,-Unfolded) is nondet.
%
%	Auxiliary to unfold_clauses/5, operating on a single Clause.
%
%	Unfold a Clause with a list of Invented clauses.
%
unfold_clause(H:-B,Ss,Is,H:-B_):-
% The call to clause/2 binds the head of the clause to an example in the
% dynamic database. Remember that, since unfold_invented/3 is called at
% the end of a dynamic learning attempt, but _before_ the elements of a
% MIL problem are retracted from the dynamic database, the clauses of
% positive examples are still available. The clauses of the learned
% hypothesis are _not_ also still in the dynamic database - but even if
% they were, the "true" bound to the second argument of the clause
% restricts the search for clauses unifying with H only to unit clauses
% "facts"). Which are _probably_ examples.
%
% TODO: Actually, all of the above means that we can't process arbitrary
% definite clause examples. For those we'd have to modify this code
% slightly.
must_be(nonvar,H)
	,must_be(nonvar,B)
	,clause(H,true)
	,unfold_literals(B,Ss,Is,(H),U_)
	,treeverse(U_,(H,B_)).


%!	unfold_literals(+Literals,+Symbols,+Invented,+Acc,-Unfolded) is
%!	nondet.
%
%	Auxiliary to unfold_clause/4, operating on each of its Literals.
%
%	Unfold a literal with a list of Invented clauses.
%
unfold_literals(true,_Ss,_Is,Us,Us):-
	!.
unfold_literals((L,Ls),Ss,Is,Acc,Bind):-
	unfold_literals(L,Ss,Is,Acc,Acc1)
	,unfold_literals(Ls,Ss,Is,Acc1,Bind).
unfold_literals(L,Ss,Is,Acc,Bind):-
	L \= (_,_)
	,member(C,Is)
	,head_body(C,L,Ls)
	,unfold_literals(Ls,Ss,Is,Acc,Bind).
unfold_literals(L,Ss,Is,Acc,Bind):-
% At this point, the clause of which L is a body literal may be any
% resolvent of the original clause. We want to keep only clauses that
% entail any positive examples, so we call L. The head of the parent
% clause of L has been instantiated to a positive example, so L, too, is
% instantiated accordingly, therefore call(L) will only succeed if L is
% a literal in the refutation sequence of a positive example.
	L \= (_,_)
	,\+ clause_of(L,Ss)
	,call(L)
	,unfold_literals(true,Ss,Is,(L,Acc),Bind).


%!	head_body(+Clause,+Literal,-Body) is det.
%
%	Auxiliary to unfold_literals/5.
%
%	Bind the head of a Clause to a Literal and return its Body.
%
%	The purpose of this is similar to the call to clause/2 in a
%	classic Prolog meta-interpreter: we want to recursively evaluate
%	the body literals of a clause, so for every body literal we find
%	its parent clause and replace the literal with the body literals
%	of the parent.
%
head_body(C,L,B):-
% Avoid binding variables in the clause to terms in the literal.
	copy_term(C,L:-B)
	,!.
head_body(A,L,true):-
	copy_term(A,L).


%!	encapsulated_signature(+Encapsulated,-Signature) is det.
%
%	Collect constants in an Encapsulated program.
%
%	Encapsulated is a list of encapsulated clauses of a program
%	learned with dynamic learning.
%
%	Signature is the list of constants of in all clauses in
%	Encapsulated.
%
%	Note well: since Encapsulated is, well, encapsulated, its
%	constants are actually constants _and_ predicate symbols. This
%	is as it should be because we want to collect both of those for
%	use in lifting an unfolded program, where we want to preserve
%	both symbols and constants in a theory.
%
%	Use this predicate to collect symbols and constants in a program
%	that must be excluded from variabilisation when unfolded clauses
%	are variabilised at the end of unfold_invented/3.
%
encapsulated_signature(Ps,Ss):-
% Below, we collect each result in each findall/3 loop with a subsequent
% call to member/2 to avoid flattening the final list of constants after
% the outer loop exits. Flattening the list of all constants also
% flattens lists that are actually constants - we don't want that. Lists
% found as terms in a theory are constants of the theory, they should be
% kept intact.
%
% Note that the work below would look a lot better as a	recursive
% predicate but this is much easier to write and we'd have to call at
% least one member/2 anyway. So it looks awful. So sue me.
%
% Actually, it looks like a stairway to heaven. A bit.
%
% Outer loop: find all constants in all clauses.
	findall(Cn
	       ,(member(C,Ps)
		,clause_literals(C,Ls)
		% Median loop: find all constants in all literals
		,findall(Cn_
			,(member(L,Ls)
			 ,L =.. [_F|As]
			 % Inner loop: find all constants in one literal
			 ,findall(A
				 ,(member(A,As)
				  ,nonvar(A)
				  )
				 ,Cs)
			 ,member(Cn_,Cs)
			 )
			,CS)
		,member(Cn,CS)
		)
	       ,Ss_)
	,sort(Ss_,Ss).



%!	pseudo_unfold_invented(+Program,+Targets,-Unfolded) is det.
%
%	As unfold_invented/3 but ignores positive examples.
%
%	This predicate generates all resolvents of the clauses of target
%	predicates in Program with the clauses of invented predicates
%	and returns the full set of such resolvents regardless of
%	whether such a clause entails a positive example or not.
%
%	unfold_invented/3 cannot be used as a stand-alone predicate,
%	without the elements of a MIL problem in the dynamic database.
%	This predicate addresses this limitation and can be useful for
%	testing, especially when trying to understand the behaviour of
%	unfold_invented/3, e.g. by observing the full set of resolvents
%	of clauses of target predicates and clauses of invented
%	predicates that can be generated regardless of whether they
%	entail any positive examples.
%
pseudo_unfold_invented(Ps,Ts,Us):-
	program_invented(Ps,Ts,Cs,Is)
	,invented_symbols_(Is,Ss)
	,!
	,pseudo_unfold_clauses(Cs,Ss,Is,[],Us_)
	,flatten(Us_,Us).
pseudo_unfold_invented(Ps,Ts,Ps):-
	program_invented(Ps,Ts,_Cs,[]).


%!	pseudo_unfold_clauses(+Clauses,+Symbols,+Invented,+Acc,-Unfolded) is
%!	det.
%
%	Unfold a set of Clauses with a set of Invented definitions.
%
%	Counterpart to unfold_clauses/5, but ignoring positive examples.
%
pseudo_unfold_clauses([],_Ss,_Is,Us,Us):-
	!.
pseudo_unfold_clauses([C|Cs],Ss,Is,Acc,Bind):-
	findall(C_
	       ,pseudo_unfold_clause(C,Ss,Is,C_)
	       ,Us)
	,pseudo_unfold_clauses(Cs,Ss,Is,[Us|Acc],Bind).


%!	pseudo_unfold_clause(+Clause,+Symbols,+Invented,-Unfolded) is nondet.
%
%	Auxiliary to pseudo_unfold_clauses/5, operating on a single Clause.
%
%	Counterpart to unfold_clause/4, but ignoring positive examples.
%
%	@tbd The only difference of this predicate and unfold_clause/4
%	is a missing call to clause/2 to instantiate the head of a
%	clause to a positive example. The alternatives are all fiddly
%	and would make the code harder to read so this is it.
%
pseudo_unfold_clause(H:-B,Ss,Is,H:-B_):-
	must_be(nonvar,H)
	,must_be(nonvar,B)
	,pseudo_unfold_literals(B,Ss,Is,(H),U_)
	,treeverse(U_,(H,B_)).


%!	pseudo_unfold_literals(+Literals,+Symbols,+Invented,+Acc,-Unfolded) is
%!	nondet.
%
%	Auxiliary to pseudo_unfold_clause/4, operating on each of its Literals.
%
%	Counterpart to unfold_literals/5, ignoring positive examples.
%
%	@tbd the only difference between this and unfold_literals/5 is a
%	single missing goal in their last clause. That's the goal that
%	calls a body literal before adding it to the set of body
%	literals in a clause. So this is pretty much copy/pasta but the
%	alternatives include all sorts of silly fiddly things to merge
%	the two programs that are only going to make the code harder to
%	read and understand. Let it be.
%
pseudo_unfold_literals(true,_Ss,_Is,Us,Us):-
	!.
pseudo_unfold_literals((L,Ls),Ss,Is,Acc,Bind):-
	pseudo_unfold_literals(L,Ss,Is,Acc,Acc1)
	,pseudo_unfold_literals(Ls,Ss,Is,Acc1,Bind).
pseudo_unfold_literals(L,Ss,Is,Acc,Bind):-
	L \= (_,_)
	,member(C,Is)
	,head_body(C,L,Ls)
	,pseudo_unfold_literals(Ls,Ss,Is,Acc,Bind).
pseudo_unfold_literals(L,Ss,Is,Acc,Bind):-
	L \= (_,_)
	,\+ clause_of(L,Ss)
	,pseudo_unfold_literals(true,Ss,Is,(L,Acc),Bind).



%!	reduce_unfolded(+Program,-Reduced) is det.
%
%	Reduce an unfolded Program.
%
%	Program is a set of clauses, currently as returned by
%	unfold_invented/3.
%
%	Reduced is the set of unique clauses in Program. Clauses are
%	considered identical if they are equal up to a) renaming of
%	variables and b) ordering of literals.
%
%	This will need some explanation and it'd be nice to have a name
%	that makes sense. It's "reduced_unfolded" for now because it's
%	only meant to be used with unfolded programs. I think with a bit
%	of work to understand and make sure it works correctly it could
%	be used in the rest of the project, alongside Plotkin's
%	reduction. In fact, I wonder if we're not essentially testing
%	subsumption between clauses to some extent, with this.
%
reduce_unfolded(Ps,Us):-
	maplist(clause_literals,Ps,Ls)
	,maplist(sort,Ls,Ls_)
	,findall(I-C
		,nth1(I,Ls_,C)
		,Ls_I)
	,indexed_mergesort(Ls_I,Is)
	,findall(Ci
		,(member(I-_C,Is)
		 ,nth1(I,Ps,Ci)
		 ,\+ tautology(Ci)
		 )
		,Us).


%!	indexed_mergesort(+Indexed,-Sorted) is det.
%
%	Mergesort implementation for key-value pairs.
%
%	Indexed is a list of key-value-pairs, K-V, where K an integer
%	key and V a value. Since this is only called from
%	reduce_unfolded, it is expected that each V is a sorted list of
%	literals of a clause in the initial program. We want to sort
%	Indexed to remove elements with duplicate values. To do this we
%	sort by comparing only the values, while ignoring the keys.
%
%	This is similar to what can be achieved by keysort/2, however we
%	want to compare literals ignoring their variables, i.e. we
%	test whether two literals are equal up to renaming of their
%	variables. This is handled by merge_compare/3.
%
%	Mergesort adapted from:
%
%	http://kti.ms.mff.cuni.cz/~bartak/prolog/sorting.html
%
indexed_mergesort([],[]):-
	!.
indexed_mergesort([I-Xs],[I-Xs]):-
	!.
indexed_mergesort(Ls,Ss):-
	Ls=[_,_|_]
	,split(Ls,Ls_R,Ls_L)
	,indexed_mergesort(Ls_R,Ss_1)
	,indexed_mergesort(Ls_L,Ss_2)
	,indexed_merge(Ss_1,Ss_2,Ss).


%!	split(+List,-Right,-Left) is det.
%
%	Split a list in two halves.
%
%	Divide step of indexed_mergesort/2. List should be a list of
%	key-value pairs, K-V, where K is an integer kay and V a list of
%	literals. Right and Left are the two halves of List.
%
split(Ls,Ls_R,Ls_L):-
	in_half(Ls,Ls,Ls_R,Ls_L).

%!	split(+List,+Buff,-Right,-Left) is det.
%
%	Business end of split/3.
%
in_half([],Rs,[],Rs):-
% for lists of even length
	!.
in_half([_],Rs,[],Rs):-
% for lists of odd length
	!.
in_half([_,_|T],[X|Ls],[X|Ls_R],Rs):-
	in_half(T,Ls,Ls_R,Rs).


%!	indexed_merge(+List1,+List2,-Merged) is det.
%
%	Merge two lists of key-value pairs.
%
%	Merge step of indexed_mergesort/2. List1 and List2 should be
%	lists of key-value pairs, K-V, where K is an integer key and V a
%	list of literals.
%
indexed_merge([],Ls,Ls):-
	!.
indexed_merge(Ls,[],Ls):-
	!.
indexed_merge([I-X|T1],[_K-Y|T2],[I-X|T]):-
% We could avoid comparison by comparing indices...
	merge_compare(X,Y,=)
	,!
	,indexed_merge(T1,T2,T).
indexed_merge([I-X|T1],[K-Y|T2],[I-X|T]):-
	merge_compare(X,Y,=<)
	,!
	,indexed_merge(T1,[K-Y|T2],T).
indexed_merge([I-X|T1],[K-Y|T2],[K-Y|T]):-
	merge_compare(X,Y,>)
	,indexed_merge([I-X|T1],T2,T).


%!	merge_compare(+Term1,+Term2,-Delta) is det.
%
%	Compare two terms up to renaming of variables.
%
merge_compare(A,B,D):-
	(   unifiable(A,B,_)
	->  D = (=)
	;   A @< B
	->  D = (=<)
	;   A @> B
	->  D = >
	).
