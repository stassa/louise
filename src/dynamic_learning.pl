:-module(dynamic_learning, [learn_dynamic/1
			   ,learn_dynamic/2
			   ,learn_dynamic/5
			   ,top_program_dynamic/4
			   ]).

:-use_module(src(mil_problem)).
:-use_module(src(auxiliaries)).
:-use_module(src(louise)).
:-use_module(src(subhypothesis_selection)).

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
	,!
	,write_program(Pos,BK,Refs)
	,top_program_dynamic(C,Pos,Neg,MS)
	,erase_program_clauses(Refs)
	,collect_clauses(Ss,MS,Ts).
top_program_dynamic(_C,_Ss,_Pos,_Neg,_BK,_MS,_Ts):-
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
	,user:call(Ls).
metasubstitution(C,E,M,MS,Sub):-
	bind_head_literal(E,M,(Sub:-(E,Ls)))
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
