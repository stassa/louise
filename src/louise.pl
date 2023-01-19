:-module(louise, [learn/1
		 ,learn/2
		 ,learn/5
		 ,top_program/5
		 ,generalise/3
		 ,specialise/3
		 ,prove/6
		 ,constraints/1
		 ,reduced_top_program/5
		 ]).

:-use_module(project_root(configuration)).
:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).
:-use_module(lib(tp/tp)).
:-use_module(src(subhypothesis_selection)).

/** <module> Meta-Interpretive Learning by Top program construction and reduction.

*/

%!	learn(+Targets) is det.
%
%	Learn a deafinition of one or more learning Targets.
%
learn(Ts):-
	learn(Ts,Ps)
	,print_clauses(Ps).



%!	learn(+Targets,-Definition) is det.
%
%	Learn a definition of one or more learning Targets.
%
learn(Ts,_Ps):-
	(   \+ ground(Ts)
	->  throw('learn/2: non-ground target symbol!')
	;   fail
	).
learn(Ts,Ps):-
	tp_safe_experiment_data(Ts,Pos,Neg,BK,MS)
	,learn(Pos,Neg,BK,MS,Ps).



%!	learn(+Pos,+Neg,+BK,+Metarules,-Progam) is det.
%
%	Learn a Progam from a MIL problem.
%
learn([],_Neg,_BK,_MS,_Ts):-
	throw('learn/5: No positive examples found. Cannot train.').
learn(Pos,Neg,BK,MS,_Ts):-
	(   var(Pos)
	->  throw('learn/5: unbound positive examples list!')
	;   var(Neg)
	->  throw('learn/5: unbound negative examples list!')
	;   var(BK)
	->  throw('learn/5: unbound background symbols list!')
	;   var(MS)
	->  throw('learn/5: unbound metarule IDs list!')
	;   fail
	).
learn(Pos,Neg,BK,MS,Ps):-
	configuration:unfold_invented(U)
	,configuration:fold_recursive(F)
	,debug(learn,'Encapsulating problem...',[])
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,debug(learn,'Constructing Top program...',[])
	,top_program(Pos_,Neg_,BK_,MS_,Ms)
	,debug(learn,'Reducing Top program...',[])
	,reduced_top_program(Pos_,BK_,MS_,Ms,Rs)
	,examples_targets(Pos,Ss)
	,debug(learn,'Excapsulating hypothesis...',[])
	,excapsulated_clauses(Ss,Rs,Ps_1)
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



%!	top_program(+Pos,+Neg,+BK,+Metarules,-Top) is det.
%
%	Construct the Top program for a MIL problem.
%
%	Clauses are selected according to the configuration setting
%	theorem_prover/1.
%
%	If theorem_prover/1 is set to "resolution", Top program
%	construction is performed in a top-down manner using SLD
%	resolution to decide entailment, which is faster (because it
%	hands off to the Prolog interpreter), but not guaranteed to
%	terminate (for example, it may go infinite given the
%	left-recursive nature of encapsulated metarules).
%
%	If the value of theorem_prover/1 is "tp", Top program
%	construction is performed in a bottom-up manner, using a TP
%	operator. This is slower (because it's implemented in Prolog)
%	but it's guaranteed to terminate. Note also that the TP operator
%	only works for datalog definite programs.
%
%	@bug Top program specialisation using the TP operator is still a
%	work in progress and may not fully eliminate too-general
%	metasubstitutions.
%
top_program(Pos,Neg,BK,MS,_Ts):-
	(   var(Pos)
	->  throw('top_program/5: unbound positive examples list!')
	;   var(Neg)
	->  throw('top_program/5: unbound negative examples list!')
	;   var(BK)
	->  throw('top_program/5: unbound background symbols list!')
	;   var(MS)
	->  throw('top_program/5: unbound metarule IDs list!')
	;   fail
	).
top_program(Pos,Neg,BK,MS,Ts):-
% Uses the Prolog engine and avoids using the dynamic db too much.
	configuration:theorem_prover(resolution)
	% Adds positive examples to BK to learn limited recursion.
	,configuration:clause_limit(0)
	,S = write_problem(user,[Pos,BK],Refs)
	,G = (debug(top_program,'Constructing Top program...',[])
	     ,generalise(Pos,MS,Ss_Gen)
	     ,debug_clauses(top_program,'Generalised Top program',Ss_Gen)
	     ,specialise(Ss_Gen,Neg,Ss_Spec)
	     ,debug_clauses(top_program,'Specialised Top program',Ss_Spec)
	     ,applied_metarules(Ss_Spec,MS,Ts)
	     ,debug_clauses(top_program,'Applied metarules',Ts)
	     )
	,C = erase_program_clauses(Refs)
	,setup_call_cleanup(S,G,C)
	% Fail if Top Program is empty.
	,Ts \= []
	,!.
top_program(Pos,Neg,BK,MS,Ts):-
% Uses the Prolog engine and avoids using the dynamic db too much.
	configuration:theorem_prover(resolution)
	,configuration:clause_limit(K)
	,K > 0
	,S = (write_problem(user,[BK],Refs)
	     ,table(prove/6)
	     )
	,G = (debug(top_program,'Constructing Top program...',[])
	     ,generalise(Pos,MS,Ss_Gen)
	     ,debug_clauses(top_program,'Generalised Top program',Ss_Gen)
	     ,specialise(Ss_Gen,MS,Neg,Ss_Spec)
	     ,debug_clauses(top_program,'Specialised Top program',Ss_Spec)
	     ,flatten(Ss_Spec,Ss_Spec_f)
	     ,sort(1,@<,Ss_Spec_f,Ss_Spec_s)
	     ,applied_metarules(Ss_Spec_s,MS,Ts)
	     ,debug_clauses(top_program,'Applied metarules',Ts)
	     )
	,C = (erase_program_clauses(Refs)
	     ,untable(prove/6)
	     )
	,setup_call_cleanup(S,G,C)
	% Fail if Top Program is empty.
	,Ts \= []
	,!.
top_program(Pos,Neg,BK,MS,Ts):-
	configuration:theorem_prover(tp)
	,examples_targets(Pos,Ss)
	,bind_target(MS,Ss,MS_)
	,flatten([Pos,BK],Ps)
	,debug(top_program,'Constructing Top program with TP operator...',[])
	,generalise_tp(MS_,Ps,Is,Ts_Pos)
	,debug_clauses(top_program,'Derived metasubstitutions:',[Ts_Pos])
	,metasubs_with_metarules(Ts_Pos,MS,Ts_Pos_)
	,debug_clauses(top_program,'Generalised Top program:',[Ts_Pos_])
	,applied_metarules(Ts_Pos_,MS,Ts_Pos_a)
	,specialise_tp(Ts_Pos_a,Is,Neg,Ts)
	,debug_clauses(top_program,'Specialised Top program:',[Ts])
	,!.
top_program(_Pos,_Neg,_BK,_MS,[]):-
% If Top program construction fails return an empty program.
% This is meant to send a clear message that learning failed.
	debug(top_program,'INSUFFICIENT DATA FOR MEANINGFUL ANSWER',[]).



%!	generalise(+Positive,+Metarules,-Generalised) is det.
%
%	Generalisation step of Top program construction.
%
%	Generalises a set of Positive examples by finding each
%	metasubstitution of a metarule that entails a positive example.
%
%	Positive is a set of positive examples of a learning target.
%
%	Metarules is a set of expanded metarules.
%
%	The form of Generalised depends on the configuration option
%	clause_limit/1.
%
%	If clause_limit(0) is set, Generalised is a set of key-value
%	pairs where the keys are ground metasubstitution atoms and the
%	values are a copy, with free variables, of the encapsulated head
%	and body literals of the metarule corresponding to the
%	metasubsitution.
%
%	If clause_limit(K) is set, and K > 0, Generalised is a list _of
%	lists_ of key-value pairs of ground metasubstitution atoms and
%	their corresponding metarules.
%
%	The key-value pairs of metarules returned by generalise/3 can be
%	passed to applied_metarules/3 to apply the metasubstitutions to
%	their metarules to obtain first-order clauses.
%
%	@tbd Give examples of the key-value-pairs of ground
%	metasubstitutions and metarules returned by generalise/3.
%
generalise(Pos,MS,Ss_Pos):-
% Hands proofs to the Prolog engine.
	configuration:clause_limit(0)
	,!
	,findall(Sub-M
	     ,(member(M,MS)
	      ,member(Ep,Pos)
	      ,debug_clauses(examples,'Positive example:',Ep)
	      ,debug_msg_metarules(metasubstitution,'Instantiating metarule:',M)
	      ,metasubstitution(Ep,M,Sub)
	      ,constraints(Sub)
	      )
	     ,Ps)
	,sort(1,@<,Ps,Ss_Pos).
generalise(Pos,MS,Ss_Pos):-
% Hands proofs to Vanilla inductive meta-interpreter.
	configuration:clause_limit(K)
	,findall(Subs
		,(member(Ep,Pos)
		 ,debug_clauses(examples,'Positive example:',Ep)
		 ,metasubstitutions(Ep,K,MS,Subs)
		 ,forall(member(Sub-_M,Subs)
			,constraints(Sub)
			)
		 ,debug_clauses(metasubstitutions,'Passed metasub constraints:',[Subs])
		 )
		,Ss_Pos_)
	,predsort(unifiable_compare,Ss_Pos_,Ss_Pos).



%!	specialise(+Generalised,+Negatives,-Specialised) is det.
%
%	Specialisation step of Top program construction.
%
%	Specialises a set of metasubstitutions generalising the positive
%	examples against the Negative examples by discarding each
%	metasubstitution that entails a negative example.
%
%	Generalised is a set of metasubstitutions that entail all
%	positive examples with respect to BK, in the form returned by
%	generalise/3.
%
%	Negatives is a set of negative examples.
%
%	Specialised is the set of metasubstitutions in Generalised that
%	entail no negative examples, with respect to BK.
%
%	The configuration option max_error/2 can be set to allow
%	Specialised to include clauses that entail one or more negative
%	examples.
%
specialise(Ss_Pos,[],Ss_Pos):-
	!.
specialise(Ss_Pos,Neg,Ss_Neg):-
	configuration:clause_limit(0)
	,configuration:max_error(L_H,L_C)
	,C_H = c(0)
	,C_C = c(0)
	,findall(H-M
		,(member(H-M,Ss_Pos)
		 ,nb_setarg(1,C_C,0)
		 ,debug_clauses(metasubstitution,'Ground metasubstitution atom:',H)
		 ,\+((member(En,Neg)
		     ,debug_clauses(examples,'Negative example:',En)
		     ,metasubstitution(En,M,H)
		     ,(   error_exceeded(L_H,C_H,hypothesis)
		      ;   error_exceeded(L_C,C_C,clause)
		      )
		     ,debug(errors,'Maximum Errors exceeded!',[])
		     )
		    )
		 )
		,Ss_Neg_)
	,sort(Ss_Neg_,Ss_Neg).


%!	error_exceeded(+Limit,+Errors) is det.
%
%	True when the count of Errors exceeds a Limit.
%
%	In the context of this predicate, an "error" is found when a
%	metasubstitution entails a negative example, with respect to BK.
%
%	Limit is one of the two values of max_error/2, either the
%	overall error allowed for the hypothesis as a whole, or the
%	error of each single clause in the hypothesis.
%
%	Errors is a compound c(N) where N is the count of errors so-far
%	for the kind of error associated with Limit (i.e. either errors
%	of the hypothesis, or of one clause). N is destructively updated
%	each time this predicate is reached.
%
error_exceeded(N,C,T):-
	arg(1,C,I)
	,succ(I,Ipp)
	,nb_setarg(1,C,Ipp)
	,debug(errors,'Max ~w errors: ~w Current errors: ~w',[T,N,Ipp])
	,Ipp > N.


%!	specialise(+Generalised,+Metarules,+Negatives,-Specialised) is
%!	det.
%
%	Specialisation step of Top program construction.
%
%	Specialises a set of metasubstitutions generalising the positive
%	examples against the Negative examples by discarding each
%	metasubstitution that entails a negative example.
%
%	Unlike the original, one-clause TPC version this one specialises
%	sub-hypotheses derived by generalise/3.
%
specialise(Ss_Pos,_MS,[],Ss_Pos):-
	!.
specialise(Ss_Pos,MS,Neg,Ss_Neg):-
	configuration:clause_limit(K)
	,K > 0
	,findall(Subs
	       ,(member(Subs,Ss_Pos)
		,findall(Sub
			,member(Sub-_M,Subs)
			,Subs_)
		,debug_clauses(metasubstitutions,'Ground metasubstitutions:',[Subs_])
		,\+((member(En,Neg)
		    ,debug_clauses(examples,'Negative example:',En)
		    ,once(metasubstitutions(En,K,MS,Subs_))
		    ,debug_clauses(examples,'Proved negative example:',En)
		    )
		   )
		)
	       ,Ss_Neg).



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
%	@tbd This version of metasubstitution/n hands proofs of body
%	literals of a metarule to the Prolog engine and only resolves
%	metarules with examples and BK predicates. That means that this
%	version is much faster than metasubstitution/4 but can only
%	derive one kind of recursive clause, the kind that can resolve
%	with positive examples (and BK predicates).
%
metasubstitution(E,M,Sub):-
	copy_term(M,M_)
	,bind_head_literal(E,M_,(Sub:-(H,Ls)))
	,debug_clauses(metasubstitution,'Trying metasubstitution:',H:-Ls)
	,user:call(Ls)
	,debug_clauses(metasubstitution,'Succeeded:',Ls).



%!	bind_head_literal(+Example,+Metarule,-Head) is det.
%
%	Bind an Example to the encapsulated Head literal of a Metarule.
%
%	Abstracts the complex patterns of binding examples to the heads
%	of metarules with and without body literals.
%
bind_head_literal(H:-B,(Sub:-(H,B)),(Sub:-(H,B))):-
% Positive or negative example given as a definite clause
% with one or more body literals.
	configuration:example_clauses(bind)
	,!.
bind_head_literal(H:-B,(Sub:-(H,Ls)),(Sub:-(H,Ls))):-
	configuration:example_clauses(call)
	,user:call(B)
	,!.
bind_head_literal(E,M,(H:-(E,Ls))):-
% Positive example given as a unit clause.
	M = (H:-(E,Ls))
	,!.
bind_head_literal(:-E,M,(H:-(E,Ls))):-
% Negative example given as a unit clause
	M = (H:-(E,Ls))
	,!.
bind_head_literal(E,M,(H:-(E,true))):-
% Positive example given as a unit clause.
% M is the Abduce metarule, i.e. body-less clause.
	M = (H:-E)
	,!.
bind_head_literal(:-E,M,(H:-(E,true))):-
% Negative example given as a unit clause.
% M is the Adbuce metarule, i.e. body-less clause.
	M = (H:-E)
	,!.
bind_head_literal(:-(L,Ls),M,(S:-(H,L,Ls))):-
% Negative example given as a Horn goal with no head literal.
% In this case, metasubstitution/3 must fail if the head of the
% metarule is entailed by its body literals.
% Note that binding the example to the body literals of the metarule
% will also bind the shared variables in the head of the metarule.
	M = (S:-(H,L,Ls))
	,!.


%!	metasubstitutions(+Example,+Limit,+Metarules,-Metasubstitutions)
%!	is nondet.
%
%	Derive all possible Metasubstitutions entailing an Example.
%
%	Limit is the clause limit specified in the configuration option
%	clause_limit/1.
%
metasubstitutions(:-En,K,MS,Subs):-
	!
        ,prove(En,K,MS,[],Subs,Subs)
	,debug(metasubstitutions,'Proved Example: ~w',[:-En])
	,debug_clauses(metasubstitutions,'With Metasubs:',[Subs]).
metasubstitutions(Ep,K,MS,Subs):-
	signature(Ep,Ss)
	,debug(signature,'Signature: ~w',[Ss])
        ,prove(Ep,K,MS,Ss,[],Subs_)
	,debug(metasubstitutions,'Proved Example: ~w',[Ep])
	,Subs_ \= []
	,sort(Subs_,Subs_s)
	,debug_clauses(metasubstitutions,'Proved Metasubs:',[Subs_s])
	,findall(Sub-M
		,(member(Sub,Subs_s)
		 ,metasub_metarule(Sub,MS,M)
		 )
		,Subs).



%!	signature(+Example,-Signature) is det.
%
%	Return the predicate Signature for one example's predicate.
%
signature(L,[T|Ss]):-
        configuration:max_invented(N)
        ,findall(S
                ,invented_symbol(N,S)
                ,Ss)
        ,L =.. [m,T|_].


%!	prove(?Literals,+Limit,+Metarules,+Acc,-Metasubs) is nondet.
%
%	A vanilla MIL meta-interpreter for Top Program Construction.
%
%	Literal is a set of encapsulated literals (as a "tree" of Prolog
%	terms, in parentheses) currently being refuted.
%
%	Limit is the clause limit defined in the configuration option
%	clause_limit/1. Limit restricts the number of distinct clauses
%	in a refutation sequence. Notably this is _not_ the number of
%	clauses _in a learned hypothesis_.
%
%	Metarules is a list of expanded metarules to be used in the
%	proof by refutation of each Literals.
%
%	Acc is the accumulator of metasubstitutions.
%
%	Metasubs is a list of metasubstitution atoms derived during the
%	proof. Metasubstitution atoms are of the form:
%
%	m(Id,Tgt,P1,...,Pn)
%
%	Where Id is the atomic identifier of a metarule in Metarules;
%	Tgt is the predicate symbol (but not arity) of one target
%	predicate, and each Pi is the symbol of a background predicate
%	(which can also be the target predicate, or an invented
%	predicate, as well as a predicate actually  defined in the BK).
%
%	When this predicate is first called, Literals should be a single
%	encapsulated literal, one example of one target predicate. As
%	the proof-by-refutation proceeds, that original goal of the
%	proof is replaced by goals derived by resolution.
%
%	When the proof completes, if it completes successfully,
%	Metasubs should be a list of metasubstitution atoms, each of
%	which can be applied to its corresponding metarule (found via
%	the metasubstitution atom's first, Id, argument) to produce a
%	first-order clause. In that sense, the clauses of Metasubs are
%	the set of clauses in one refutation sequence of the original
%	example Literal.
%
%	The number of metasubstitution atoms in Metasubs can be at most
%	equal to Limit.
%
%	Since Metasubstitutions is a list of atoms in a refutation
%	sequence, this meta-interpreter is capable of inducing, for each
%	single example, an entire proof-branch refuting that example. In
%	other words, this is is a multi-clause learning inductive
%	meta-interpreter, capable of learning entire programs from one
%	example.
%
%	What is the motivation for multi-clause learning? Traditional
%	ILP systems like Aleph or Progol learn a single "rule" from each
%	positive example (and then remove examples "covered" by the
%	rule). That works fine until an example is found that cannot be
%	covered by a single rule. This is often the case with recursive
%	programs and certainly the case with predicate invention. The
%	ability to learn multiple clauses that resolve with each other
%	to refute a goal-example allows Louise (and MIL systems in
%	general) to learn recursive programs and to perform predicate
%	invention without the restrictions of earlier systems.
%
prove(true,_K,_MS,_Ss,Subs,Subs):-
	!
	,debug(prove,'Metasubs so-far: ~w',[Subs]).
prove((L,Ls),K,MS,Ss,Subs,Acc):-
	prove(L,K,MS,Ss,Subs,Subs_)
        ,prove(Ls,K,MS,Ss,Subs_,Acc).
prove((L),K,MS,Ss,Subs,Acc):-
        L \= (_,_)
	,L \= true
        ,clause(L,K,MS,Ss,Subs,Subs_,Ls)
        ,prove(Ls,K,MS,Ss,Subs_,Acc).
/* % Uncomment for richer debugging and logging.
prove(L,_MS,_Ss,Subs,_Acc):-
	L \= true
        ,debug(prove,'Failed to prove literals: ~w',[L])
	,debug(prove,'Metasubs so-far: ~w',[Subs])
	,fail.
*/


%!	clause(?Literal,+K,+MS,+Sig,+Subs,-Subs_New,-Body) is nondet.
%
%	MIL-specific clause/2 variant.
%
%	This predicate is similar to clause/2 except that if the body of
%	a clause with the given Literal as head can't be found in the
%	program database, the metasubstitution store Subs is searched
%	for a known metasubstitution whose encapsulated head literal
%	unifies with Literal. If that fails, a new metasubstitution is
%	contsructed and added to the store.
%
%	Literal is a partially or fully instantiated literal to be
%	proved.
%
%	K is an integer, the clause limit defined in the configuration
%	option clause_limit/1. This limits the number of new
%	metasubstitutions added to the metasubstitution store.
%
%	MS is the set of metarules for the current MIL Problem.
%
%	Sigs is the predicate signature, a list of _atoms_ (not yet
%	predicate identifiers).
%
%	Subs is a list of encapsulated metasubstitution atoms.
%
%	Subs_New is the list Subs with any new metasubstitution
%	constructed.
%
%	Body is the body literals of Literal found in the database, or a
%	metasubstitution already in Subs, or a new one constructed by
%	new_metasub/6.
%
clause(_L,_K,_MS,_Ss,Subs,_Acc,_Ls):-
	\+ check_constraints(Subs)
	,!
	,fail.
clause(L,_K,_MS,_Ss,Subs,Subs,true):-
	(   predicate_property(L,foreign)
	;   built_in_or_library_predicate(L)
	)
	,debug(prove_,'Proving built-in literal: ~w', [L])
        ,call(L)
	,debug(prove_,'Proved built-in clause: ~w', [L:-true]).
clause(L,_K,_MS,_Ss,Subs,Subs,Ls):-
	\+ predicate_property(L,foreign)
	,\+ built_in_or_library_predicate(L)
	,debug(prove_,'Proving literal with BK: ~w', [L])
        ,clause(L,Ls)
	,debug(prove_,'Trying BK clause: ~w', [L:-Ls]).
clause(L,_K,MS,_Ss,Subs,Subs,Ls):-
        debug(prove_,'Proving literal with known metasubs: ~w',[L])
        ,known_metasub(L,MS,Subs,Ls).
clause(L,K,MS,Ss,Subs,Subs_,Ls):-
	length(Subs,N)
	,N < K
        ,debug(prove_,'Proving literal with new metasub: ~w',[L])
        ,new_metasub(L,MS,Ss,Subs,Subs_,Ls).


%!	check_constraints(+Metasubs) is det.
%
%	True if all ground metasubstitutions obey constraints.
%
%	Metasubs is the list of metasubstitutions derived so-far. For
%	each _ground_ metasubstitution in Metasubs, this predicate
%	checks that it does not violate any constraints declared in a
%	metarule_constraints/2 clause.
%
check_constraints(Subs):-
	forall(member(Sub,Subs)
	      ,(   ground(Sub)
	       ->  constraints(Sub)
	       ;   \+ ground(Sub)
	       )
	      ).


%!	known_metasub(?Literal,+Subs,-Body) is nondet.
%
%	Selects a known metasubstition whose head unifies with Literal.
%
known_metasub(L,MS,Subs,Ls):-
	member(Sub,Subs)
        ,applied_metasub(MS,Sub,L,Ls)
	,debug(prove,'Trying known metasub: ~w',[Sub]).


%!	applied_metasub(+Metarules,?Metasubstitution,?Head,-Body)
%!	is nondet.
%
%	Get the encapsulated body literals of a Metasubstitution.
%
applied_metasub(MS, Sub, H, B):-
        free_member(Sub:-(H,B),MS)
	,!.
applied_metasub(MS, Sub, L, true):-
	free_member(Sub:-(L),MS).


%!	free_member(?Element,?List) is nondet.
%
%	member/2 variant that copies elements without unifying them.
%
%	Used by applied_metasub/4 to avoid binding all instances of a
%	metarule throughout a proof branch.
%
free_member(Z,Xs):-
	free_member(_X,Xs,Z).

%!	free_member(?Element,?List,?Copy) is nondet.
%
%	Business end of free_member/2.
%
free_member(X,[Y|_],Z):-
	unifiable(X,Y,_)
	,copy_term(Y,Y_)
	% Unifying in copy_term/2 may fail.
	,Y_ = Z.
free_member(X,[_|Ys],Z):-
	free_member(X,Ys,Z).



%!	new_metasub(?Literal,+MS,+Sig,+Subs,-New_Subs,-Body) is nondet.
%
%	Constructs new metasubstitutions whose heads unify with Literal.
%
new_metasub(L,MS,Ss,Subs,[Sub|Subs],Ls):-
        member(M,MS)
        ,applied_metasub(Sub,M,Ss,L,Ls)
	,debug(prove,'Added new metasub: ~w',[Sub]).


%!	applied(?Metasubstitution,+Metarule,+Sig,?Head,-Body) is
%!	nondet.
%
%	Construct a new Metasubstitution whose head unifies with Head.
%
applied_metasub(Sub, M, Ss, H, Ls):-
	copy_term(M,M_)
	,M_ = (Sub:-(H,Ls))
	,louise:bind_head_literal(H,M_,(Sub:-(H,Ls)))
	,member(S,Ss)
        ,symbol(H,S).
applied_metasub(Sub, M, Ss, H, H):-
	copy_term(M,M_)
	,M_ = (Sub:-(H))
	,louise:bind_head_literal(H,M_,(Sub:-(H)))
	,member(S,Ss)
        ,symbol(H,S).


%!	symbol(?Literal,+Symbol) is det.
%
%	Instantiate a literal's predicate symbol to the given Symbol.
%
symbol(L,S):-
        L =.. [m,S|_As].



%!	metasub_metarule(+Sub,+Metarules,-Metarule) is det.
%
%	Retrieve an expanded metarule matching a metasub atom.
%
%	Sub is an encapsulated metasubtitution atom.
%
%	Metarules is the list of expanded metarules.
%
%	Metarules is an expanded metarule in Metarules with a metarule
%	Id matching the metarule Id of Sub.
%
%	Used by metasubstitutions/3 to retrieve the encapsulated
%	metarule matching the metarule id of a ground metasubstitution
%	atom without binding variables in the encapsulated metarule.
%
metasub_metarule(Sub,MS,Sub_:-M):-
	Sub =.. [m,Id|As]
	,length(As,N)
	,length(As_,N)
	,Sub_ =.. [m,Id|As_]
	,free_member(Sub_:-M,MS).



%!	metasubs_with_metarules(+Metasubs,+Metarules,-Zipped) is det.
%
%	Associate metasubstitution atoms to corresponding metarules.
%
%	Metasubs is a list of (ground) metasubstitution atoms.
%
%	Metarules is a list of expanded metarules with metasubstitution
%	atoms in their heads.
%
%	Zipped is a list of key-value-pairs Sub-M where each Sub is a
%	metasubstitution in Metasubs and each M is an expanded metarule
%	in Metarules, such that the metasubstitution atom at the head of
%	M matches Sub.
%
%	In plain English what this predicate does is that it finds, for
%	each metasubstitution in Metasub, its corresponding metarule, so
%	that the metasubstitution can later be applied to the metarule
%	to form a first-order clause.
%
metasubs_with_metarules(Subs,MS,Subs_):-
	findall(Sub-M
		,(member(Sub,Subs)
		 ,metasub_metarule(Sub,MS,M)
		 )
		,Subs_).



%!	bind_target(+Metarules,+Target,-Bound) is det.
%
%	Bind the Target\'s symbol to the heads of Metarules.
%
%	Small optimisation to ensure that lfp/2 only considers
%	metasubstitutions where the target predicate is the first
%	predicate symbol, rather than all possible metasubstitutions.
%
bind_target(MS,Ts,MS_):-
	is_list(Ts)
	,!
	,findall(H:-B
	       ,(member(H:-B, MS)
		,member(T/_,Ts)
		,H =.. [m,_Id,T|_Ps]
		)
	       ,MS_).


%!	generalise_tp(+Metarules,+Program,-Model,-Generalised) is det.
%
%	Top program generalisation step with TP operator.
%
%	Metarules is the set of metarules in the problem, expanded and
%	with the first predicate symbol in their head bound to the
%	symbol of the target predicate, as returned by bind_target/3.
%
%	Program is a flat list of the encapsulation of the positive
%	examples and background knowledge.
%
%	Model is the least Herbrand model of Program. This is passed to
%	specialise/4 to avoid duplicating work (specifically, the work
%	of building this one up again).
%
%	@tbd This is just a thin wrapper around lfp_query/4 now. Still,
%	keep it around - makes it clear this is a distinct step in the
%	construction of the Top program.
%
generalise_tp(MS,Ps,Is,Ts_Pos):-
	lfp_query(MS,Ps,Is,Ts_Pos).


%!	specialise_tp(+Generalised,+Model,+Negatives,-Specialised) is
%!	det.
%
%	Top program specialisation step with TP operator.
%
%	Generalised is the result of generalise/4. Model is the least
%	Herbrand model of the positive examples and background
%	knowledge, calculated during execution of generalise/4.
%
specialise_tp(Ts_Pos,Ps,Neg,Ts_Neg):-
	findall(H:-B
		,(member(H:-B,Ts_Pos)
		 ,lfp_query([H:-B],Ps,As)
		 ,ord_intersection(As, Neg, [])
		 )
		,Ts_Neg).


%!	constraints(+Metasubstitution) is det.
%
%	Apply a set of constraints to a generalising Metasubstitution.
%
%	Metasubstitution is a generalising metasubstitution considered
%	for addition to the Top program. A generalising metasubstitution
%	is one found during the generalisation step of Top program
%	construction.
%
%	constraints/1 tests Metasubstitution against a set of
%	user-defined constraints. If each applicable constraint is true,
%	then constraints/1 succeeds and Metasubstitution is included in
%	the Top program. Otherwise constraints/1 fails and
%	Metasubstitution is excluded from the Top program.
%
%	Note that only metasubstitutions found to generalise an example
%	are tested for constraints, i.e. metasubstitutions that can not
%	be proven against the MIL problem will be excluded without
%	constraints being tested.
%
%	User-defined constraints
%	------------------------
%
%	User-defined constraints are declared in experiment files as
%	clauses of configuration:metarule_constraints/2:
%
%	==
%	configuration:metarule_constraints(?Metasub,+Goal) is semidet.
%	==
%
%	A metarule_constraints/2 clause can be any Prolog clause. To
%	clarify, it can be a unit clause (a "fact"), or a non-unit
%	clause (a "rule").
%
%	The first argument of metarule_constraints/2 should match the
%	metasubstitution atom of an encapsulated metarule (the functor
%	must be "m" not "metarule"). If a generalising metasubstitution
%	matches this first argument, the matching metarule_constraint/2
%	clause is called. If this first call succeeds, the Prolog goal
%	in the second argument is called to perform the constraint test.
%
%	The second argument of metarule_constraints/2 is an arbitrary
%	Prolog goal. When the first argument of metarule_constraints/2
%	matches a generalising metasubstitution and the initial call
%	to metarule_constraint/2 succeds, the second argument is passed
%	to call/1. If this second call fails, the constraint test fails
%	and the metasubstitution matching the first argument is removed
%	from the generalised Top program. If this second calls succeeds,
%	the cosntraint test passes and the metasubstitution is added to
%	the generalised Top program.
%
%	Example
%	-------
%
%	The following metarule constraint will match a metasubstitution
%	with any metarule Id and with three existentially quantified
%	variables all ground to the same term. When the match succeeds,
%	and given that the constraint is a unit clause (i.e. always
%	true), the second argument of the constraint, fail/0 will be
%	called causing the constraint to fail and the metasubstitution
%	to be discarded:
%
%	==
%	configuration:metarule_constraints(m(_ID,P,P,P),fail).
%	==
%
%	The metarule constraint listed above can be used to exclude
%	left-recursive clauses from the Top program, but only for
%	metarules with exactly two body literals. A more general
%	constraint that will apply to a metarule with an arbitrary
%	number of body literals is as follows:
%
%	==
%	configuration:metarule_constraints(M,fail):-
%	M =.. [m,_Id,P|Ps]
%	,forall(member(P1,Ps)
%	       ,P1 == P).
%	==
%
%	Alternatively, the symbol of a target predicate can be specified
%	so that only metasubstitutions of that predicate are excluded
%	(if they would result in left-recursive clauses):
%
%	==
%	configuration:metarule_constraints(M,fail):-
%	M =.. [m,_Id,ancestor|Ps]
%	,forall(member(P1,Ps)
%	       ,P1 == ancestor).
%	==
%
%	Or the metarule Id can be ground to test only metasubstitutions
%	of a specific metarule, and so on.
%
%
%	Metarule constraints and predicate invention
%	--------------------------------------------
%
%	In the above examples, note the use of ==/2 instead of =/2 to
%	perform the comparison between existentially quantified
%	variables. This is to allow for variables remaining unbound on
%	generalisation during metarule extension by unfolding in the
%	proces of predicate invention.
%
%	@see data(examples/constraints) for examples of using metarule
%	constraints, in particular for the purpose of excluding
%	metasubstitutions resulting in left-recursive clauses from the
%	Top progam.
%
constraints(_Sub):-
	predicate_property(metarule_constraints(_,_), number_of_clauses(0))
	,!.
constraints(Sub):-
	predicate_property(metarule_constraints(_,_), number_of_clauses(N))
	,N > 0
	,copy_term(Sub,Sub_)
	,forall(configuration:metarule_constraints(Sub_, C)
	       ,user:call(C)
	       ).



%!	reduced_top_program(+Pos,+BK,+Metarules,+Program,-Reduced)
%!	is det.
%
%	Reduce the Top Program.
%
%	Clauses are selected according to the value of the configuration
%	option recursive_reduction/1. If this is set to true, the Top
%	program is reduced recursively, by passing the output of each
%	reduction step to the next, as input. If recursive_reduction/1
%	is set to false a single reduction step is performed.
%
%	Recursive reduction is useful when the Top program is large, or
%	recursive, and a large number of resolution steps are required
%	to reduce it effectively. In such cases, recursive reduction can
%	result in a stronger reduction of the Top program (i.e. result
%	in fewer redundant clauses in the learned hypothesis) in a
%	shorter amount of time, without increasing the number of
%	resolution steps in the program reduction meta-interpreter.
%
reduced_top_program(_Pos,_BK,_MS,Ps,Ps):-
	configuration:reduction(none)
	,debug(reduction,'reduction/1 is "none". The Top program is not reduced.',[])
	,!.
reduced_top_program(Pos,BK,_MS,Ps,Rs):-
	configuration:reduction(subhypothesis)
	,!
	,debug(reduction,'Reducing Top program by subhypothesis selection...',[])
	,subhypothesis(Pos,BK,Ps,Rs)
	,debug_clauses(reduction,'Reduced Top program:',Rs).
reduced_top_program(Pos,BK,MS,Ps,Rs):-
	configuration:recursive_reduction(true)
	,!
	,flatten([Pos,BK,Ps,MS],Fs)
	,list_to_set(Fs, Fs_)
	,debug(reduction,'Reducing Top program recursively...',[])
	,program_reduction(Fs_,Rs_,_)
	,length(Fs_,M)
	,length(Rs_,N)
	,debug(reduction,'Initial reduction: ~w to ~w',[M,N])
	,reduced_top_program_(N,Rs_,BK,MS,Rs)
	,debug_clauses(reduction,'Reduced Top program:',Rs)
	% program_reduction module leaves behind garbage
	% in program module. Why?
	,cleanup_experiment.
reduced_top_program(Pos,BK,MS,Ps,Rs):-
	configuration:recursive_reduction(false)
	,flatten([Pos,BK,Ps,MS],Fs)
	,list_to_set(Fs,Fs_)
	,debug(reduction,'Reducing Top program by Plotkin\'s algorithm...',[])
	,program_reduction(Fs_,Rs,_)
	,debug_clauses(reduction,'Reduced Top program:',Rs)
	,cleanup_experiment.


%!	reduced_top_program_(+N,+Prog,+BK,+Metarules,-Reduced) is
%!	det.
%
%	Business end of reduced_top_program/6
%
%	Recursively reduces the Top Program, by feeding back the result
%	of each call to program_reduction/2 to itself, a process known
%	as "doing feedbacksies".
%
reduced_top_program_(N,Ps,BK,MS,Bind):-
	program_reduction(Ps,Rs,_)
	,length(Rs, M)
	,debug(reduction,'New reduction: ~w to ~w',[N,M])
	,M < N
	,!
	,reduced_top_program_(M,Rs,BK,MS,Bind).
reduced_top_program_(_,Rs,_BK,_MS,Rs):-
	length(Rs, N)
	,debug(reduction,'Final reduction: ~w',[N]).
