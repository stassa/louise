:-module(vanilla_tpc, [learn_v/1
		      ,learn_v/2
		      ,learn_v/5]).

/** <module> Attempt to vanillise the MIL meta-interpreter.

Copied from Louise:
a) Learning predicates learn/[1,2,5].
b) top_program/5.

*/


%!	learn(+Targets) is det.
%
%	Learn a deafinition of one or more learning Targets.
%
learn_v(Ts):-
	learn_v(Ts,Ps)
	,print_clauses(Ps).



%!	learn(+Targets,-Definition) is det.
%
%	Learn a definition of one or more learning Targets.
%
learn_v(Ts,_Ps):-
	(   \+ ground(Ts)
	->  throw('learn/2: non-ground target symbol!')
	;   fail
	).
learn_v(Ts,Ps):-
	tp_safe_experiment_data(Ts,Pos,Neg,BK,MS)
	,learn_v(Pos,Neg,BK,MS,Ps).



%!	learn(+Pos,+Neg,+BK,+Metarules,-Progam) is det.
%
%	Learn a Progam from a MIL problem.
%
learn_v([],_Neg,_BK,_MS,_Ts):-
	throw('learn/5: No positive examples found. Cannot train.').
learn_v(Pos,Neg,BK,MS,_Ts):-
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
learn_v(Pos,Neg,BK,MS,Ps):-
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
	,configuration:prove_recursive(fast)
	% Metarules and examples don't need to be added to the dynamic db.
	,S = (write_problem(user,[BK],Refs)
	     ,table(prove/5)
	     )
	,G = (debug(top_program,'Constructing Top program...',[])
	     ,vanilla_tpc:generalise(Pos,MS,Ss_Gen)
	     ,debug_clauses(top_program,'Generalised Top program',Ss_Gen)
	     ,vanilla_tpc:specialise(Ss_Gen,Neg,Ss_Spec)
	     ,debug_clauses(top_program,'Specialised Top program',Ss_Spec)
	     ,flatten(Ss_Spec,Ss_Spec_f)
	     ,sort(Ss_Spec_f,Ss_Spec_s)
	     ,applied_metarules(Ss_Spec_s,MS,Ts)
	     ,debug_clauses(top_program,'Applied metarules',Ts)
	     )
	,C = (erase_program_clauses(Refs)
	     ,untable(prove/5)
	     )
	,setup_call_cleanup(S,G,C)
	% Fail if Top Program is empty.
	,Ts \= []
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
%	Generalised is a set of key-value pairs where the keys are
%	ground metasubstitution atoms and the values are a copy with
%	free variables of the encapsulated head and body literals of the
%	metarule corresponding to the metasubsitution.
%
generalise(Pos,MS,Ss_Pos):-
% Hands proofs to the Prolog engine.
% Runs faster and avoids manipulating the dynamic db.
	findall(Subs
		,(member(Ep,Pos)
		 ,debug_clauses(examples,'Positive example:',Ep)
		 ,metasubstitutions(Ep,MS,Subs)
		 ,forall(member(Sub-_M,Subs)
			,constraints(Sub)
			)
		 ,debug_clauses(prove,'Passed metasub constraints:',[Subs])
		 )
		,Ss_Pos_)
	,sort(Ss_Pos_,Ss_Pos).


%!	specialise(+Generalised,+Negatives,-Specialised) is det.
%
%	Specialisation step of Top program construction.
%
%	Specialises a set of metasubstitutions generalising the positive
%	examples against the Negative examples by discarding each
%	metasubstitution that entails a negative example.
%
%	TODO: temp version until I sort out meta-interpretation.
%
specialise(Ss_Pos,[],Ss_Pos):-
	!.
specialise(Ss_Pos,Neg,Ss_Neg):-
	setof(H-M
	     ,H^M^Ss_Pos^En^Neg^
	      (member(Subs,Ss_Pos)
              ,member(H-M,Subs)
	      ,\+((member(En,Neg)
		  ,debug_clauses(examples,'Negative example:',En)
		  ,debug_clauses(metasubstitution,'Ground metasubstitution atom:',H)
		  ,louise:metasubstitution(En,M,H)
		  )
		 )
	      )
	     ,Ss_Neg).


%!	metasubstitutions(+Example,+Metarules,-Metasubstitutions) is
%!	nondet.
%
%	Derive all possible Metasubstitutions entailing an Example.
%
metasubstitutions(Ep,MS,Subs):-
        signature(Ep,Ss)
	,debug(signature,'Signature: ~w',[Ss])
        ,prove(Ep,MS,Ss,[],Subs_)
	,debug(prove,'Proved Example: ~w',[Ep])
	,Subs_ \= []
	,sort(Subs_,Subs_s)
	,debug_clauses(prove,'Proved Metasubs:',[Subs_s])
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


%!	prove(?Literal,+Metarules,+Acc,-Metasubs) is nondet.
%
%	A vanilla MIL meta-interpreter for Top Program Construction.
%
prove(true,_MS,_Ss,Subs,Subs):-
	debug(prove,'Metasubs so-far: ~w',[Subs]).
prove((L,Ls),MS,Ss,Subs,Acc):-
	debug(prove,'Proving literal (1): ~w',[L])
        ,prove(L,MS,Ss,Subs,Subs_)
        ,debug(prove,'Proved literal (2): ~w',[L])
        ,debug(prove,'Proving literals (3): ~w',[Ls])
        ,prove(Ls,MS,Ss,Subs_,Acc).
prove((L),MS,Ss,Subs,Acc):-
        L \= (_,_)
	,L \= true
        ,debug(prove,'Proving literal (4): ~w',[L])
        ,clause(L,MS,Ss,Subs,Subs_,Ls)
	,debug(prove,'Proving literals (5): ~w',[Ls])
        ,prove(Ls,MS,Ss,Subs_,Acc).
prove(L,_MS,_Ss,Subs,_Acc):-
	L \= true
        ,debug(prove,'Failed to prove literals: ~w',[L])
	,debug(prove,'Metasubs so-far: ~w',[Subs])
	,fail.



%!	clause(?Literal,+MS,+Sig,+Subs,-Subs_New,-Body) is nondet.
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
clause(L,_MS,_Ss,Subs,Subs,true):-
	(   predicate_property(L,foreign)
	;   built_in_or_library_predicate(L)
	)
	,debug(prove,'Proving built-in literal: ~w', [L])
        ,call(L)
	,debug(prove,'Proved built-in clause: ~w', [L:-true]).
clause(L,_MS,_Ss,Subs,Subs,Ls):-
	\+ predicate_property(L,foreign)
	,\+ built_in_or_library_predicate(L)
	,debug(prove,'Proving literal with BK: ~w', [L])
        ,clause(L,Ls)
	,debug(prove,'Trying BK clause: ~w', [L:-Ls]).
clause(L,MS,_Ss,Subs,Subs,Ls):-
        debug(prove,'Proving literal with known metasubs: ~w',[L])
        ,known_metasub(L,MS,Subs,Ls).
clause(L,MS,Ss,Subs,Subs_,Ls):-
        debug(prove,'Proving literal with new metasub: ~w',[L])
        ,new_metasub(L,MS,Ss,Subs,Subs_,Ls).



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
	,debug(prove,'Adding new metasub: ~w',[Sub])
	%,debug_clauses(prove,'Known metasubs:',[Subs])
	,\+ member(Sub,Subs)
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
