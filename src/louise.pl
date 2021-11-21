:-module(louise, [learn/1
		 ,learn/2
		 ,learn/5
		 ,top_program/5
		 ,generalise/3
		 ,specialise/3
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
	debug(learn,'Encapsulating problem',[])
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,debug(learn,'Constructing Top program...',[])
	,top_program(Pos_,Neg_,BK_,MS_,Ms)
	,debug(learn,'Reducing Top program...',[])
	,reduced_top_program(Pos_,BK_,MS_,Ms,Rs)
	,examples_targets(Pos,Ss)
	,debug(learn,'Excapsulating hypothesis',[])
	,excapsulated_clauses(Ss,Rs,Ps).



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
	,!
	% Metarules and negative examples don't need to be added to the dynamic db.
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
	,setup_call_cleanup(S,G,C).
top_program(Pos,Neg,BK,MS,Ts):-
% Uses the specialised metarule meta-interpreter resolve_metarules/5
% Runs slower and manipulates the dynamic db.
	configuration:theorem_prover(resolution)
	% Negative examples don't need to be added to the dynamic db.
	,examples_targets(Pos,Ss)
	,S = (write_problem(user,[Pos,BK,MS],Refs)
	     ,dynamic_learning:table_encapsulated(Ss)
	     )
	,G = (debug(top_program,'Constructing Top program...',[])
	     ,generalise(Pos,MS,Ss_Gen)
	     ,debug_clauses(top_program,'Generalised Top program',Ss_Gen)
	     ,specialise(Ss_Gen,Neg,Ss_Spec)
	     ,debug_clauses(top_program,'Specialised Top program',Ss_Spec)
	     ,applied_metarules(Ss_Spec,MS,Ts)
	     ,debug_clauses(top_program,'Applied metarules',Ts)
	     )
	,C = (erase_program_clauses(Refs)
	     ,dynamic_learning:untable_encapsulated(Ss)
	     ,cleanup_experiment
	     )
	,setup_call_cleanup(S,G,C)
	,!.
top_program(Pos,Neg,BK,MS,Ts):-
	configuration:theorem_prover(tp)
	,examples_targets(Pos,Ss)
	,bind_target(MS,Ss,MS_)
	,flatten([Pos,BK],Ps)
	,debug(top_program,'Constructing Top program...',[])
	,generalise(MS_,Ps,Is,Ts_Pos)
	,applied_metarules(Ts_Pos,MS,Ts_Pos_)
	,specialise(Ts_Pos_,Is,Neg,Ts)
	,!.
top_program(_Pos,_Neg,_BK,_MS,[]):-
% If Top program construction fails return an empty program.
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
	configuration:prove_recursive(fast)
	,!
	,findall(Sub-M
	     ,(member(M,MS)
	      ,member(Ep,Pos)
	      ,debug_clauses(metasubstitution,'Positive example:',Ep)
	      ,debug_clauses(metasubstitution,'Instantiating metarule:',M)
	      ,metasubstitution(Ep,M,Sub)
	      ,constraints(Sub)
	      )
	     ,Ps)
	,sort(1,@<,Ps,Ss_Pos).
generalise(Pos,MS,Ss_Pos):-
% Hands proofs to the metarule meta-interpreter resolve_metarules/5.
% Runs slower, but is more complete.
%
% Tabling keeps resolve_metarules/5 from going infinite during
% meta-interpretation if possible. Tabled as incremental because
% encapsulated clauses may be added to or removed from the dynamic db
% during meta-interpretation.
        table(resolve_metarules/6 as incremental)
	,findall(Sub-M_-Refs
	     ,(member(M,MS)
	      ,member(Ep,Pos)
	      ,debug_clauses(metasubstitution,'Positive example:',Ep)
	      ,debug_clauses(metasubstitution,'Instantiating metarule:',M)
	      ,metasubstitution(Ep,M,MS,Sub-M_)
	      ,constraints(Sub)
	      ,assert_clause(Sub-M_,Refs)
	      )
	     ,Ps)
	,untable(resolve_metarules/6)
	% Remove clauses added to dynamic db, if any.
	,pairs_keys_values(Ps,Ss_Pos_,Rs)
	,flatten(Rs,Rs_f)
	,erase_program_clauses(Rs_f)
	,sort(1,@<,Ss_Pos_,Ss_Pos).



%!	specialise(+Generalised,+Negatives,-Specialised) is det.
%
%	Specialisation step of Top program construction.
%
%	Specialises a set of metasubstitutions generalising the positive
%	examples against the Negative examples by discarding each
%	metasubstitution that entails a negative example.
%
specialise(Ss_Pos,[],Ss_Pos):-
	!.
specialise(Ss_Pos,Neg,Ss_Neg):-
	setof(H-M
	     ,H^M^Ss_Pos^En^Neg^
	      (member(H-M,Ss_Pos)
	      ,\+((member(En,Neg)
		  ,debug_clauses(metasubstitution,'Negative example:',En)
		  ,debug_clauses(metasubstitution,'Instantiated metarule:',H)
		  ,metasubstitution(En,M,H)
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


%!	metasubstitution(+Example,+Metarule,+Metarules,-Metasubss) is
%!	nondet.
%
%	Derive a list of Metasubs entailing an Example.
%
%	As metasubstitution/3 but this one can derive multiple
%	metasubstitutions of either Metarule or a metarule in Metarules.
%
%	Example is a positive example, only.
%
%	Metarule is one expanded metarule.
%
%	Metarules is the list of all metarules for the current MIL
%	problem, expandned.
%
%	Metasubs is a list of pairs: Sub-Metarule, where Sub is a ground
%	metasubstitution atom and Metarule is a non-ground metarule in
%	Metarules, which may or may not be Metarule.
%
%	The point of this variant is that metasubstitutions are derived
%	by meta-interpretation with a call to resolve_metarules/4, which
%	is capable of resolving the single input metarule M with a)
%	positive examples (including, but not only, Example), b)
%	Metarule itself, c) other metarules in Metarules and d) clauses
%	in the Top Program derived so-far. So, everything, really.
%
%	Most of those capabilities veer off towards Metagol, or anyway,
%	search-based MIL and so are separated according to the value of
%	the configuration option prove_recursive/1. See
%	resolve_metarules/4 for details.
%
%	@tbd Since this now takes as input the entire list of Metarules
%	for this MIL problem, we could do the selection of single
%	metarules here, rather than in generalise/3. However, I'd like
%	to separate the "vanilla" version of TPC from this, more complex
%	version, in the future and for that we may need to have the
%	selection of the current metarule in generalise/3. Or not. We'll
%	see.
%
metasubstitution(E,M,MS,S-M_e):-
	E \= (:-_)
	,examples_targets([E],[T])
	,configuration:recursion_depth_limit(metasubstitution, DL)
	,copy_term(M,M_)
	,bind_head_literal(E,M_,(Sub:-(H,Ls)))
	,debug_clauses(metasubstitution,'Trying metasubstitution:',H:-Ls)
	% Not necessary?
	,metarule_clause(Sub,H,Ls)
	,(   DL = none
	->   resolve_metarules(T,Sub,MS,Subs,Ls)
	;    G = resolve_metarules(T,Sub,MS,Subs,Ls)
	    ,call_with_inference_limit(G,DL,_R)
	 )
	,member(S, Subs)
	% Has to be checked here.
	% Because resolve_metarules/2 backtracks over prove_recursive/1
	% So it can succeed even when resolve_metarules/4 fails
	% Leaving Sub non-ground.
	,ground(S)
	,S =.. [_,Id|_Ps]
	,expanded_metarules([Id],[M_e])
	,debug_clauses(metasubstitution,'Proved Metasubstitution:',S-M_e).



%!	metarule_clause(?Metasub,?Head,?Body) is det.
%
%	clause/2 alternative for metarules in the program database.
%
%	Abstracts calling clause/2 to get the body literals of metarules
%	that may have no body literals. Magick!
%
metarule_clause(Sub,L,Ls):-
	clause(Sub,(L,Ls))
	,!.
metarule_clause(Sub,L,true):-
	clause(Sub,(L)).


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


%!	resolve_metarules(+Target,?Metasub,+Metarules,-Metasubs,+Literals)
%!	is nondet.
%
%	Resolve a set of Literals with a set of Metarules.
%
%	Top-level for resolve_metarules/3. See that predicate for
%	details.
%
resolve_metarules(T,Sub,MS,Subs,Ls):-
	proof_steps(Ss)
	,resolve_metarules(Ss,T,[Sub],MS,Subs,Ls).


%!	proof_steps(-Steps) is det.
%
%	Construct a listing of allowed recursive proof Steps.
%
%	Steps is a list [E,T,S,O], where each element is an atom 't' or
%	'f', denoting "true" and "false" respectively. Each of the
%	elements in the list stands for one of the recognised values of
%	the configuration option prove_recursive/1: E for 'examples', T
%	for 'top_program, S for 'self' and O for 'others'. The list
%	is populated with a 't' for each value actually set in the
%	configuration, and 'f' for each value not actually set in the
%	configuration.
%
%	For example, if only the values 'examples' and 'self' are set,
%	the list Steps will be [t,f,t,f].
%
%	Steps is passed to resolve_metarules/5 to determine how to prove
%	metasubstitutions.
%
%	Yeah, I know it looks procedural as all hell, but it's also very
%	much more efficient than calling memberchk/2 on a list of
%	options in each clause of resolve_metarules/5. _Much_ more.
%
proof_steps(Ss):-
	Vs = [examples
	     ,top_program
	     ,self
	     ,others]
	,Ss = [_E,_T,_S,_M]
	,findall(T
		,(member(V,Vs)
		 ,(   configuration:prove_recursive(V)
		  ->  T = t
		  ;   T = f
		  )
		 )
		,Ss).


%!	resolve_metarules(+Target,+Steps,?Metasub,+Metarules,-Acc,+Literals)
%!	is nondet.
%
%	Meta-interpreter for clauses and metarules.
%
%	Target is the predicate symbol and arity of the target
%	predicate, used to only keep metasubstitutions of metarules
%	that are clauses of the Target. Otherwise, this meta-interpreter
%	can perform accidentaly non-Observational Predicate Learning.
%
%	Steps is the list [E,T,S,O], built by proof_steps/1 as a compact
%	representation of all the values of the configuration option
%	prove_recursive/1, which determines how Literals are proved. If
%	the first element of Steps, E, is 't', meaning that
%	prove_recursive/1 is set to 'examples', Literals are resolved
%	with the positive examples. If the second element, T, is 't',
%	meaning that prove_recursive/1 is set to 'self', Literals is
%	resolved recursively with the "current" metarule (bound to the
%	second argument of metasubstitution/4). If S is 't', meaning
%	that prove_recursive/1 is set to 'top_program', Literals is
%	resolved with the clauses added to the Top-Program so-far (these
%	are asserted to the BK upon a completed proof). If O is 't',
%	meaning that prove_recursive/1 is set to 'others', Literals
%	is resolved with every metarule in the current MIL problem
%	except for the "current" metarule (in the second argument of
%	metasubstitution/4). Each element of Steps that is not 't' is
%	'f', meaning that the corresponding prove_recursive/1 option is
%	"off" (unset, whatever you want to call it). The combination of
%	't' and 'f' atoms in Steps determines how Literals is resolved.
%
%	Metasub is a partially ground metasubstitution atom, found in
%	the head of an expanded metarule. That is the "current"
%	metarule, selected in generalise/3 and passed as the second
%	argument of metasubstitution/4.
%
%	Metarules is the list of expanded metarules for the current MIL
%	Problem. These are resolved with iff the last element of Steps
%	is 't'.
%
%	Acc is a list of metasubstitutions of metarules in Metarules
%	derived during proof of Literals. These may include
%	metasubstitutions of any metarules in Metarules, or the
%	metarule of Metasub itself.
%
%	Literals is the set of body literals of an expanded metarule.
%	Literals in the given metarule are resolved recursively with
%	that metarule, positive examples, background predicates, other
%	metarules, and clauses in the Top Program, according to Prove.
%
%	__Motivation__
%
%	This predicate is called by metasubstitution/4 to attempt to
%	construct clauses by resolving them with the metarules of which
%	they are instances, as well as with the BK.
%
%	The motivation for this is to be able to add to the Top Program
%	clauses that can only resolve with themselves. Since the TPC
%	algorithm constructs a clause during resolution it's not
%	possible to construct a clause that can only resolve with
%	itself, because we don't have the clause in the first place.
%	What we do have is metarules of which each clause in the Top
%	Program is an instance, and we can implicitly resolve clauses
%	with themselves by resolving metarules with themselves and
%	keeping the metasubstitutions resulting from successful
%	resolution. This is what this meta-interpreter does.
%
%	A meta-interpreter is needed here because of the internal
%	representation of metarules in Louise, as "expanded" metarules
%	with a metasubstitution atom in the head and the litearls of the
%	actual metarule clause in the body. The meta interpreter is used
%	to split the metasubstitution atom in the head of a metarule
%	from the literals of the metarule in the body and resolve only
%	the literals in the body.
%
%	At the same time this meta-interpreter is responsible for
%	ensuring that metarules are _only_ resolved with themselves (not
%	other metarules) by filtering the clauses in the program
%	database by their heads, which should match the Metasubstitution
%	atom at the head of an expanded metarule. Provided there is only
%	a single metarule with the same metasubstitution atom in its
%	head (as it should) it is only possible to resolve such a
%	metarule with itself, using this meta-interpreter.
%
%	__Addendum__
%
%	Actually, this meta-interpreter is now capable of also resolving
%	metarules with other metarules to derive mutually recursive
%	clauses.
%
%	@tbd The ability to derive clauses of non-observational
%	predicates is interesting and should probably be explicitly
%	allowed in the future.
%
resolve_metarules(_,Tgt,[Sub|Ss],_MS,Subs,true):-
% Only keep metasubstitutions that are clauses of a target predicate.
	!
        ,(   target_atom(Tgt,Sub)
	 ->  Subs = [Sub|Ss]
	 ;   Subs = Ss
	 )
	,debug_clauses(self_resolution,'Proved atom:',[Sub]).
resolve_metarules(P,Tgt,Sub,MS,Acc,(L,Ls)):-
% Split the proof tree.
	resolve_metarules(P,Tgt,Sub,MS,Acc_1,L)
	,resolve_metarules(P,Tgt,Acc_1,MS,Acc,Ls).
resolve_metarules(P,Tgt,[Sub|Ss],MS,Acc,(L)):-
% L is an atom of a foreign predicate in the body of a BK clause.
% clause/2 would raise an access permission error.
% So we just call(L).
	L \= (_,_)
	,predicate_property(L,foreign)
	,call(L)
	,debug_clauses(self_resolution,'Proved foreign literal:',[L])
	,resolve_metarules(P,Tgt,[Sub|Ss],MS,Acc,true).
resolve_metarules(P,Tgt,Subs,MS,Acc,(L)):-
% L unifies with the head of a BK predicate.
	L \= (_,_)
	,\+ predicate_property(L,foreign)
	% This grounds L.
	,clause(L,Bs)
	% So we must call this only after clause/2.
	,bk_atom(Tgt,L)
	,debug_clauses(self_resolution,'Proving BK literal:',[L])
	,resolve_metarules(P,Tgt,Subs,MS,Acc,Bs).
resolve_metarules([E,T,t,O],Tgt,[Sub|Ss],MS,Acc,(L)):-
% L unifies with the head of the "current" expanded metarule.
	L \= (_,_)
	,\+ predicate_property(L,foreign)
	,metarule_clause(Sub,L,Ls)
	,debug_clauses(self_resolution,'Proving metarule literals:',[L:-Ls])
	,resolve_metarules([E,T,t,O],Tgt,[Sub|Ss],MS,Acc,Ls).
resolve_metarules([E,T,S,t],Tgt,[Sub|Ss],MS,Acc,(L)):-
% L unifies with the head of an expanded metarule in MS.
	L \= (_,_)
	,\+ predicate_property(L,foreign)
	,ground(L)
	,ground(Sub)
	,member(Sub_:-_,MS)
	% Only allows resolution between different metarules.
	,\+ unifiable(Sub,Sub_,_)
	,metarule_clause(Sub_,L,Ls)
	,debug_clauses(self_resolution,'Proving other metarule literals:',[L:-Ls])
	,resolve_metarules([E,T,S,t],Tgt,[Sub_,Sub|Ss],MS,Acc,Ls).


%!	target_atom(+Symbol,+Literal) is det.
%
%	True when Literal is an atom of the given predicate.
%
%	Symbol is the predicate symbol and arity of a target predicate
%	in the current MIL problem.
%
%	Literal is an atom, which may be a metasubstitution atom.
%	target_atom/2 checks that Literal is an atom of the target
%	predicate.
%
%	This predicate is used to ensure that only metasubstitutions of
%	target predicates are returned by the resolve_metarules/6
%	meta-interpreter.
%
target_atom(F/A, L):-
	L =.. [S,F|As]
	,memberchk(S,[m,p])
	,length(As,A)
	,!.
target_atom(F/_A, L):-
	L =.. [m,Id,F|_As]
	,configuration:metarule(Id,_).


%!	bk_atom(?Literal) is det.
%
%	True when Literal is an atom of a BK predicate.
%
%	Literal is an encapsulated atom of a BK predicate. This
%	predicate checks that Literal is not the metasubstitution atom
%	of an expanded metarule.
%
%	Used to avoid forming metasubstitutions with metarule
%	identifiers in the place of predicate symbols, which can happen
%	when a literal that is currently being refuted unifies with the
%	metasubstitution atom at the head of an expanded metarule clause
%	in the dynamic database.
%
bk_atom(_,L):-
	L =.. [S,F|_As]
	,memberchk(S,[m,p])
	% Avoid resolving with metasub atoms.
	,\+ configuration:metarule(F,_).


%!	assert_clause(+Metasub,-Refs) is det.
%
%	Assert a clause to the dynamic database.
%
%	This predicate is responsible for adding clauses to the dynamic
%	database so that they can be resolved against during
%	construction of new clauses, when the configuration option
%	prove_recursive(self) is set.
%
%	Metasub is a pair Sub-Metarule where Sub is a ground
%	metasubstitution atom and Metarule is an expanded metarule. Refs
%	is a list of a single element, the clause reference of the
%	clause added to the dynamic db.
%
%	Refs is used to remove clauses from the dynamic db at the end of
%	generalise/3, in order to avoid confusingly proving negative
%	examples.
%
assert_clause(_Sub,[]):-
	\+ configuration:prove_recursive(top_program)
	,!.
assert_clause(Sub-M,Refs):-
	applied_metarules([Sub-M],[M],[C])
	,assert_program(user,[C],Refs)
	,debug_clauses(co_resolution,'Asserted clause:',[C]).



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


%!	generalise(+Metarules,+Program,-Model,-Generalised) is
%!	det.
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
generalise(MS,Ps,Is,Ts_Pos):-
	lfp_query(MS,Ps,Is,Ts_Pos)
	%,writeln('Top program - generalise:')
	%,print_clauses(Ts_Pos)
	%,nl
	%,writeln('Top program - Interpretation')
	%,print_clauses(Is)
	%,nl
	.


%!	specialise(+Generalised,+Model,+Negatives,-Specialised) is
%!	det.
%
%	Top program specialisation step with TP operator.
%
%	Generalised is the result of generalise/4. Model is the least
%	Herbrand model of the positive examples and background
%	knowledge, calculated during execution of generalise/4.
%
specialise(Ts_Pos,Ps,Neg,Ts_Neg):-
	findall(H:-B
		,(member(H:-B,Ts_Pos)
		 ,lfp_query([H:-B],Ps,As)
		 ,ord_intersection(As, Neg, [])
		 )
		,Ts_Neg)
	%,writeln('Top program - specialise:')
	%,print_clauses(Ts_Neg)
	%,nl
	.


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
	,forall(configuration:metarule_constraints(Sub, C)
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
