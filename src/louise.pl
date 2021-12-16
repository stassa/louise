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
	,examples_targets(Pos,Ss)
	% The following tests determine the structure of the program db.
	,(   configuration:prove_recursive(examples)
	 ->  Ws_ = [Pos,BK]
	 ;   Ws_ = [BK]
	 )
	,(   configuration:prove_recursive(H)
	    ,member(H,[self,others,invented])
	 ->  Ws = [MS|Ws_]
	 ;   Ws = Ws_
	 )
	,S = (write_problem(user,Ws,Refs)
	     ,dynamic_learning:table_encapsulated(Ss)
	     )
	,G = (debug(top_program,'Constructing Top program...',[])
	     ,generalise_specialise(Pos,Neg,MS,Subs)
	     ,debug_clauses(top_program,'Constructed Top program:',Subs)
	     ,applied_metarules(Subs,MS,Ts)
	     ,debug_clauses(top_program,'Applied metarules:',Ts)
	     )
	,C = (erase_program_clauses(Refs)
	     ,dynamic_learning:untable_encapsulated(Ss)
	     % TODO: this is mainly to catch clauses written to the
	     % TODO: dynamic db by generalise/3 for which we don't
	     % TODO: have references. Is there a better way to do it?
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
	,generalise_tp(MS_,Ps,Is,Ts_Pos)
	,applied_metarules(Ts_Pos,MS,Ts_Pos_)
	,specialise_tp(Ts_Pos_,Is,Neg,Ts)
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
	      ,debug_msg_metarules(metasubstitution,'Instantiating metarule:',M)
	      ,metasubstitution(Ep,M,Sub)
	      ,constraints(Sub)
	      )
	     ,Ps)
	,sort(1,@<,Ps,Ss_Pos).



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
		  ,debug_clauses(metasubstitution,'Ground metasubstitution atom:',H)
		  ,metasubstitution(En,M,H)
		  )
		 )
	      )
	     ,Ss_Neg).



%!	generalise_specialise(+Pos,+Neg,+Metarules,-Metasubs) is det.
%
%	Variant Top Program Construction implementation.
%
%	This predicate implements a variant of the Top Program
%	Construction algorithm designed to learn more kinds of recursive
%	clauses than the "vanilla" implementation.
%
%	The "vanilla" TPC version is only capable of constructing
%	recursive clauses that can resolve with positive examples. The
%	variant implemented by this predicate can also construct
%	recursive clauses that resolve with other clauses in the Top
%	Program, such as self-resolving clauses or a pair of a recursive
%	clause and its base-case.
%
%	The "vanilla" implementation of TPC is split into two steps,
%	generalisation and specialisation. Generalisation adds to the
%	Top Program all clauses that entail one or more positive
%	examples with respect to background knowledge. The
%	specialisation step removes from the Top Program each clause
%	that entails one or more negative examples. In the vanilla TPC
%	version, first all positive examples are processed in the
%	generalisation step, then all negative examples are processed in
%	the specialisation step. The variant implemented by this
%	predicate instead combines the generalise and specialise step
%	into one: first it constructs all clauses that entail one
%	positive example with respect to background knowledge, then it
%	throws out all the ones that entail any negative example. Then
%	it picks up the next positive example. The increased overhead is
%	justified by the reduced opportunity for over-generalisation.
%
%	This variant also hands proofs to the metarule meta-interpreter
%	resolve_metarules/5, rather than directly to Prolog. The
%	resolve_metarules/5 meta-interpreter allows resolution with, you
%	guessed it, metarules, which avoids all sorts of complications
%	that arise from the fact that TPC must first prove a clause
%	before adding it to the Top Program.
%
%	In general this variant runs slower, but is more complete than
%	"vanilla" TPC.
%
%	@see the configuration option prove_recursive/1 that controls
%	the resolve_metarules/5 meta-interpreter.
%
generalise_specialise(Pos,Neg,MS,Ss_Pos):-
% Tabling keeps resolve_metarules/6 from going infinite during
% meta-interpretation if possible. Tabled as incremental because
% encapsulated clauses may be added to or removed from the dynamic db
% during meta-interpretation.
        table(resolve_metarules/6 as incremental)
	,configuration:max_invented(I)
	,invented_symbols(I,Is)
	,findall(Sub-M-Refs
	     ,(member(Ep,Pos)
	      ,debug_clauses(metasubstitution,'Positive example:',Ep)
	      ,metasubstitutions(Is,Ep,MS,Sub-M)
	      ,\+((member(En,Neg)
		  ,debug_clauses(dynamic,'Negative example:',[En])
		  ,metasubstitution(En,M,Sub)
		  ))
	      ,assert_clause(Sub-M,Refs)
	      )
	     ,Ps)
	,untable(resolve_metarules/6)
	% TODO: References are ignored because it's a bother to remove them.
	% TODO: There should be a better way to do this.
	,pairs_keys_values(Ps,Ss_Pos_,_Rs)
	,sort(1,@<,Ss_Pos_,Ss_Pos).



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


%!	metasubstitutions(+Invented,+Example,+Metarule,+Metarules,-Metasubs)
%!	is nondet.
%
%	Derive all Metasubstitutions entailing an Example.
%
%	As metasubstitution/3 but this one can derive multiple
%	metasubstitutions of multiple Metarules. Additionally, proofs
%	are performed by the metarule meta-interpreter
%	resolve_metarules/5.
%
%	Invented is a list of symbols reserved for invented predicates,
%	up to the maximum defined in the configuration option
%	max_invented/1.
%
%	Example is a positive example, only.
%
%	Metarules is the list of all metarules for the current MIL
%	problem, expandned.
%
%	Metasubs is a list of pairs: Sub-Metarule, where Sub is a ground
%	metasubstitution atom and Metarule is a non-ground metarule in
%	Metarules, which may or may not be Metarule.
%
%	The point of this variant is that metasubstitutions are derived
%	by meta-interpretation with a call to resolve_metarules/5, which
%	is capable of resolving a metarule M in Metarules with a)
%	positive examples (including, but not only, Example), b) M
%	itself, c) other metarules in Metarules and d) clauses in the
%	Top Program derived so-far. So, everything, really.
%
metasubstitutions(Is,E,MS,S-M_e):-
	configuration:recursion_depth_limit(metasubstitution, DL)
	,member(M,MS)
	,debug_msg_metarules(metasubstitution,'Instantiating metarule:',M)
	,copy_term(M,M_)
	,bind_head_literal(E,M_,(Sub:-(H,Ls)))
	,debug_clauses(metasubstitution,'Trying metasubstitution:',H:-Ls)
	,(   DL = none
	 ->  resolve_metarules(Is,Sub,MS,Subs,Ls)
	 ;   G1 = resolve_metarules(Is,Sub,MS,Subs,Ls)
	    ,D = ( debug(metasubstitution,'Inference limit exceeded!',[])
		  ,fail
		 )
	    ,G2 = catch(G1,inference_limit_exceeded,D)
	    ,call_with_inference_limit(G2,DL,_R)
	 )
	,member(S, Subs)
	% TODO: is this	needed?
	% TODO: Can resolve_metarules/4 succeed but leave S non-ground?
	,ground(S)
	,(   configuration:test_constraints(clause)
	 ->  debug_clauses(const,'Testing constraint for metasub:',[S])
            ,constraints(S)
	 ;   true
	 )
	,S =.. [_,Id,T|_Ps]
	,examples_targets([E],Ts)
	% Now we check that S is a clause of the target predicate.
	% And that it does not have a metarule id as a symbol.
	% Passing only a symbol without arity is asking for trouble...
	,target_or_invention(Ts,T/_)
	,expanded_metarules([Id],[M_e])
	,debug_clauses(metasubstitution,'Proved Metasubstitution:',S-M_e).


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


%!	resolve_metarules(+Counter,?Metasub,+Metarules,-Metasubs,+Literals)
%!	is nondet.
%
%	Resolve a set of Literals with a set of Metarules.
%
%	Top-level for resolve_metarules/3. See that predicate for
%	details.
%
resolve_metarules(Is,Sub,MS,Subs,Ls):-
	proof_steps(Ss)
	,resolve_metarules(Is,Ss,[Sub],MS,Subs,Ls).


%!	proof_steps(-Steps) is det.
%
%	Construct a listing of allowed recursive proof Steps.
%
%	Steps is a list [E,T,S,O,I], where each element is an atom 't'
%	or 'f', denoting "true" and "false" respectively. Each of the
%	elements in the list stands for one of the recognised values of
%	the configuration option prove_recursive/1: E for 'examples', T
%	for 'top_program, S for 'self', O for 'others' and I for
%	'invented'. The list is populated with a 't' for each value
%	actually set in the configuration, and 'f' for each value not
%	actually set in the configuration.
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
	     ,others
	     ,invented]
	,Ss = [_E,_T,_S,_O,_I]
	,findall(T
		,(member(V,Vs)
		 ,(   configuration:prove_recursive(V)
		  ->  T = t
		  ;   T = f
		  )
		 )
		,Ss).


%!	resolve_metarules(+Invented,+Steps,?Metasub,+Metarules,-Acc,+Literals)
%!	is nondet.
%
%	Meta-interpreter for clauses and metarules.
%
%	Invented is a list of symbols reserved for invented predicates.
%	The number of symbols in Invented is equal to the value of the
%	configuration option max_invented/1.
%
%	Steps is the list [E,T,S,O,I], built by proof_steps/1 as a
%	compact representation of all the values of the configuration
%	option prove_recursive/1, which determines how Literals are
%	proved. If the first element of Steps, E, is 't', meaning that
%	prove_recursive/1 is set to 'examples', Literals are resolved
%	with the positive examples. If the second element, T, is 't',
%	meaning that prove_recursive/1 is set to 'self', Literals is
%	resolved recursively with the "current" metarule (bound to the
%	second argument of metasubstitution/4). If S is 't', meaning
%	that prove_recursive/1 is set to 'top_program', Literals is
%	resolved with the clauses added to the Top-Program so-far (these
%	are asserted to the BK upon a completed proof). If O is 't',
%	meaning that prove_recursive/1 is set to 'others', Literals is
%	resolved with every metarule in the current MIL problem except
%	for the "current" metarule (in the second argument of
%	metasubstitution/4). If I is 't', meaning that prove_recursive/1
%	is set to 'invented' predicate invention is attempted to
%	resolve Literals. Each element of Steps that is not 't' is 'f',
%	meaning that the corresponding prove_recursive/1 option is "off"
%	(unset, whatever you want to call it). The combination of 't'
%	and 'f' atoms in Steps determines how Literals is resolved.
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
%	@tbd This predicate will happily derive clauses of
%	non-observational predicates. This is interesting and
%	should probably be explicitly allowed in the future. Currently,
%	metasubstitutions/3 weeds out clauses of those predicates. We
%	don't remove them here because that would cut off necessary
%	branches of the proof tree.
%
%	@tbd The predicate invention in this meta-interpreter is
%	currently extremely rickety. In particular, I'm not convinced
%	that invented_literal/2 is capable of inventing more than one
%	symbol for each example. More work is needed before dynamic
%	learning can be replaced.
%
resolve_metarules(_Is,_P,[Sub|Ss],_MS,[Sub|Ss],true):-
% When a proof completes there may be metasubstitutions that just got
% fully ground, e.g. because we just finished proving some invented
% predicate. So we must test all the metasubstitutions we have so far.
% TODO: is there no better way to do this?
	constrained_metasubs(Sub)
	,!
	,debug_clauses(meta_interpreter,'Proved metasub:',[Sub])
	,debug_clauses(meta_interpreter,'Accumulated metasubs:',[Ss]).
resolve_metarules(Is,P,Sub,MS,Acc,(L,Ls)):-
% Split the proof tree.
% I actually had to do this kind of thing in java, once. :shiver:
	debug_clauses(meta_interpreter,'Proving one literal:',[L])
	,resolve_metarules(Is,P,Sub,MS,Acc_1,L)
	,debug_clauses(meta_interpreter,'Proving remaining literals:',[Ls])
	,resolve_metarules(Is,P,Acc_1,MS,Acc,Ls).
resolve_metarules(Is,P,Subs,MS,Acc,(L)):-
% L will be proved by calling it directly.
% If L can be proved by calling it, that usually means that L unifies
% with the head of a BK predicate for which we have a complete
% definition, or a positive example.
        L \= (_,_)
	,call(L)
	,debug_clauses(meta_interpreter,'Proved BK atom:',[L])
	,resolve_metarules(Is,P,Subs,MS,Acc,true).
resolve_metarules(Is,P,Subs,MS,Acc,(L)):-
% L will be proved by meta-interpretation.
% If L can be proved (only) by meta-interpretation usually that means
% that L unifies with the head of a clause of a partial definition of a
% predicate in the program database, most likely the definition of a
% target predicate that is in the process of being constructed, i.e. a
% clause in the Top Program.
	provable_literal(L)
	,clause(L,Bs)
	,\+ metasub_atom(L,MS)
	,debug_clauses(meta_interpreter,'Proving BK literal:',[L])
	,resolve_metarules(Is,P,Subs,MS,Acc,Bs).
resolve_metarules(Is,[E,T,t,O,I],[Sub|Ss],MS,Acc,(L)):-
% L will be proved by meta-interpretation with the "current" metarule.
% If Sub is not ground we keep trying to ground it. This forces
% resolution with the same instance of the current metarule, i.e. we
% only resolve a clause with a copy of itself.
	provable_literal(L)
	% Notice the check here. See note above.
	,\+ ground(Sub)
	,metarule_clause(Sub,MS,L,Ls)
	,constrained_metasub(Sub)
	,debug_clauses(meta_interpreter,'Proving metarule literals:',[L:-Ls])
	,resolve_metarules(Is,[E,T,t,O,I],[Sub|Ss],MS,Acc,Ls).
resolve_metarules(Is,[E,T,t,O,I],[Sub|Ss],MS,Acc,(L)):-
% L will be proved by meta-interpretation with the "current" metarule.
% If Sub is ground, we can try to construct a new instance of its parent
% metarule. This allows resolution between clauses that are instances of
% the same metarule, but that are not the same clause.
	provable_literal(L)
	,ground(Sub)
	,copy_term(Sub,Sub_)
	,metarule_clause(Sub_,MS,L,Ls)
	,constrained_metasub(Sub_)
	,debug_clauses(meta_interpreter,'Proving metarule literals:',[L:-Ls])
	,resolve_metarules(Is,[E,T,t,O,I],[Sub_|Ss],MS,Acc,Ls).
resolve_metarules(Is,[E,T,S,t,I],[Sub|Ss],MS,Acc,(L)):-
% L will be proved by meta-interpretation with a new metarule in MS.
	provable_literal(L)
	% TODO: Yes, but why?
	,ground(L)
	,ground(Sub)
        ,next_metarule(MS,Sub_)
	% Only allows resolution between different metarules.
	,\+ unifiable(Sub,Sub_,_)
	,metarule_clause(Sub_,MS,L,Ls)
	,constrained_metasub(Sub_)
	,debug_clauses(meta_interpreter,'Proving other metarule literals:',[L:-Ls])
	,resolve_metarules(Is,[E,T,S,t,I],[Sub_,Sub|Ss],MS,Acc,Ls).
resolve_metarules(Is,[E,T,S,O,t],[Sub|Ss],MS,Acc,(L)):-
% L will be proved by meta-interpretation with a new metarule in MS.
	provable_literal(L)
	,debug_clauses(meta_interpreter,'Try inventing a new symbol for literal:', [L])
	,invented_literal(Is,L,Is_,L)
	,debug(meta_interpreter,'Succeeded: ~w',[L])
        ,next_metarule(MS,Sub_)
	,metarule_clause(Sub_,MS,L,Ls)
	,constrained_metasub(Sub_)
	,debug_clauses(meta_interpreter,'Proving invented predicate literals:',[L:-Ls])
	,resolve_metarules(Is_,[E,T,S,O,t],[Sub_,Sub|Ss],MS,Acc,Ls).


%!	constrained_metasubs(+Metasubstitutions) is det.
%
%	Check constraints on a list of Metasubstitutions.
%
%	Called at the end of a proof to ensure that all
%	metasubstitutions derived so-far obey constraints.
%
constrained_metasubs(Subs):-
	forall(member(Sub,Subs)
	      ,constrained_metasub(Sub)
	      ).


%!	constrained_metasub(?Metasubstitution) is det.
%
%	Check constraints on a Metasubstitution.
%
%	Called whenever a new metasubstitution is constructed to ensure
%	that it does not violate constraints. Metasubstitution may not
%	be fully ground.
%
constrained_metasub(_Sub):-
	\+ configuration:test_constraints(proof)
	,!.
constrained_metasub(Sub):-
	configuration:test_constraints(proof)
	,debug_clauses(constraints,'Testing constraints for metasub:',[Sub])
	,copy_term(Sub,Sub_)
	,constraints(Sub_)
	,debug(constraints,'Constraint test succeeded!',[]).


%!	provable_literal(+Literal,+Metarules) is det.
%
%	True when a Literal is to be proved by meta-interpretation.
%
%	Checks the following:
%	* Literal is a single literal.
%	* Literal is not an atom of a foreign predicate.
%
provable_literal(L):-
	L \= (_,_)
	,\+ predicate_property(L,foreign)
	,\+ built_in_or_library_predicate(L).


%!	metasub_atom(+Atom,+Metarules) is det.
%
%	True Atom is a metasubstitution atom of an expanded metarule.
%
%	Used to check that a literal we are trying to prove is _not_ the
%	encapsulated metasubstitution atom of an expanded metarule.
%
metasub_atom(L,MS):-
	L =.. [m,S|_As]
	,member(Sub:-_,MS)
	,Sub =.. [m,Id|_]
	,S == Id.


%!	next_metarule(+Metarules,-Metasub) is nondet.
%
%	Generate metasubstitution atoms from a list of Metarules.
%
%	Avoids smushing together the variables in metasubstitution atoms
%	while going over Metarules with member/2. We want to avoid that
%	because it cuts off paths of the proof tree. One particular
%	situation where this becomes apparent is during predicate
%	invention where we end up not being able to invent more than one
%	new predicate symbol, because the same symbol gets passed around
%	inadvertently through unification at the boundary condition of
%	member/2.
%
next_metarule(MS, Sub_):-
	member(Sub:-_, MS)
	,copy_term(Sub,Sub_).


%!	invented_literal(+Invented,+Literal,-Literal_new,-Invented_new)
%!	is det.
%
%	Invent a predicate symbol for a Literal.
%
%	Creates invented predicate symbols for predicate invention in
%	resolve_metarules/5.
%
%	Invented is the list of invented symbols not yet used in the
%	current proof branch.
%
%	Literal is an encapsulated literal whose second-order variable
%	is as yet unbound.
%
%	Literal_new is Literal with its second-order variable bound to a
%	new invented predicate symbol selected from Invented. Invented
%	predicates' symbols are of the form '$n' where n is an integer
%	higher than 0.
%
%	Invented_new is the list of invented predicate symbols remaining
%	in Invented once a symbol is removed to add to Literal.
%
invented_literal(Ss,L,Ss_,L_):-
	L =.. [m,P|As]
	,\+ ground(P)
	,select(S,Ss,Ss_)
	,debug(predicate_invention,'Invented new predicate symbol: ~w',[P])
	,L_ =.. [m,S|As].


%!	assert_clause(+Metasub,-Refs) is det.
%
%	Assert a clause to the dynamic database.
%
%	This predicate is responsible for adding clauses to the dynamic
%	database so that they can be resolved with during construction
%	of new clauses, when the configuration option
%	prove_recursive(top_program) is set. If that options is not set,
%	nothing is written.
%
%	Metasub is a pair Sub-Metarule where Sub is a ground
%	metasubstitution atom and Metarule is an expanded metarule. Refs
%	is a list of a single element, the clause reference of the
%	clause added to the dynamic db.
%
%	Refs is meant to be used to remove clauses from the dynamic db
%	at the end of learning. However, this is currently not done and
%	instead cleanup_experiment/0 is called at the end of
%	top_program/5 to gather up any garbage left.
%
assert_clause(_Sub,[]):-
	\+ configuration:prove_recursive(top_program)
	,!.
assert_clause(Sub-M,Refs):-
	applied_metarules([Sub-M],[M],[C])
	,assert_program(user,[C],Refs)
	,debug_clauses(meta_interpreter,'Asserted clause:',[C]).


%!	metarule_clause(?Metasub,+Metarules,?Head,?Body) is det.
%
%	clause/2 alternative for metarules in the program database.
%
%	Abstracts calling clause/2 to get the body literals of metarules
%	that may have no body literals. Magick!
%
%	Metarules is the set of encapsulated metarules passed to
%	resolve_metarules/5. It is used to check that Head is not bound
%	to the metasubstitution atom of an expanded metarule.
%
metarule_clause(Sub,MS,L,Ls):-
	clause(Sub,(L,Ls))
	,\+ metasub_atom(L,MS)
	,!.
metarule_clause(Sub,MS,L,true):-
	clause(Sub,(L))
	,\+ metasub_atom(L,MS).



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
	lfp_query(MS,Ps,Is,Ts_Pos)
	%,writeln('Top program - generalise:')
	%,print_clauses(Ts_Pos)
	%,nl
	%,writeln('Top program - Interpretation')
	%,print_clauses(Is)
	%,nl
	.


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
