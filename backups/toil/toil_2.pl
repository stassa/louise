:-module(toil_2, [learn_metarules/1
	         %,learn_metarules/2
	         %,learn_metarules/5
		 %,top_program/5
		 %,generalise/3
		 %,specialise/3
		 %,prove/6
		 %,constraints/1
		 %,reduced_top_program/5
		 ]).

:-use_module(project_root(configuration)).
:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).

/** <module> Meta-Interpretive Learning of a second-order Top Program.

New version of TOIL, using the Vanilla meta-interpreter in Louise.
Currently a WIP. The goal is to allow TOIL to learn arbitrary recursion
and perform predicate invention, like Louise, but in the second-order.

*/

%!	learn_metarules(+Targets) is det.
%
%	Learn metarules for a list of learning Targets.
%
learn_metarules(Ts):-
	learn_metarules(Ts,Ps)
	,length(Ps,N)
	,debug(cardinality,'Learned ~w metarules',[N])
	,print_metarules(Ps).



%!	learn_metarules(+Targets,-Metarules) is det.
%
%	Learn a set of metarules for one or more learning Targets.
%
learn_metarules(Ts,_Ps):-
	(   \+ ground(Ts)
	->  throw('learn_metarules/2: non-ground target symbol!')
	;   fail
	).
learn_metarules(Ts,Ps):-
	tp_safe_experiment_data(Ts,Pos,Neg,BK,MS)
	,learn_metarules(Pos,Neg,BK,MS,Ps).



%!	learn_metarules(+Pos,+Neg,+BK,+Metarules,-Specialised) is det.
%
%	Learn a set of Specialised metarules from a MIL problem.
%
%	Pos is a list of ground unit clauses, the positive examples.
%
%	Neg is a list of ground, atomic Horn goals, the negative
%	examples.
%
%	BK is a list of predicate indicators F/A, of the predicate
%	definitions in the background knowledge.
%
%	Metarules is a list of metarule IDs. These should be the IDs of
%	higher-order metarules, i.e. generalised second-order metarules
%	and third-order metarules (a.k.a. "matrix" and "punch"
%	metarules, respectively).s
%
%	Specialised is a list of fully-connected second-order metarules,
%	specialising the higher-order Metarules with respect to the
%	examples and BK.
%
learn_metarules([],_Neg,_BK,_MS,_Ts):-
	throw('learn_metarules/5: No positive examples found. Cannot train.').
/*
learn_metarules(Pos,Neg,BK,MS,_Ts):-
	(   var(Pos)
	->  throw('learn_metarules/5: unbound positive examples list!')
	;   var(Neg)
	->  throw('learn_metarules/5: unbound negative examples list!')
	;   var(BK)
	->  throw('learn_metarules/5: unbound background symbols list!')
	;   var(MS)
	->  throw('learn_metarules/5: unbound metarule IDs list!')
	;   fail
	).
*/
learn_metarules(Pos,Neg,BK,MS,Ms):-
	%configuration:unfold_invented(U)
	%,configuration:fold_recursive(F)
	debug(learn,'Encapsulating problem...',[])
	,encapsulated_problem(Pos,Neg,BK,[],[Pos_,Neg_,BK_,_])
	,parsed_generalised_metarules(MS,MS_)
	,debug(learn,'Constructing Top program...',[])
	,top_program(Pos_,Neg_,BK_,MS_,Ms).



%!	parsed_generalised_metarules(+Ids,-Metarules) is det.
%
%	Parse a list of metarules to all-existentially quantified.
%
%	Ids is a list of names of generalised metarules.
%
%	Metarules is a list of encapsulated metarules, as returned by
%	expanded_metarules/2, but with all the variables in each literal
%	of a metarule kept in the encapsulated metasubstitution atom of
%	the encapsulated metarule. The purpose of this conversion is to
%	make it easier to keep the substitutions of the universally
%	quantified variables in a metarule. Remember that in prove/7, in
%	louise.pl, we only keep the existentially quantified variables
%	in metarules.
%
%	The following is an example of parsing generalised metarules.
%	For clarity, metarules are shown both in expanded, and
%	quantified format:
%
%	==
%	?- metarule_formatting(F).
%	F = expanded.
%
%	?- print_metarules([meta_dyadic])
%	,toil_2:parsed_generalised_metarules([meta_dyadic], _MS)
%	,print_metarules(_MS).
%
%	m(meta_dyadic,P,Q,R):-m(P,X,Y),m(Q,Z,U),m(R,V,W)
%	m(meta_dyadic,P,Q,R,X,Y,Z,U,V,W):-m(P,X,Y),m(Q,Z,U),m(R,V,W)
%	true.
%
%	?- metarule_formatting(F).
%	F = quantified.
%
%	?- print_metarules([meta_dyadic])
%	,toil_2:parsed_generalised_metarules([meta_dyadic], _MS)
%	,print_metarules(_MS).
%
%	(Meta-dyadic) ∃.P,Q,R ∀.x,y,z,u,v,w: P(x,y)← Q(z,u),R(v,w)
%	(Meta-dyadic) ∃.P,Q,R,X,Y,Z,U,V,W: P(X,Y)← Q(Z,U),R(V,W)
%	true.
%	==
%
%	In the example, note that the variables x,y,z,u,v,w which are
%	universally quantified in the meta_dyadic metarule before
%	parsing with parsed_generalised_metarules/2, are replaced with
%	existentially quantified ones after parsing. During learning,
%	substitutions of these variables are kept in the encapsulated
%	metasubstitution atom shown in the expanded metarule format,
%	i.e. the atom: m(meta_dyadic,P,Q,R,X,Y,Z,U,V,W).
%
parsed_generalised_metarules(Ids,MS):-
	findall(M
	       ,(member(Id,Ids)
		,parsed_generalised_metarule(Id,M)
		)
	       ,MS).

parsed_generalised_metarule(Id,M):-
	(   \+ configuration:metarule(Id, _)
	->  throw('Unknown metarule ID':Id)
	;   configuration:metarule(Id, M_)
	)
	,parsed_generalised_metarule(Id,M_,M).

%!	parsed_generalised_metarule(+Id,+Metarule,-Parsed) is det.
%
%	Business end of parsed_generalised_metarules/2.
%
parsed_generalised_metarule(Id,M,M1):-
	atom_chars(M,Cs)
	,metarules_parser:remove_whitespace(Cs,Cs_)
	,once(phrase(metarules_parser:clause_(Ls),Cs_))
	,metarules_parser:existential_vars(Ls,Es)
	,metarules_parser:args_vars(Es,Es_)
	,universal_vars(Ls,Us)
	,metarules_parser:args_vars(Us,Us_)
	,append(Es_,Us_,Vs)
	,A =.. [m,Id|Vs]
	,metarules_parser:literals_clause(Ls, M_)
	,varnumbers(A:-M_, M1).


%!	universal_vars(+Literals,-Universal) is det.
%
%	Collect universally quantified variables in a Literal.
%
universal_vars(Ls,Us):-
	flatten(Ls,Ls_)
	,metarules_parser:args_of_case(Ls_,lower,Us).



%!	top_program(+Pos,+Neg,+BK,+Metarules,-Top) is det.
%
%	Construct the Top program for a higher-order MIL problem.
%
%	Pos, Neg and BK are the examples and background knowledge for a
%	learning problem.
%
%	Metarules is a list of generalised second-order metarules or
%	third-order metarules, used to learn second-order metarules by
%	specialisation during SLD-Resolution.
%
%	Top is the second-order Top Program learned from the given
%	elements of the MIL problem.
%
top_program(Pos,Neg,BK,MS,Ms):-
	configuration:theorem_prover(resolution)
	,S = (write_problem(user,[BK],Refs)
	     ,table(prove_generalised/7)
	     )
	,G = (debug(top_program,'Constructing Top program...',[])
	     ,generalise_meta(Pos,MS,Ss_Gen)
	     ,debug_clauses(top_program,'Generalised Top program',Ss_Gen)
	     ,specialise_meta(Ss_Gen,MS,Neg,Ss_Spec)
	     ,debug_clauses(top_program,'Specialised Top program',Ss_Spec)
	     ,flatten(Ss_Spec,Ss_Spec_f)
	     ,findall(M
		     ,(member(_Sub-M,Ss_Spec_f)
		      ,numbervars(M)
		      )
		     ,Ms_)
	     ,sort(2,@>,Ms_,Ms_s)
	     ,findall(M_
		     ,(member(M,Ms_s)
		      ,varnumbers(M,M_)
		      )
		     ,Ms)
	     ,debug_clauses(top_program,'Applied metarules',Ss_Spec_f)
	     )
	,C = (erase_program_clauses(Refs)
	     ,untable(prove_generalised/7)
	     )
	,setup_call_cleanup(S,G,C)
	% Fail if Top Program is empty.
	,Ss_Spec_f \= []
	,!.
top_program(_Pos,_Neg,_BK,_MS,[]):-
% If Top program construction fails return an empty program.
% This is meant to send a clear message that learning failed.
	debug(top_program,'INSUFFICIENT DATA FOR MEANINGFUL ANSWER',[]).



%!	generalise_meta(+Positive,+Metarules,-Generalised) is det.
%
%	Generalisation step of higher-order Top program construction.
%
%	Generalises a set of Positive examples by finding each
%	metasubstitution of a metarule that entails a positive example.
%
%	Positive is the list of positive examples.
%
%	Metarules is a list of generalised second-order metarules or
%	third-order metarules, used to learn second-order metarules by
%	specialisation during SLD-Resolution.
%
%	Generalised is a set of key-value pairs where the keys are
%	ground metasubstitution atoms and the values are a copy with
%	free variables of the encapsulated head and body literals of the
%	metarule corresponding to the metasubsitution.
%
%	@tbd Needs more second-order explanation.
%
generalise_meta(Pos,MS,Ss_Pos):-
	configuration:clause_limit(K)
	,findall(Subs
		,(member(Ep,Pos)
		 ,debug_clauses(examples,'Positive example:',Ep)
		 ,generalised_metasubstitutions(Ep,K,MS,Subs)
		 )
		,Ss_Pos).



%!	specialise_meta(+Generalised,+Metarules,+Negatives,-Specialised)
%!	is det.
%
%	Specialisation step of second-order Top program construction.
%
%	Specialises a set of metasubstitutions generalising the positive
%	examples against the Negative examples by discarding each
%	metasubstitution that entails a negative example.
%
%	@tbd Needs more second-order explanation.
%
specialise_meta(Ss_Pos,_MS,[],Ss_Pos):-
	!.
specialise_meta(Ss_Pos,MS,Neg,Ss_Neg):-
	configuration:clause_limit(K)
	,findall(Subs_t
	       ,(member(Subs_g,Ss_Pos)
		,debug_clauses(specialise_meta,'Input metasubs:',[Subs_g])
		,transform_metasubs(Subs_g,MS,Subs_t)
		,debug_clauses(specialise_meta,'Transformed metasubs:',[Subs_t])
		,pairs_keys_values(Subs_t,Subs_,MS_)
		,debug_clauses(specialise_meta,'Ground metasubstitutions:',[Subs_])
		,debug_metarules(specialise_meta,MS_)
		,\+((member(En,Neg)
		    ,debug_clauses(examples,'Negative example:',En)
		    ,once(louise:metasubstitutions(En,K,MS_,Subs_))
		    ,debug_clauses(examples,'Proved negative example:',En)
		    )
		   )
		)
	       ,Ss_Neg).



%!	generalised_metasubstitutions(+Example,+Limit,+Metarules,-Metasubstitutions)
%!	is nondet.
%
%	Derive all possible Metasubstitutions entailing an Example.
%
%	Limit is the clause limit configured in the configuration.
%
generalised_metasubstitutions(Ep,K,MS,Subs_s):-
	debug(generalised_metasubstitutions,'Proving Example: ~w',[Ep]),
	louise:signature(Ep,Ss)
	,debug(signature,'Signature: ~w',[Ss])
        ,prove_generalised(Ep,K,[],MS,Ss,[],Subs_)
	,debug(generalised_metasubstitutions,'Proved Example: ~w',[Ep])
	,Subs_ \= []
	,ground(Subs_)
	,sort(Subs_,Subs_s)
	,debug_clauses(proved_metasubs,'Proved Metasubs:',[Subs_s]).


%!	prove_genralised(?Literals,+Limit,+Buffer,+Metarules,+Sig,+Acc,-Subs)
%	is nondet.
%
%	Higher-order version of the Vanilla MIL meta-interpreter.
%
%	Literals is the vector(?) of current literals being proved.
%
%	Limit is the value of the clause_limit/1 configuration setting.
%
%	Buffer is the substitution buffer, a list of lists where each
%	sublist holds the first-order terms in a metasubstitution.
%
%	Metarules is a list of generalised second-order metarules or
%	third-order metarules, used to learn second-order metarules by
%	specialisation during SLD-Resolution.
%
%	Sig is the predicate signature: a list of predicate symbols of
%	the target predicates, and any invented predicates (in that
%	order).
%
%	Acc is the accumulator of metasubstitutions.
%
%	Subs is the list of learned metasubstitutions of the metarules
%	in Metarules.
%
prove_generalised(true,_K,Cs,_MS,_Ss,Subs,Subs):-
	debug(clause,'Clause true',[]),
	!
	,debug(prove,'Metasubs so-far (true): ~w',[Subs])
	,debug(buffer,'Substitution buffer so-far (true): ~w',[Cs])
	,(   ground(Subs)
	 ->  debug(prove,'Ground metasubs!',[])
	 ;   true
	 )
	,connected_subs(Cs,Subs)
	,debug(buffer,'Passed connectedness check',[]).
prove_generalised((L,Ls),K,Cs,MS,Ss,Subs,Acc):-
	debug(clause,'Clause split (1)',[]),
	debug(prove,'Proving literals: (~w,~w)',[L,Ls]),
	prove_generalised(L,K,Cs,MS,Ss,Subs,Subs_)
	,debug(clause,'Clause split (2)',[])
	,debug(prove,'Proving literals: (~w)',[Ls])
        ,prove_generalised(Ls,K,Cs,MS,Ss,Subs_,Acc).
prove_generalised((L),K,Cs,MS,Ss,Subs,Acc):-
        L \= (_,_)
	,L \= true
	,debug(clause,'Clause prove',[])
	,debug(prove,'Proving literal: (~w)',[L])
        ,clause(L,K,Cs,MS,Ss,Subs,Cs_,Subs_,Ls)
        ,prove_generalised(Ls,K,Cs_,MS,Ss,Subs_,Acc).
 % Uncomment for richer debugging and logging.
prove_generalised(L,_K,Cs,_MS,_Ss,Subs,_Acc):-
	L \= true
	,debug(clause,'Clause fail',[])
        ,debug(prove,'Failed to prove literal: ~w',[L])
	,debug(prove,'Metasubs so-far (fail): ~w',[Subs])
	,debug(buffer,'Substitution buffer so-far (fail): ~w',[Cs])
	,fail.


%!	connected_subs(+Buffer,+Subs) is det.
%
%	True when each ground metasub in Subs is fully-connected.
%
%	Buffer is the substitution buffer, a list of lists where each
%	sublist holds the first-order terms in a metasubstitution.
%
%	Subs is the list of metasubstitutions derived during
%	meta-interpretation by prove_generalised/7. Note that, for each
%	metasubstitution added to Subs (by the last clause of clause/9)
%	a substitution buffer is also added to the list, in the same
%	index as the corresponding metasubstitution in Subs.
%
%	As the proof by prove_generalised/7 proceeds, terms in each
%	sub-list in the substitution buffer become bound to constants.
%	These are counted by counts/3 and their counts inspected to find
%	if any are 1. If any are, that means at least one constant bound
%	to a first-order variable in the corresponding metasubstitution
%	will become a free variable when the metasubstitution is
%	variabilised, thus resulting in a metarule that is not
%	fully-connected. This heuristic only applies to ground
%	metasubstitutions.
%
connected_subs([],[]):-
	!.
connected_subs([Cs_i|Cs],[Sub|Subs]):-
	connected_literals(Cs_i,Sub)
	,connected_subs(Cs,Subs).


%!	connected_literals(+Buffer,+Metasubstitution) is det.
%
%	Connectedness check for the literals in a Metasubstitution.
%
%	Buffer is the substitution buffer for Metasubstitution, in other
%	words, a list of first order terms in Metasubstitution.
%
%	Metasubstitution is a metasubstitution. Duh.
%
%	This predicate checks that Metasubstitution is fully ground. If
%	it is not, connected_literals/2 succeeds. If Metasubstitution is
%	fully ground, the occurence of constants in Buffer (bound to the
%	first-order variables in Metasubstitution) are counted. If any
%	are found to occur only once, that means that Metasubstitution
%	will not be variabilised into a fully-connected metarule. Thus,
%	it must be rejected.
%
connected_literals(_Cs,Sub):-
	\+ ground(Sub)
	,!.
connected_literals(Cs,Sub):-
	ground(Sub)
	,debug(connected_literals,'Ground metasub: ~w',[Sub])
	,counts(Cs,[],Ns)
	,debug(connected_literals,'Substitution buffer: ~w',[Cs])
	,debug(connected_literals,'Buffer counts: ~w',[Ns])
	,forall(member(_C-N,Ns)
	       ,N > 1)
	,debug(connected_literals,'Buffer is fully-connected',[]).



%!      counts(+Terms,?Counts,-Updated) is det.
%
%       Count or update the Counts of a list of Terms.
%
counts(Ts,Cs,Cs_u):-
        sort(0,@=<,Ts,Ts_)
        ,counts_(Ts_,Cs,Cs_)
        ,findall(C-N
                ,order_by([asc(N)]
                         ,member(C-N,Cs_)
                         )
                ,Cs_u).

%!      counts(+Terms,?Counts,+Acc,-Updated) is det.
%
%       Business end of counts/3.
%
%	@tbd Stupid use of selectchk/3. Terms is sorted by counts/3. We
%	can forego the selectchk.
%
counts_([],Cs,Cs).
counts_([T1|Ts],Acc,Bind):-
% Variable - skip (and avoid binding).
        var(T1)
        ,!
        ,counts_(Ts,Acc,Bind).
counts_([T1|Ts],[],Bind):-
% Empty counts- initialise.
        ground(T1)
        ,!
        ,counts_(Ts,[T1-1],Bind).
counts_([T|Ts],[T-C|Cs],Bind):-
% Increment count of T.
        ground(T)
        ,!
        ,succ(C,C_)
        ,counts_(Ts,[T-C_|Cs],Bind).
counts_([Ti|Ts],[Tk-C|Cs],Bind):-
% Find current count of T and update.
        ground(Ti)
        ,Ti \== Tk
        ,selectchk(Ti-Ci,Cs,Cs_)
        ,!
        ,succ(Ci,Ci_)
        ,counts_(Ts,[Ti-Ci_,Tk-C|Cs_],Bind).
counts_([Ti|Ts],[Tk-C|Cs],Bind):-
% Start a count for new term.
        ground(Ti)
        ,Ti \== Tk
        ,counts_(Ts,[Ti-1,Tk-C|Cs],Bind).



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
%	@tbd Pending second-order explanation.
%
clause(_L,_K,Cs,_MS,_Ss,Subs,Cs,_Acc,_Ls):-
	\+ check_constraints(Subs)
	,!
	,fail.
clause(L,_K,Cs,_MS,_Ss,Subs,Cs,Subs,true):-
	debug(which_clause,'Clause BI',[]),
	(   predicate_property(L,foreign)
	;   built_in_or_library_predicate(L)
	)
	,debug(prove_BI,'Proving built-in literal: ~w', [L])
        ,call(L)
	,debug(prove_BI,'Proved built-in clause: ~w', [L:-true]).
clause(L,_K,Cs,_MS,_Ss,Subs,Cs,Subs,Ls):-
	debug(which_clause,'Clause BK',[]),
	\+ predicate_property(L,foreign)
	,\+ built_in_or_library_predicate(L)
	,debug(prove_BK,'Proving literal with BK: ~w', [L])
	,debug(prove_BK,'Metasubs: ~w', [Subs])
        ,clause(L,Ls)
	,debug(prove_BK,'Trying BK clause: ~w', [L:-Ls])
	,debug(prove_BK,'Updated metasubs: ~w', [Subs]).
clause(L,_K,Cs,MS,_Ss,Subs,Cs,Subs,Ls):-
	debug(which_clause,'Clause known',[]),
        debug(prove_known,'Proving literal with known metasubs: ~w',[L])
	% Don't waste time trying to prove p(...) atoms in BK closure.
	,L =.. [m|_]
        ,known_metasub(L,MS,Subs,Ls).
clause(L,K,Cs,MS,Ss,Subs,[Fs|Cs],Subs_,Ls):-
	debug(which_clause,'Clause new',[]),
	length(Subs,N)
	,N < K
        ,debug(prove_new,'Proving literal with new metasub: ~w',[L])
        ,debug(prove_new,'Metasubs so-far: ~w',[Subs])
        ,debug(buffer,'Substitution buffer: ~w',[Cs])
        ,new_metasub(L,MS,Ss,Subs,Subs_,Ls)
        ,debug(prove_new,'Updated metasubs: ~w',[Subs_])
	,first_order(L,Ls,Fs)
        ,debug(buffer,'Updated substitution buffer: ~w',[Fs]).



%!	first_order(+Head,+Body,-First_Order) is det.
%
%	Collect first-order terms in a clause.
%
%	Literal is the head literal of a clause.
%
%	Body is the list of body literals in that clause.
%
%	First_Order is the list of first-order terms in the clause.
%
%	@tbd Explain better.
%
first_order(L,Ls,Fs):-
	first_order_((L,Ls),[],Fs).

first_order_((L),Acc,Fs):-
	L \= (_,_)
	,L =.. [_Enc,_S|Ts]
	,append(Ts,Acc,Acc_)
	,reverse(Acc_,Fs)
	,!.
first_order_((L,Ls),Acc,Bind):-
	L =.. [_Enc,_S|Ts]
	,append(Ts,Acc,Acc_)
	,first_order_(Ls,Acc_,Bind).


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



%!	transform_metasubs(+Metasubs,+Metarules,-Transformed) is det.
%
%	Transform generalised metasubstitutions to metarules.
%
%	Used to transform a list of metasubstitutions of generalised
%	metarules, to a list of sort metarules.
%
%	Metasubs is a list of metasubstitutions returned by
%	prove_generalised/7. Each metasubstitution in Metasubs is
%	ground.
%
%	Metarules is a list of metarules expanded by
%	parsed_generalised_metarules/2.
%
%	Transformed is the list of metasubstitutions in Metasubs, but
%	with constants and symbols replaced by new variables, while of
%	course maintaining the "wiring" of symbols and constants in the
%	original, ground metasub.
%
%	Each metarule in Transformed is given a unique id, based on the
%	Id of its parent (generalised) metarule, plus a number added by
%	a call to gensym/2.
%
transform_metasubs(Subs_s,MS,Subs):-
	findall(Sub_2-(Sub_3:-M_)
		,(member(Sub_g,Subs_s)
		 ,louise:metasub_metarule(Sub_g,MS,M)
		 ,applied_metarules([Sub_g-M],MS,[H:-B])
		 ,toil:new_metarule(Sub_g:-(H,B),Sub_1:-M_)
		 ,literals_symbols((H,B),Syms)
		 ,second_order_variables((H,B),Sub_1,SOs)
		 ,Sub_g =.. [m,Id|_Ps]
		 ,gensym(Id,Id_u)
		 ,Sub_2 =.. [m,Id_u|Syms]
		 ,Sub_3 =.. [m,Id_u|SOs]
		 ,\+ tautological_metarule(Sub_3-M_)
		 )
		,Subs).


you_are_here.

%!	literals_symbols(+Literals,-Symbols) is det.
%
%	Collect the predicate Symbols of a list of Literals.
%
literals_symbols(Ls,Ss):-
	literals_symbols(Ls,[],Ss).

literals_symbols((L,Ls),Acc,Bind):-
	!
	,L =.. [m,S|_]
	,literals_symbols(Ls,[S|Acc],Bind).
literals_symbols((L),Ss,Ss_):-
	L =.. [m,S|_]
	,reverse([S|Ss],Ss_).


second_order_variables(Ss,Sub,SOs):-
	Sub =.. [m,_Id|Vs]
	,second_order_variables(Ss,Vs,[],SOs).

second_order_variables((_L,Ls),[V|Vs],Acc,Bind):-
	!
	,second_order_variables(Ls,Vs,[V|Acc],Bind).
second_order_variables((_L),[V|_Vs],Acc,Ss):-
	reverse([V|Acc],Ss).



%!	tautological_metarule(+Metarule) is det.
%
%	True when Metarule expands to a tautological metarule.
%
%	Metarule is an expanded metarule, Sub-M, where Sub is a
%	metasubstitution atom and M are the literals of the metarule.
%
%	This predicate is true when, if M = (H,B), then H:-B is a
%	autology.
%
%	This is a test employed by transform_metasubs/3 to ensure that
%	any tautological metarules derived during learning from
%	generalised metarules (or, hey, specialised ones) are not
%	returned.
%
%	In short, this is a filter for tautologies. The reason this is
%	needed is that when a tautological metarule is passed to
%	metasubstitutions/4 (by specialise_meta/4) evaluation does not
%	"go infinite". To be honest, I have no idea why that happens
%	when it happens. Normally metasubstitutions/4 can handle
%	tautologies just fine and they shouldn't be a reason to go
%	infinite, but something fishy's going on when we pass them as
%	learned metarules. Until I can find what, this is a hack that
%	will hopefully not calcify and become a feature.
%
tautological_metarule(Sub-M):-
	debug(tautology_filtering,'Checking for tautologies!',[])
	,M = (H,B)
	,debug_metarules(tautology_filtering,[Sub:-M])
	,debug_clauses(tautology_filtering,[H:-B])
	,tautology(H:-B)
	,debug(tautology_filtering,'Passed!',[]).



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
