:-module(greedy, [learn_greedy/1
		 ,learn_greedy/2
		 ,learn_greedy/5
		 ]).

:-use_module(project_root(configuration)).
:-use_module(src(vanilla)).
:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).

/** <module> New version of greedy Top Program Construction.

This module defines learning predicates for "greedy" Top Program
Construction (TPC) that combines ordinary TPC with an example coverage
algorithm.

The greedy version of TPC implemnted in this module proceeds as follows:

a) Set the hypothesis H to empy, []
b) Take the next positive example Ep from Pos
c) Learn: construct a hypothesis Hi from Ep, by TPC.
d) Update: Set H to H Union Hi
e) Reduce: discard each positive example entailed by H.
f) Repeat: start over from (a)
g) Return H when Pos = [].

Unlike the standard TPC, greedy TPC is non-deterministic and returns
hypotheses that are sub-sets of the Top Program, rather than a unique
set of clauses. On backtracking, each sub-set of the Top Program is
returned such that each returned program is a correct hypothesis (that
entails all the positive and none of the negative examples) but without
guaranteeing uniqueness.

Examples of use:
==
% Hypothesis learned from data/examples/anbn.pl with orindary TPC:

?- _T = s/2, time(learn(_T,_Ps)), print_clauses(_Ps), length(_Ps,N).
% 407,728 inferences, 0.062 CPU in 0.071 seconds (88% CPU, 6523648 Lips)
'$1'(A,B):-a(A,C),a(C,B).
'$1'(A,B):-a(A,C),s(C,B).
'$1'(A,B):-b(A,C),b(C,B).
'$1'(A,B):-s(A,C),b(C,B).
s(A,B):-'$1'(A,C),'$1'(C,B).
s(A,B):-'$1'(A,C),b(C,B).
s(A,B):-a(A,C),'$1'(C,B).
s(A,B):-a(A,C),b(C,B).
N = 8.

% Sub-hypotheses learned from the same experiment file by greedy-TPC:

?- _T = s/2, time(learn_greedy(_T,_Ps)), print_clauses(_Ps), length(_Ps,N).
% 235,886 inferences, 0.016 CPU in 0.028 seconds (55% CPU, 15096704 Lips)
'$1'(A,B):-s(A,C),b(C,B).
s(A,B):-a(A,C),'$1'(C,B).
s(A,B):-a(A,C),b(C,B).
N = 3 ;
% 10,066 inferences, 0.000 CPU in 0.002 seconds (0% CPU, Infinite Lips)
'$1'(A,B):-a(A,C),s(C,B).
s(A,B):-'$1'(A,C),b(C,B).
s(A,B):-a(A,C),b(C,B).
N = 3 ;
% 78,098 inferences, 0.016 CPU in 0.011 seconds (138% CPU, 4998272 Lips)
'$1'(A,B):-a(A,C),a(C,B).
'$1'(A,B):-b(A,C),b(C,B).
s(A,B):-'$1'(A,C),'$1'(C,B).
N = 3 ;
% 11,910 inferences, 0.000 CPU in 0.015 seconds (0% CPU, Infinite Lips)
[]
N = 0.
==

*/

%!	learn_greedy(+Targets) is nondet.
%
%	Learn sbusets of a Top Program greedily from a list of Targets.
%
%	Greedy Top Program Construction version of learn/1.
%
%	Targets is a list of predicate indicators, Symbol/Arity, of a
%	set of target predicates. Each target in Targets must have at
%	least one example defined in the current experiment file.
%
%	The learned Top Program is printed to the top-level. More
%
learn_greedy(Ts):-
	learn_greedy(Ts,Ps)
	,print_clauses(Ps).



%!	learn_greedy(+Targets,+Program) is det.
%
%	Learn subsets of a Top Program greedily from a list of Targets.
%
%	Greedy Top Program Construction version of learn/2.
%
%	The elements of the MIL problem are taken from the current
%	experiment file, via tp_safe_experiment_data/5.
%
%	Targets is a list of predicate indicators, Symbol/Arity, of a
%	set of target predicates. Each target in Targets must have at
%	least one example defined in the current experiment file.
%
learn_greedy(Ts,_Ps):-
	(   \+ ground(Ts)
	->  throw('learn_greedy/2: non-ground target symbol!')
	;   fail
	).
learn_greedy(Ts,Ps):-
	tp_safe_experiment_data(Ts,Pos,Neg,BK,MS)
	,learn_greedy(Pos,Neg,BK,MS,Ps).



%!	learn_greedy(+Pos,+Neg,+BK,+Metarules,-Progam) is det.
%
%	Learn subsets of a Top Progam from elements of a MIL problem.
%
%	Pos is a list of ground unit clauses, the positive examples.
%
%	Neg is a list of ground Horn goals, the negative examples.
%
%	BK is a list of predicate indicators, Symbol/Arity, of the
%	predicates defined in a background knowledge theory that must be
%	accessible to this predicate.
%
%	Metarules is a list of atomic identifiers of metarules to be
%	used in learning.
%
%	Program is a list of clauses, a sub-set of the Top Program
%	derivable from the given elements of the MIL problem. On
%	backtracking, each sub-set of the Top Progra is returned, such
%	that each sub-set is a correct hypothesis and each sub-set of
%	the Top Program that is a correct hypothesis is returned
%	eventually.
%
%	This predicate learns subsets of the Top Program greedily, by
%	the procedure outlined at the start of this module. In
%	particular, subsets of the Top Program are constructed
%	individually, without first constructing the entire Top Program.
%
%	The trade-off is that the greedy version of Top Program
%	Construction implemented in this predicate is faster, since it
%	doesn't need to process each positive example (at least not
%	until the worst case); but the order in which sub-sets of the
%	Top Program are constructed, and returned, depends on the
%	ordering of examples and the background knowledge and metarules,
%	and the thus may be over-specific by overfitting to the first
%	few examples processed.
%
%	Note also that, unlike learn/1, this predicate does not call
%	Plotkin's program reduction to eliminate redundant clauses from
%	the output. Such redundant clauses are eliminated automatically
%	by the greedy learning procedure.
%
learn_greedy([],_Neg,_BK,_MS,_Ts):-
	throw('learn_greedy/5: No positive examples found. Cannot train.').
learn_greedy(Pos,Neg,BK,MS,_Ts):-
	(   var(Pos)
	->  throw('learn_greedy/5: unbound positive examples list!')
	;   var(Neg)
	->  throw('learn_greedy/5: unbound negative examples list!')
	;   var(BK)
	->  throw('learn_greedy/5: unbound background symbols list!')
	;   var(MS)
	->  throw('learn_greedy/5: unbound metarule IDs list!')
	;   fail
	).
learn_greedy(Pos,Neg,BK,MS,Ps):-
	configuration:unfold_invented(U)
	,configuration:fold_recursive(F)
	,debug(learn,'Encapsulating problem',[])
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,debug(learn,'Constructing Top Program greedily...',[])
	,greedy_top_program(Pos_,Neg_,BK_,MS_,Ms)
	,examples_targets(Pos,Ss)
	,debug(learn,'Excapsulating hypothesis',[])
	,excapsulated_clauses(Ss,Ms,Ps_1)
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


%!	greedy_top_program(+Pos,+Neg,+BK,+MS,-Program) is nondet.
%
%       Construct a Top Program greedily from elements of a MIL problem.
%
greedy_top_program(_Pos,_Neg,_BK,_MS,_Ts):-
	configuration:theorem_prover(tp)
	,throw('theorem_prover(tp) cannot yet be used with greedy TPC.').
greedy_top_program(Pos,Neg,BK,MS,Ts):-
	configuration:theorem_prover(resolution)
        ,configuration:clause_limit(K)
	,(   K > 1
	 ->  Ps = [BK]
	 ;   Ps = [Pos,BK]
	 )
	,S = (write_problem(user,Ps,Refs)
	     ,refresh_tables(untable)
	     ,refresh_tables(table)
	     )
	,G = (greedy_top_program(Pos,Neg,MS,K,[],Es,[],Subs)
	     ,applied_metarules(Subs,MS,Ts_)
	     ,append(Es,Ts_,Ts)
	     )
	,C = (erase_program_clauses(Refs)
	     ,refresh_tables(untable)
	     )
	,setup_call_cleanup(S,G,C)
	,Ts \= [].
greedy_top_program(_Pos,_Neg,_BK,_MS,[]):-
% If Top program construction fails return an empty program.
	debug(greedy_top_program,'INSUFFICIENT DATA FOR MEANINGFUL ANSWER',[]).

%!	greedy_top_program(+Ps,+Ns,+Bs,+Ms,+K,+Res,+Acc,-Subs) is
%!	nondet.
%
%	Business end of greedy_top_program/5.
%
%	Ps, Ns Bs, Ms are the positive and negative examples, the
%	background knowledge and the metarules, respectively.
%
%	K is the value of the configuration option clause_limit/1,
%	passed in as an argument to avoid unnecessarily querying the
%	option all the time through various loops.
%
%	Res is the atomic residue left behind after learning: a set of
%	positive examples not covered by the returned hypothesis.
%
%	Acc is an accumulator of metasubstitutions and Subs is the list
%	of metasubstitutions returned: a list of Sub-Metarule pairs,
%	where each Sub is a ground metasubstitution atom and each
%	Metarule is an expanded metarule.
%
%	@bug This doesn't correctly collect uncovered examples.
%
greedy_top_program([],_Neg,_MS,_K,Es,Es,Subs,Subs):-
	debug(top_program,'Finished greedy learning.',[])
	,!.
greedy_top_program([Ep|Pos],Neg,MS,K,Acc_Es,Bind_Es,Acc_Ts,Bind_Ts):-
% Kind of a red cut. Stops backtracking over reduced_examples/5
% so we don't re-process sub-sets of examples already seen.
        !
	,debug(top_program,'Generalising example ~w...',[Ep])
	,generalise_greedy(Acc_Ts,Ep,K,MS,Acc_Ts_)
	,debug_clauses(top_program,'Hypothesis so-far:',Acc_Ts_)
	,debug(top_program,'Specialising learned hypothesis...',[])
        ,specialise_greedy(Acc_Ts_,Neg,K,MS,Acc_Ts_s)
	,debug_clauses(top_program,'Specialised hypothesis:',Acc_Ts_s)
	,length(Acc_Ts_s,M)
	,debug(top_program,'Cardinality of hypothesis so-far: ~w',[M])
	,debug(top_program,'Reducing examples...',[])
        ,reduced_examples(Pos,MS,K,Acc_Ts_s,Pos_)
	,length(Pos_,N)
	,debug(top_program,'Positive examples remaining: ~w',[N])
	,greedy_top_program(Pos_,Neg,MS,K,Acc_Es,Bind_Es,Acc_Ts_s,Bind_Ts).
greedy_top_program([Ep|Pos],Neg,MS,K,Acc_Es,Bind_Es,Acc_Ts,Bind_Ts):-
% Couldn't generalise _Ep (or not any more). Drop it.
	debug(top_program,'Could not further generalise example ~w...',[Ep])
	,greedy_top_program(Pos,Neg,MS,K,[Ep|Acc_Es],Bind_Es,Acc_Ts,Bind_Ts).



%!	generalise_greedy(+Acc,+Example,+K,+MS,-Acc_New) is det.
%
%	Generalise step of greedy Top Program Construction.
%
%	Clauses are selected according to K, the value of the
%	configuration option clause_limit/1.
%
generalise_greedy(Acc,Ep,K,MS,Acc_s):-
	K == 0
	,!
	,member(M,MS)
	,louise:metasubstitution(Ep,M,Sub)
	,constraints(Sub)
	,applied_metarules([Sub-M],MS,[C])
	,\+ tautology(C)
	,sort(1,@<,[Sub-M|Acc],Acc_s).
generalise_greedy(Acc,Ep,K,MS,Subs_s):-
	K > 0
	,louise:metasubstitutions(Ep,K,MS,Subs)
	,forall(member(Sub-M,Subs)
	       ,(check_constraints([Sub])
		,applied_metarules([Sub-M],MS,[C])
		,\+ tautology(C)
		)
	       )
	,flatten([Subs|Acc],Subs_f)
	,sort(1,@<,Subs_f,Subs_s).


%!	specialise_greedy(+Generalised,+Neg,+K,+MS,-Specialised) is det.
%
%	Specialise step for greedy Top Program Construction.
%
%	Clauses are selected according to K, the value of the
%	configuration option clause_limit/1.
%
specialise_greedy(Subs,Neg,K,_MS,Subs_s):-
	K == 0
	,!
	,specialise(Subs,Neg,Subs_s).
specialise_greedy(Subs,Neg,K,MS,Subs_s):-
% specialise/4 expects a list-of-lists as the first argument!
% We shall oblige to avoid bracketfucking.
        K > 0
	,specialise([Subs],MS,Neg,Subs_s).


%!	specialise_greedy(+Generalised,+MS,+Neg,-Specialised) is det.
%
%	Specialise step for clause_limit(K) where K > 0.
%
%	Copy of louise:specialise/4, slightly modified to return a list
%	of metasubstitutions rather than a list of lists of
%	metasubstitutions, to make it easier to use this predicate in a
%	greedy Top Program Construction loop.
%
specialise([Ss_Pos],_MS,[],Ss_Pos):-
	!.
specialise(Ss_Pos,MS,Neg,Ss_Neg):-
	configuration:clause_limit(K)
	,K > 0
	,findall(Sub
	       ,(member(Subs,Ss_Pos)
		,findall(Sub
			,member(Sub-_M,Subs)
			,Subs_)
		,debug_clauses(metasubstitutions,'Ground metasubstitutions:',[Subs_])
		,\+((member(En,Neg)
		    ,debug_clauses(specialise,'Negative example:',En)
		    ,once(louise:metasubstitutions(En,K,MS,Subs_))
		    ,debug_clauses(specialise,'Proved negative example:',En)
		    )
		   )
		,member(Sub,Subs)
		)
	       ,Ss_Neg).



%!	reduced_examples(+Pos,+MS,+K,+Subs,-Reduced) is det.
%
%	Eliminate positive examples entailed by a program.
%
%	Pos and MS are the positive examples and metarules for the
%	current MIL problem.
%
%	K is the value of the configuration option clause_limit/1.
%
%	Subs is the list of metasubstitutions derived so-far from
%	positive examples, and specialised by negative examples. Subs is
%	in the form returned by specialise_greedy/5, a list of
%	Sub-Metarule pairs.
%
%	Reduced is the set of examples in Pos, minus each example in
%	Pos that is entailed by the metasubstitutions in Subs (or
%	rather the clauses into which they are expanded).
%
reduced_examples(Pos,MS,K,Subs,Pos_r):-
	length(Pos,N)
	,debug(reduced_examples,'Reducing ~w positive example(s)...',[N])
	,reduced_examples(Pos,MS,K,Subs,[],Pos_r)
	,length(Pos_r,M)
	,debug(reduced_examples,'Positive examples remaining: ~w',[M]).

%!	reduced_examples(+Pos,+MS,+K,+Subs,+Acc,-Reduced) is det.
%
%	Business end of reduced_examples/5.
%
%	Acc is the accumulator of remaining examples in Pos that cannot
%	be eliminated by resolution with Subs.
%
%	Note that this predicate proves an example is entailed by Subs
%	(with respect to the background knowledge in the Prolog
%	database) by first unifying the example to the head of a
%	clause expanded from a metasubstitution in Subs and then
%	refuting the body literals of that clause (using the Vanilla
%	meta-interpreter). This is to avoid examples being proved by
%	dint of being included in the background knowledge-base, as
%	happens when clause_limit/1 is set to 0.
%
reduced_examples([],_MS,_K,_Subs,Acc,Pos_):-
	reverse(Acc,Pos_)
	,!.
reduced_examples([Ep|Pos],MS,K,Subs,Acc,Bind):-
	debug_clauses(reduced_examples,'Testing example:',[Ep])
	,example_body(Ep,K,Subs,MS,Bs)
	,debug_clauses(reduced_examples,'Refuting body goals:',[Bs])
	,(   K == 0
	 ->  K_ = 1
	 ;   K_ = K
	 )
	,prove(Bs,K_,MS,[],Subs,Subs)
	,debug(reduced_examples,'Succeeded!',[])
	,!
	,reduced_examples(Pos,MS,0,Subs,Acc,Bind).
reduced_examples([Ep|Pos],MS,K,Subs,Acc,Bind):-
	debug(reduced_examples,'Failed!',[])
	,reduced_examples(Pos,MS,K,Subs,[Ep|Acc],Bind).


%!	example_body(+Example,+K,+Subs,+MS,-Body) is nondet.
%
%	Match an example to the head of a clause and return its Body.
%
%	Clauses are selected according to the value of K.
%
%	If K = 0, the first metasubstitution in Subs is selected to be
%	expanded into a clause, the head of which is matched to the
%	given Example.
%
%	If K = 1, each metasubstitution in Subs is tried in turn until
%	one is found that is proved by Vanilla, in reduced_examples/6.
%
example_body(Ep,0,[Sub-M|_Subs],MS,Bs):-
% Sub is the latest-derived metasubstitution
	applied_metarules([Sub-M],MS,[C])
	,clause_head_body(C,Ep,Bs).
example_body(Ep,K,Subs,MS,Bs):-
	K > 0
	,member(Sub-M,Subs)
	,applied_metarules([Sub-M],MS,[C])
	,clause_head_body(C,Ep,Bs).


%!	clause_head_body(+Clause,-Head,-Body) is det.
%
%	Split a Clause into its Head and Body.
%
clause_head_body(Ep:-Bs,Ep,Bs):-
% Clause with head and body.
	!.
clause_head_body(C,_Ep,true):-
% Clause with no body. Quelle horreure!
	functor(C,m,_A).
