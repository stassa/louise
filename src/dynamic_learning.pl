:-module(dynamic_learning, [learn_dynamic/1
			   ,learn_dynamic/2
			   ,learn_dynamic/5
			   ]).

:-use_module(configuration).
:-use_module(src(louise)).

/** <module> Dynamic learning with metarule extension for predicate invention.

*/


%!	learn_dynamic(+Target) is det.
%
%	Perform dynamic learning for a learning Target.
%
learn_dynamic(T):-
	learn_dynamic(T,Ps)
	,print_clauses(Ps).



%!	learn_dynamic(+Target,-Program) is det.
%
%	Learn a Program by dynamic learning.
%
learn_dynamic(T,Ps):-
	louise:tp_safe_experiment_data(T,Pos,Neg,BK,MS)
	,learn_dynamic(Pos,Neg,BK,MS,Ps).



%!	learn_dynamic(+Pos,+Neg,+BK,+MS,-Prog) is det.
%
%	Learn with metarule extension for predicate invention.
%
%	Implements dynamic lerning (provisional title), a method for
%	predicate invention by metarule extension.
%
%	Dynamic Learning
%	================
%
%	Below, two clauses C1 and C2 are connected when a body literal
%	of C1 unifies with the head literal of C2.
%
%	Additionally, an "extension pair" below signifies two metarules
%	M1 and M2, where M1 is extended by M2 with a call to extend/3,
%	for example extend(M1,M2,M3).
%
%	Dynamic learning proceeds in learning episodes similar to
%	episodic learning. As in episodic learning, the hypothesis
%	learned in each episode is added to the background knowledge for
%	subsequent episodes.
%
%	In addition, in each episode of dynamic learning the metarules
%	in the original MIL problem are extended and used to generalise
%	the positive examples. If any examples are generalised, the
%	metasubstitution atoms of the _original_ metarules in an
%	extension pair are added to the generalised Top program. During
%	generalisation the bindings of the existentially quantified
%	variables in the extended metarule are preserved so the two
%	metasubstitutions added to the top program, applied to their
%	corresponding initial metarules, result in two connected
%	clauses.
%
%	However, note that the set of existentially quantified variables
%	in the metarules in the extension pair are does _not_ include
%	the predicate symbol variable of the head literal of the second
%	metarule in the pair, i.e. the literal "called" from the first
%	metarule in the pair. This variable is therefore bound to an
%	invented symbol, given the name of the target predicate with a
%	serial number appended.
%
%	The result of all this complicated dance is predicate invention.
%
%	Note that the actualy extended metarules, or their
%	metasubstitution atoms, are not used anywhere other than in the
%	attempts to use those metarules to generalise each positive
%	example.
%
%	Note also that only the originally given metarules are ever
%	extended- extended metarules themselves are never extended. This
%	means that the number of extension steps taken in each episode
%	is a _constant_ function of the number of originally given
%	metarules.
%
%	Examples
%	========
%
%	The following examples should help clarify the function of
%	metarule extension and predicate invention.
%
%	1. Metarule extension
%
%	In the first example we show (roughly) the process of extending
%	a metarule by another (in this case itself), binding the
%	existentially quantified variables in the extension and
%	propagating the bindings to the original metarules.
%	Additionally, we bind an invented symbol to the
%	existentially quantified second-order variable of the second
%	metarule in the extension pair:
%
%	==
%	?- expanded_metarules([chain], [_C1])   % (1)
%	,copy_term(_C1, _C2)                    % (2)
%	,dynamic_learning:extend(_C1, _C2, _M)  % (3)
%	,_C1 = (m(chain,P1,Q1,R1):-_)           % (4)
%	,_C2 = (m(chain,p_1,Q2,R2):-_)          % (5)
%	,_M = (m(Nm,p,q,r,s):-_)                % (6)
%	, print_clauses([_C1,_C2,_M]).          % (7)
%
%	m(chain,p,p_1,s):-m(p,A,B),m(p_1,A,C),m(s,C,B).  % (8)
%	m(chain,p_1,q,r):-m(p_1,A,B),m(q,A,C),m(r,C,B).  % (9)
%	m(chain_chain7,p,q,r,s):-m(p,A,B),m(q,A,C),m(r,C,D),m(s,D,B). %(10)
%
%	P1 = p,               % (11)
%	Q1 = p_1,	      % (12)
%	R1 = s,               % (13)
%	Q2 = q,               % (14)
%	R2 = r,               % (15)
%	Nm = chain_chain7 .   % (16)
%	==
%
%	In the example above, first (line 1) expanded_metarules/2 is
%	used to expand the chain metarule, then (line 2) copy_term/2 is
%	used to make a copy of the expansion with fresh variables
%	(remember that this is _expansion_ not _extension; not yet).
%
%	On line 3 the two copies are extended by a call to extend/3.
%
%	On lines 4-6 the existentially quantified variables in the three
%	metarules (the two copies of chain in the original extension
%	pair and their extension) are bound to the predicate symbols p,
%	q, r, s and p_1. These are made up of course; in an actual
%	learning attempt, p, q, r and s would come from the background
%	knowledge. p_1 is the _invented_ symbol and would have a symbol
%	composed of the learning target's predicate symbol and a
%	suitable serial number.
%
%	On line 7, the three metarules are printed out with a call to
%	print_clauses/2. Lines 8-9 are the output of print_clauses/2.
%
%	On lines 8 and 9 are listed the two copies of chain in the
%	original extension pair. The metasubstitution atoms of those two
%	copies have their existentially quantified variables bound
%	according to their grounding on line 6, where the "background
%	predicate symbols" are bound, and on line 5, where the "invented
%	predicate symbol" is bound. Scare quotes remind you that those
%	predicate symbols are fictional and chosen by hand for the needs
%	of this example.
%
%	Lines 11 to 16 list the bindings of the existentially quantified
%	variables in the three metarules (and also the Nm variable,
%	taking the value of the name of the extended metarule).
%
%	Note that, above, we stopped backtracking before any more
%	results could be retrieved. There is an additional result,
%	because of the nondeterminism of extend/3. We list this
%	additional result below for completeness. The difference with
%	the above listed result is that the predicate variable of a
%	different body literal of the first metarule is bound to the
%	invented predicate symbol. In a real learning attempt, not every
%	result of extend/2 would be possible, and which results
%	are actually possible would depend on the MIL problem (i.e.
%	what predicate symbols can be bound to the second order
%	existentially quantified variables in each extension, if any).
%
%	==
%	m(chain,p,q,p_1):-m(p,A,B),m(q,A,C),m(p_1,C,B).
%	m(chain,p_1,r,s):-m(p_1,A,B),m(r,A,C),m(s,C,B).
%	m(chain_chain15,p,q,r,s):-m(p,A,B),m(q,A,C),m(r,C,D),m(s,D,B).
%	P1 = p,
%	Q1 = q,
%	R1 = p_1,
%	Q2 = r,
%	R2 = s,
%	Nm = chain_chain15
%	==
%
%	2. Predicate invention
%
%	In the second example we show the result of applying predicate
%	invention to the problem of learning a grammar for the a^nb^n
%	language (the experiment file for this example can be found in
%	data/examples/grammar/anbn.pl):
%
%	==
%	?- learn_dynamic('S'/2).% Encapsulating problem
%	% First dynamic episode
%	% Constructing Top program
%	% Reducing Top program
%	% New dynamic episode
%	% Constructing Top program
%	% Reducing Top program
%	% Length: 8
%	% New dynamic episode
%	% Constructing Top program
%	% Reducing Top program
%	% Length: 8
%	'S'(A,B):-'A'(A,C),'B'(C,B).
%	'S'(A,B):-'A'(A,C),'S_2'(C,B).
%	'S'(A,B):-'S_1'(A,C),'B'(C,B).
%	'S_1'(A,B):-'A'(A,C),'S'(C,B).
%	'S_2'(A,B):-'S'(A,C),'B'(C,B).
%	true.
%	==
%
%	Above, two sub-hypotheses of the anbn grammar are found, each
%	with a different invented predicate, called by a different body
%	literal of 'S'/2.
%
%	For clarity, the two sub-hypotheses, separated by hand, are as
%	follows:
%	==
%	% First hypothesis, with invented definition of 'S_1'/2:
%	'S'(A,B):-'A'(A,C),'B'(C,B).
%	'S'(A,B):-'S_1'(A,C),'B'(C,B).
%	'S_1'(A,B):-'A'(A,C),'S'(C,B).
%
%	% Second hypothesis, with invented definition of 'S_2'/2:
%	'S'(A,B):-'A'(A,C),'B'(C,B).
%	'S'(A,B):-'A'(A,C),'S_2'(C,B).
%	'S_2'(A,B):-'S'(A,C),'B'(C,B).
%	==
%
%	Note that in each definition the corresponding invented
%	predicate's symbol is "called" from a different body literal of
%	the second 'S'/2 clause. This is the result of nondeterminism in
%	extend/3. In this case, each extension of the original set of
%	metarules (i.e. only chain, in the anbn.pl experiment file) is a
%	clause that belongs to a correct hypothesis. Therefore, both are
%	kept.
%
%	@tbd Metarule extension is currently performed by the predicate
%	extend/3 defined in this file and _not_ with
%	extended_metarules/3. This is a bit of a premature optimisation
%	hack, to avoid the more expensive operation in
%	extended_metarules/3. In the future, the two extension versions
%	will be merged to reduce confusion.
%
learn_dynamic(Pos,Neg,BK,MS,Ps):-
	C = c(0)
	,examples_target(Pos,T)
	,debug(dynamic,'Encapsulating problem',[])
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,debug(dynamic,'First dynamic episode',[])
	,dynamic_episode(C,Pos_,Neg_,BK_,MS_,Es_1)
	%,debug_clauses(dynamic, Es_1)
	,debug(dynamic,'Reducing Top program',[])
	,reduced_top_program(Pos_,BK_,MS_,Es_1,Rs_1)
	,atomic_residue(Rs_1,Pos_,Rs)
	,length(Rs,N)
	,debug(dynamic,'Reduced examples: ~w', [N])
	,learn_dynamic(C,T,N,Pos_,Neg_,BK_,MS_,Es_1,Ps_k)
	,excapsulated_clauses(T,Ps_k,Ps).

%!	learn_dynamic(+Counter,+Tgt,+Size,+Pos,+Neg,+BK,_MS,-Episode,-Prog)
%!	is det.
%
%	Business end of learn_dynamic/5.
%
%	This predicate implements the dynamic learning loop described in
%	learn_dynamic/5.
%
%	Counter is a Prolog term C(K), updated dynamically to keep track
%	of the number of predicates invented during a dynamic learning
%	attempt. This is a bit of a hack- it's meant to stop
%	dynamic_episode/6 from producing an infinite set of hypotheses
%	differing only in the name of their invented predicates. A
%	better way to do this is to test whether a new hypothesis with
%	invented predicates is already in the background knowledge- but
%	that may be a bit expensive.
%
%	Size is the cardinality of the hypothesis in the previous
%	dynamic learning step. If Size does not increase between steps,
%	learn_dynamic/9 exits with the current theory bound in Prog.
%
learn_dynamic(C,T/A,N,Pos,Neg,BK,MS,Es_i,Bind):-
	append(BK,Es_i,BK_)
	,debug(dynamic,'New dynamic episode',[])
	,dynamic_episode(C,Pos,Neg,BK_,MS,Es_j)
	%,debug_clauses(dynamic, Es_j)
	,debug(dynamic,'Reducing Top program',[])
	,reduced_top_program(Pos,BK_,MS,Es_j,Rs_j)
	,atomic_residue(Rs_j,Pos,Rs)
	,length(Rs,M)
	,debug(dynamic,'Reduced examples: ~w', [M])
	,M < N
	,!
	,learn_dynamic(C,T/A,M,Pos,Neg,BK_,MS,Es_j,Bind).
learn_dynamic(_C,_T,_M,Pos,_Neg,BK,MS,Es_i,Ps):-
	reduced_top_program(Pos,BK,MS,Es_i,Ps).


%!	dynamic_episode(+Counter,+Pos,+Neg,+BK,+MS,-Episode) is det.
%
%	Complete one dynamic learning episode.
%
%	Thin ish wrapper around top_program_dynamic/6 to allow for some
%	control of recursion, via louise:recursion_guard/3.
%
dynamic_episode(C,Pos,Neg,BK,MS,Ms):-
	configuration:theorem_prover(TP)
	,configuration:recursion_depth_limit(episodic_learning,L)
	,debug(dynamic,'Constructing Top program',[])
	,G = dynamic_learning:top_program_dynamic(C,Pos,Neg,BK,MS,Ms)
	,louise:recursion_guard(G,L,TP).


%!	atomic_residue(+Program,+Positive,-Residue) is det.
%
%	Residue is the intersection of Program and Positive examples.
%
%	Program is the reduction of the BK, Positive (examples) and
%	learned hypothesis. Residue is a list of examples in
%	Positive that remain in Program after it is reduced.
%
%	This is used to determine whether dynamic learning should
%	continue. Dynamic learning should stop if the number of
%	unreduced examples does not change between learning attempts,
%	because this indicates that no new clauses have been added to
%	the Top program.
%
%	Note that the last part is a bit of a conjecture. There is
%	always the possibility that adding one or more new clauses to
%	the Top program doesn't change the program's success set.
%
%	More worryingly it's possible that adding one or more new
%	clauses to the Top program will make it harder to reduce the MIL
%	problem successfully.
%
atomic_residue(Ps,Pos,Is):-
	sort(Ps,Ps_)
	,ord_intersect(Ps_, Pos, Is).


%!	top_program_dynamic(+Counter,+Pos,+Neg,+BK,+MS,-Top) is det.
%
%	Construct a Top program dynamically.
%
%	This is a dynamic version of Top program construction. It is
%	identical to the ordinary Top program construction implemented
%	in louise:top_program/5, except that an additional
%	generalisation step is taken where metarules are extended and
%	the metasubstitution atoms of the original metarules in an
%	extension pair are added to the Top program, as described in
%	learn_dynamic/5.
%
top_program_dynamic(C,Pos,Neg,BK,MS,Ts):-
	configuration:theorem_prover(resolution)
	,!
	,louise:write_program(Pos,BK,MS,Refs)
	,generalise_dynamic(C,Pos,MS,Ms_Pos)
	,louise:specialise(Ms_Pos,Neg,Ms)
	%,constraints(Ms, Ms_)
	,unfolded_metasubs(Ms,Ts)
	,erase_program_clauses(Refs).


%!	generalise_dynamic(+Counter,+Pos,+MS,-Metasubs) is det.
%
%	Generalisation step of dynamic Top program construction.
%
generalise_dynamic(C,Pos,MS,Ss_Pos):-
	(   louise:generalise(Pos,MS,Ss_1)
	->  true
	;   Ss_1 = []
	)
	,generalise_invent(C,Pos,MS,Ss_2)
	,append(Ss_1,Ss_2,Ss_Pos).


%!	generalise_invent(+Counter,+Pos,+MS,-Metasubs) is det.
%
%	Generalisation step of dynamic Top program construction.
%
%	This is the second generalisation step that actually adds the
%	metasubstitution atoms of the original metarules in an extension
%	pair to the Top program, resulting in predicate invention.
%
generalise_invent(C,Pos,MS,Ss_Pos):-
	examples_target(Pos,T/_)
	,setof(S
	     ,M1^MS^M2^M3^Ep^Pos^H^(metarule_extension(MS,M3,M1,M2)
				   ,member(Ep,Pos)
				   ,louise:metasubstitution(Ep,M3,H)
				   %,debug(dynamic,'Extended:',[])
				   %,debug_clauses(dynamic,H)
				   ,metasub_atom(C,T,M1,M2,S)
				   %,debug(dynamic,'Invented:',[])
				   %,debug_clauses(dynamic,S)
				   )
	     ,Ss_Pos)
	%,debug(dynamic,'Generalised:',[])
	%,debug_clauses(dynamic,Ss_Pos)
	,!.
% Extended metarules may fail to generalise any examples.
% In that case, generalise_invent/4 should not fail and take down
% generalise_dynamic/4 with it. Note however that a catch-all clause
% like this is a rather dangerous way to achieve a graceful exit.
% Perhaps change this so the call to setof/3 doesn't fail even when
% metasubstitution/3 never succeeds?
generalise_invent(_C,_Pos,_MS,[]).


%!	metarule_extension(+Metarules,+Extension,+Original1,+Original2)
%!	is det.
%
%	Extend a pair of metarules and return them and their Extension.
%
%	The original metarules, Original1 and Original2 are needed in
%	generalise_invent/4 because we want to collect their ground
%	metasubstitutions as invented clauses.
%
metarule_extension(MS,M3,M1,M2_):-
	member(M1,MS)
	,member(M2,MS)
	,(   same_metarule(M1,M2)
	->   copy_term(M2,M2_)
	    ,extend(M1,M2_,M3)
	 ;   M2_ = M2
	    ,extend(M1,M2_,M3)
	 ).


%!	metasub_atom(+Count,+Target,+Metarule1,+Metarule2,-Metasub) is
%!	nondet.
%
%	Construct a metasubstitution atom for an invented metarule.
%
%	Count is the current highest index of an invented predicate.
%	Target is the predicate symbol (name only- not arity) of the
%	learning target. Metarule1 and Metarule2 are the metarules in an
%	extension pair.
%
%	Metasub is successively bound to the metasubstitution atom of
%	each of the two metarules with a suitable predicate symbol bound
%	to the existentially quantified second order variable of each
%	metasubstiution atom where an invented symbol should be. The
%	"suitable" predicate symbol is of the form: <Target>_<Count+1>.
%	The position of the existentially quantified second order
%	variable for the invented predicate symbol is known because when
%	this predicate is called, it should be the only existentially
%	quantified variable in each of the two metarules that remains
%	free. Remember that remaining predicate symbols are ground in
%	generalise_invent/4 when an extended metarule succeeds in
%	generalising a positive example.
%
metasub_atom(C,T,(M1:-_),(M2:-_),S):-
% There should only be one variable in both metasub atoms
% The variable of the invented predicate's symbol.
	term_variables([M1,M2],[V])
	,invented_symbol(T,C,V)
	% We use member/2's nondeterminism here to return
	% multiple results in setof/3 and avoid having to flatten
	% its set of results later.
	% Yeah, I know this is a bit cryptic, sorry.
	,member(S,[M1,M2]).


%!	invented_symbol(+Target,+Counter,?Symbol) is det.
%
%	Create an invented predicate Symbol.
%
%	Symbol is of the form <Target>_<Count+1>.
%
invented_symbol(T,C,V):-
	arg(1,C,I)
	,succ(I,I_)
	,configuration:max_invented(K)
	,I_ =< K
	,atomic_list_concat([T,I_],'_',V)
	,nb_setarg(1,C,I_).


/*================================================================================
 * Metarule extension
 ================================================================================ */


%!	extend(+Metarule_1,+Metarule_2,-Extension) is det.
%
%	Extend a pair of metarules.
%
extend(H1:-M1,H2:-M2,H3:-M3):-
	mil_problem:unfold(M1,M2,M3)
	,existential(H1,H2,M3,Es)
	,rename(H1,H2,Es,H3).


%!	existential(+Metasub_1,+Metasub_2,+Metarule,-Existential) is
%!	det.
%
%	Collect Existentially quantified variables in a metarule Body.
%
%	Metarule is the "vector" of head and body literals of an
%	extended metarule. Metasub_1 and Metasub_2 are the
%	metasubstitution atoms of the original metarules in the
%	extension pair that produced the extended metarule. Existential
%	is the set of existentially quantified variables in Body that
%	are also in Metasub_1 and Metasub_2. These need to be included
%	in the metasubstitution atom of the extension, i.e. the one
%	associated with Metarule.
%
existential(H1,H2,B,Es):-
	maplist(symbols,[H1,H2],[Ss1,Ss2])
	,term_variables([Ss1,Ss2],Vs)
	,term_variables(B,Bs)
	,existential_(Bs,Vs,[],Es).


%!	symbols(+Metarule, -Symbols) is det.
%
%	Extract predicate symbol variables from a Metarule.
%
symbols(M,Ps):-
	M =.. [m,_N|Ps].


%!	existential_(?Metarule,?Metasubs,+Acc,-Existential) is det.
%
%	Business end of existential/4.
%
%	Metarule is the list of existentially quantified variables in an
%	extended metarule, M1. Metasubs is the set of existentially
%	quantified variables in the metarules in the extension pair that
%	produced M1. Existential is the list of existentially quantified
%	variables in Metarule that are also in Metasubs. Those need to
%	be included in the metasubstitution atom associated with
%	Metarule.
%
existential_([],_,Acc,Es):-
	!
       ,reverse(Acc,Es).
existential_([V|Vs],Bs,Acc,Bind):-
	in_vars(V, Bs)
	,!
	,existential_(Vs,Bs,[V|Acc],Bind).
existential_([_V|Vs],Bs,Acc,Bind):-
	existential_(Vs,Bs,Acc,Bind).


%!	in_vars(?Variable,?Variables) is det.
%
%	True when a Variable is in a list of Variables.
%
%	Version of memeber/2 that avoids unfiying Variable with every
%	other variable in Variables and thereby making an awful
%	mish-mashed mess of unexpectedly identical variables. We need to
%	preserve Variables and their bindings throughout the project.
%
in_vars(V,[V1|_Vs]):-
	V == V1
	,!.
in_vars(V,[_|Vs]):-
	in_vars(V,Vs).


%!	rename(+Metasub_1, +Metasub_2,+Existential,-Metasub_3) is det.
%
%	Create a metasubstitution atom for an extended metarule.
%
%	Metasub_1 and Metasub_2 are the metasubstitution atoms of the
%	two metarules in an extension pair. Existential is the set of
%	existentially quantified variables in the metarule resulting
%	from this pair's extension, M3. Metasub_3 is the
%	metasubstitution atom of M3, including a unique name and the set
%	of Existential variables of the other two metarules that are
%	also found in the head and body literals of M3.
%
%	The unique name for Metasub_3 is currently created by appending
%	the metarule names in Metasub_1 and Metasub_2, separated by an
%	underscore, "_", and passed to gensym/2 that adds to it a unique
%	... ish... numeric index.
%
%	The use of gensym/2 is justified, despite its lack of guarantee
%	of actual uniqueness. Extensions are short lived and we don't
%	need to track their names throughout the process. Basically, the
%	new name in Metasub_3 is only useful for debugging purposes.
%
rename(H1,H2,Es,H3):-
	H1 =.. [m,N1|_]
	,H2 =.. [m,N2|_]
	,atomic_list_concat([N1,N2],'_',N_)
	,gensym(N_,N3)
	,H3 =.. [m,N3|Es].
