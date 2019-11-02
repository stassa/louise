:-module(dynamic_learning, [learn_dynamic/1
			   ,learn_dynamic/2
			   ,learn_dynamic/5
			   ]).

:-use_module(configuration).
:-use_module(src(auxiliaries)).
:-use_module(src(louise)).
:-use_module(src(mil_problem)).
:-use_module(src(metagen)).

/** <module> Dynamic learning with metarule extension and predicate invention.

Dynamic learning is Louise's strategy for bias shift by metarule
extension and predicate invention.

Dynamic Learning
================

Dynamic learning proceeds in a finite number of discrete _learning
episodes_. The hypothesis learned in each episode k is added to the
background knowledge of episode k+1. Hypotheses may include invented
predicates, constructed by a process of metarule extension, described
below.

The purpose of dynamic learning is to perform _bias shift_. Bias shift
is the automatic modification, or generation, of inductive bias. In the
context of machine learning, iductive bias is normally taken to mean
everything that contributes to learning, other than the learner itself
and the set of training examples. In the case of Louise and
Meta-Interpretive Learning (MIL) in general, inductive bias refers to
the background knowledge and metarules.

Louise's implementation of dynamic learning modifies both the metarules
and the background knowledge. More precisely, the modification of
metarules is done by _metarule extension_ and the modification of the
background knowledge by inclusion of previously learned hypotheses in
the background knowledge and by _predicate invention_.

Metarule extension
------------------

Metarule extension refers to a process of adding new body literals to
metarules by _unfolding_ between pairs of metarules taken from an
initially given set (provided by the user). Metarule extension involves
an ordered set of two metarules, Ext = {M1, M2}, that we will refer to
as the _extension pair_. The first metarule in an extension pair, M1, is
the _extended metarule_ and the second metarule, M2, is the _extending
metarule_. We will say that "M1 is extended by M2", or that M2 "extends
M1".

During metarule extension, first each body literal, L_i, of the extended
metarule, M1, is unified with the head literal of the extending
metarule, M2, then L_i is replaced by each body literal of the extending
metarule, M2. The result is a new metarule, M_i, the _extension of M1
with respect to L_i_, whereas L_i is the _extended literal_. M_i has n-1
more body literals than M1, where n is the number of body literals in
M2.

The following is an example of extending the chain metarule by a call to
Louise's extended_metarules/2 predicate:

==
?- extended_metarules([chain], _Es), print_clauses(_Es).
m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E).
m(chain_2,A,B,C,D):-m(A,E,F),m(B,E,G),m(C,G,H),m(D,H,F).
m(chain_2_2,A,B,C,D):-m(A,E,F),m(B,E,G),m(C,G,H),m(D,H,F).
true.
==

Note that the result of calling extended_metarules/2 also returns the
extended metarule itself, in this case, chain.

Also note that the two extending metarules above, named chain_2 and
chain_2_2, appear otherwise identical, however this is an artifact of
the pretty-printing by print_clauses/1. In reality, the two extensions
are identical _up to renaming of variables_ (and ignoring the
automatically assigned names). Each extension results from resolving
chain by itself on a different body literal each time. Thus, while the
structure of the two extensions is identical different variable
susbsitutions are possible, as shown in the section on predicate
inventionm, below.

The result of unconstrained metarule extension is a set of extensions
that grows exponentially in the number of metarules in the initial set.
What's worse, many of the metarule extensions resulting from the
unconstrainted process may have duplicate body literals and are
therefore redundant with respect to shorter extensions of the same
extension pair. For efficiency, we wish to constraint the process of
extension so that the number of resulting extensions remains polynomial
and is maximally relevant to the learning target. Dynamic learning
ensures that this is the case.

Predicate invention
-------------------

During a dynamic learning episode, Louise extends the metarules in the
initial set _once_, then the extensions and the initial metarules are
used to construct the Top program as normal. If a metarule extension is
successfully used to generalise an example, the metasubstitution atoms
of the _extension pair_ used to construct the extension are added to the
Top program. Note again: the extended metarule's metasubstitution atom
is _not_ added to the Top program; only the metasubstitution atoms of
the extension pair are.

The following example illustrates the process described above:

==
% [1] Extension pair consisting of two instances of chain.
M1 = m(chain, P1, Q1, R1):- m(P1, X, Y), m(Q1, X, Z), m(R1, Z, Y)
M2 = m(chain, P2, Q2, R2):- m(P2, U, W), m(Q2, U, V), m(R2, V, W)

% [2] Extension of M1 by M2.
M = m(chain_chain, P1, Q2, R2, R1):- m(P1, U, Y), m(Q2, U, V), m(R2, V, W), m(R1, W, Y)

% [3] Example variable substitutions.
% Zeta is applied to M1 and M2 during metarule extension.
% Eta is applied to M during Top program construction.
% Theta is applied to M1 and M2 during predicate invention.
Zeta = {Q1/P2, X/U, Z/W}
Eta = {P1/p, R1/s, Q2/q, R2/r}
Theta = {P2/p_1}

% [4] Result of applying Zeta to M1 and M2
m(chain, P1, P2, R1):-m(P1, U, Y), m(P2, U, W), m(R1, W, Y)
m(chain, P2, Q2, R2):-m(P2, U, W), m(Q2, U, V), m(R2, V, W)

% [5] Result of applying Eta to M.
m(chain_chain, p, q, r, s):- m(p, U, Y), m(q, U, V), m(r, V, W), m(s, W, Y)

% [6] Result of applying Theta to M1 and M2.
m(chain, p, p_1, s):- m(p, U, Y), m(p_1, U, W), m(s, W, Y)
m(chain, p_1, q, r):- m(p_1, U, W), m(q, U, V), m(r, V, W)

% [7] Excapsulation of M1 and M2 after variable substitutions.
C1 = p(U, Y):- p_1(U, W), s(W, Y)
C2 = p_1(U, W):- q(U, V), r(V, W)
==

Above, in [1], M1 and M2 are two instances of _chain_ with variables
standardised apart. M1 and M2 constitute an extension pair.

In [2], M1 is extended by M2, producing M, essentially a chain metarule
with one additional body literal.

In [3] we illustrate the variable substitutions found during metarule
extension, Top program construction and predicate invention, with
examples.

Zeta is a substitution of the variables in M1 and M2 found during the
extension of M1 by M2. Q1 and P2 are the existentially quantified,
second-order variables corresponding to the predicate symbols of the
extended literal of M1, m(Q1, X, Z), and the head literal of M2, m(P2,
U, W), respectively. X, U, Z and W are universally quantified,
first-order variables in body literals of M1 and M2 unified during
extension.

Eta is a _metasubstitution_, a substitution of the existentially
quantified, second-order variables, P1, R1, Q2 and R2 in M by the
predicate symbols, p, q, r and s, representing symbols of predicates in
the examples and the background knowledge. p represents the predicate
symbol of the learning target whreas q, r and s represent predicate
symbols selected by successful resolution of M with the background
knowledge during the generalisation step of Top program construction.

Theta is a metasubstitution of the existentially quantified,
second-order variable P2 by an _invented_ symbol.

In [4] we list the result of applying Zeta to M1 and M2. In [5] we list
the result of appying Eta to M. And in [6] we list the result of
applying Theta to M1 and M2.

Note that, until applying Theta to M1 and M2, the variable P2 remains
free (after unification with Q1).

Finally, in [7], we excapsulate M1 and M2, resulting in two clauses, C1
and C2. Note that C1 and C2 are _connected_ in the sense that a body
literal in C1 is the body literal of C2. The predicate symbol, p_1,
bound to P2 by Theta, represents an _invented_ predicate symbol.

C1 and C2 would be added to the learned hypothesis during a dynamic
learning episode. Then, the learned hypothesis would be added to the
background knowledge for the next learning episode. In the new episode,
the initial set of metarules, including M1 and M2 _but not M_ would be
extended again, in the same way as described above and any new clauses
resulting from this extension added to the learned hypothesis for this
new step; and so on.

In this way, dynamic learning will build up a hypothesis containing long
"chains" of connected clauses incrementally but without ever having to
extend the initial set of metarules more than a single time.

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
	tp_safe_experiment_data(T,Pos,Neg,BK,MS)
	,learn_dynamic(Pos,Neg,BK,MS,Ps).



%!	learn_dynamic(+Pos,+Neg,+BK,+MS,-Prog) is det.
%
%	Learn with metarule extension for predicate invention.
%
%	Implements dynamic lerning (provisional title), a method for
%	predicate invention by metarule extension.
%
%	@tbd Metarule extension for dynamic learning is currently
%	performed by the predicate metarule_extension/4 defined in this
%	file and _not_ with extended_metarules/3 defined in
%	mil_problem.pl, even though extended_metarules/3 is itself
%	defined in terms of metarule_extension/4. Metarule extension is
%	still being developed so some confusion like this will reign for
%	a while still.
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
	,configuration:recursion_depth_limit(dynamic_learning,L)
	,debug(dynamic,'Constructing Top program',[])
	,G = dynamic_learning:top_program_dynamic(C,Pos,Neg,BK,MS,Ms)
	,recursion_guard(G,L,TP).


%!	recursion_guard(+Goal,+Time_Limit,+Theorem_Prover) is det.
%
%	Call Goal guarding for infinite recursion.
%
%	Time_Limit is passed to call_with_depth_limit/3 if necessary.
%
%	Theorem_Prover is the current value of the configuration option
%	theorem_prover/1. If this is "resolution" then Depth_Limit is
%	used with call_with_depth_limit/3. If theorem_prover/1 is set to
%	"tp" there is no reason to guard against recursion here: the TP
%	operator is guaranteed to terminate. At least it is, given a
%	definite datalog program.
%
recursion_guard(G,L,resolution):-
	!
	,call_with_depth_limit(G,L,Rs)
	,Rs \= depth_limit_exceeded.
recursion_guard(G,_L,tp):-
% TP operator is already recursion-safe.
	call(G).


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
%	in louise:top_program/5, except that a second Top program
%	construction step is taken where metarules are
%	extended and the metasubstitution atoms of the original
%	metarules in an extension pair are added to the Top program, as
%	described in learn_dynamic/5.
%
top_program_dynamic(C,Pos,Neg,BK,MS,Ts):-
	configuration:theorem_prover(resolution)
	,!
	,louise:write_program(Pos,BK,Refs)
	,top_program_dynamic(C,Pos,Neg,MS,Ms)
	%,constraints(Ms, Ms_)
	,applied_metarules(Ms,MS,Ts)
	,erase_program_clauses(Refs).

%!	top_program_dynamic(+Counter,+Pos,+Neg,+Metarules,-Metasubs) is
%!	det.
%
%	Business end of top_program_dynamic/6.
%
%	Performs Top program construction by calling top_program_/5 in
%	louise.pl. Then takes a second Top program construction step
%	where metarules are extended and retracted in order to perform
%	predicate invention. The result of the two construction steps
%	are appended in the end.
%
%	@tbd The second Top program construction step implemented in
%	this predicate performs generalisation and specialisation "in
%	one go", i.e. without first collecting all generalising
%	metasubstitutions and then specialising them. Could this be
%	applied to ordinary Top program construction, i.e. not in
%	dynamic learning?
%
top_program_dynamic(C,Pos,Neg,MS,Ms):-
	% Construct Top program for initial metarules
	(   louise:top_program_(Pos,Neg,_,MS,Ss_Neg)
	 ->  true
	;   Ss_Neg = []
	 )
	% Construct Top program for metarule extensions
	% Potentially performing predicate invention
	,examples_target(Pos,T/_)
	,findall(S
		,(metarule_extension(MS,M3,M1,M2)
		 %,debug_clauses(dynamic, [M3])
		 % Keep fresh variables for specialisation step
		 ,copy_term(M3,M3_)
		 % Generalisation
		 ,entails(+,M3,Pos,A)
		 ,constraints(A)
		 % Specialisation
		 ,\+ entails(-,M3_,Neg,A)
		 ,metasub_atom(C,T,M1,M2,S)
		 )
		,Ss_Inv_)
	,sort(Ss_Inv_, Ss_Inv_s)
	% And append
	,append(Ss_Neg,Ss_Inv_s,Ms).


%!	metarule_extension(+Metarules,+Extension,+Original1,+Original2)
%!	is det.
%
%	Extend a pair of metarules and return them and their Extension.
%
%	The original metarules, Original1 and Original2 are needed in
%	generalise_invent/4 because we want to collect their ground
%	metasubstitutions as invented clauses.
%
%	@tbd This predicate has a very confusing interface. Metarule is
%	a single argument, a list of two (expanded) metarules, however
%	it's described as a "pair" - because the predicate operates on a
%	pair of metarules. This is probably a vestige of early
%	development without prior design and needs fixin.
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


%!	entails(+Sign,+Metarule,+Examples) is det.
%
%	True when Metarule entails each of a set of Examples.
%
%	Sign is one of [+,-] denoting whether Examples is the set of
%	positive or negative examples. This is only used to remove the
%	now very bothersome ":-" prefix for negative examples.
%
entails(+,M,Pos,S):-
	member(Ep, Pos)
	,louise:metasubstitution(Ep,M,S).
entails(-,M,Neg,S):-
	member(:-En, Neg)
	,louise:metasubstitution(En,M,S).


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
metasub_atom(C,T,(S1:-_),(S2:-_),S):-
% There should only be one variable in both metasub atoms
% The variable of the invented predicate's symbol.
	term_variables([S1,S2],[V])
	,invented_symbol(T,C,V)
	% We use member/2's nondeterminism here to return
	% multiple results in setof/3 and avoid having to flatten
	% its set of results later.
	% Yeah, I know this is a bit cryptic, sorry.
	,member(S,[S1,S2]).


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
