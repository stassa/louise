:-module(soil, [soil_aggregated/1
	       ,soil_aggregated/2
	       ,soil_aggregated/5
               ]).

:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).
:-use_module(subsystems(thelma/thelma)).


/** <module> Second-Order Inductive Learner.

SOIL combines Louise and Thelma in a best-of-both-worlds
Meta-Interpretive Learning (MIL) system.

SOIL is still a prototype and not fully implemented.

The best of both worlds
-----------------------

Louise is a MIL system that learns by Top Program Construction (TPC).
TPC is a polynomial-time algorithm that constructs a Top Program, a
unique object in the space of hypotheses that is a correct hypothesis
that entails all the positive and none of the negative training
examples. The Top Program can be constructed directly, without a search
of the Hypothesis Space and in polynomial time. TPC is capable of
learning an approximate theory when the Hypothesis Space does not
include a correct hypothesis, such as when some training examples are
mislabelled.

Thelma is a re-implementation of Metagol, the original MIL system, that
learns by searching the Hypothesis Space for a correct hypothesis. The
Hypothesis Space is an exponentially-growing search space and so
Thelma's hypothesis search has exponential time complexity. Thelma is
designed to only learn a correct hypothesis and fails when mislabelled
examples make it impossible to do so.

Louise is most useful when the Hypothesis Space is large, or when the
Hypothesis Space does not include a correct hypothesis. Louise has
various subsystems (other than Thelma) that make it possible to learn
under different conditions, e.g. by performing predicate invention or
one-shot learning a recursive hypothesis, etc. On the other hand, Thelma
is best suited when the hypothesis space is small and includes a correct
hypothesis and can learn more easily from single examples, particularly
when the target theory must be recursive.

Combining Thelma and Louise together gives us a MIL system with the
capabilities of both systems.

Combining search-based and constructive MIL
-------------------------------------------

1. Aggregating results.

There are two ways to combine Thelma and Louise, realised in two
families of learning predicate defined in this module.

The first way is to make a first learning attempt with Louise and then
pass any learned clauses to Thelma as a partial hypothesis, to be
completed. This is particularly useful when predicate invention or
one-shot recursive learning is needed. Conversely, this should be
avoided when the Hypothesis Space is large. When the Hypothesis Space is
large and predicate invention is needed, Louise's dynamic learning
subsystem should be preferred over SOIL.

This first way of combining Thelma and Louise is implemented as a family
of laerning predicates: soil_aggregated/[1,2,5]. We give an example
below.

==
?- list_config.
depth_limits(3,1)
example_clauses(call)
experiment_file(data/examples/anbn.pl,anbn)
generalise_learned_metarules(false)
learned_metarules_printing(pretty)
learner(louise)
max_invented(1)
metarule_learning_limits(none)
minimal_program_size(2,inf)
recursion_depth_limit(dynamic_learning,none)
recursive_reduction(false)
reduce_learned_metarules(false)
reduction(plotkins)
resolutions(5000)
theorem_prover(resolution)
unfold_invented(false)
true.

% Use this constraint:
order_constraints(chain,[P,Q,_R],_Fs,[P>Q],[]).

?- list_mil_problem('S'/2).
Positive examples
-----------------
'S'([a,b],[]).
'S'([a,a,b,b],[]).
'S'([a,a,a,b,b,b],[]).

Negative examples
-----------------
[]

Background knowledge
--------------------
A/2:
'A'([a|A],A).

B/2:
'B'([b|A],A).

Metarules
---------
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
true.

% Debug to see what is being learned
?- debug(soil).
true.

?- soil_aggregated('S'/2).
% Encapsulating problem...
% Learning initial theory with learn/5...
% Completing initial theory with Thelma:
% 'S'([a,a,b,b],[]).
% 'S'([a,a,a,b,b,b],[]).
% 'S'(A,B):-'A'(A,C),'B'(C,B).
% Completed theory:
% 'S'(A,B):-'A'(A,C),'B'(C,B).
% 'S'(A,B):-'$1'(A,C),'B'(C,B).
% '$1'(A,B):-'A'(A,C),'S'(C,B).
'S'(A,B):-'A'(A,C),'B'(C,B).
'S'(A,B):-'$1'(A,C),'B'(C,B).
'$1'(A,B):-'A'(A,C),'S'(C,B).
true ;
% Completed theory:
% []
[]
true.
==


2. Combining learning.

The second way is to use Thelma as an alternative predicate invention
engine to dynamic learning. Thelma is very good at one-shot learning of
recursive theories, without having to jumpt through hoops (as is
necessary in Louise's examples invention subsystem) and, in MIL,
predicate invention is always a one-shot learning problem. In this case,
Louise passes to Thelma each atom that fails resolution during its
meta-interpretive learning proof procedure and Thelma tries to invent a
definition of a predicate that entails the single atom it is passed.
Note: _this capability is currently not yet implemented_!

*/



%!	soil_aggregated(+Targets) is nondet.
%
%	Learn a program with Louise and Thelma, aggregating results.
%
soil_aggregated(T):-
	soil_aggregated(T,Ps)
	,print_clauses(Ps).



%!	soil_aggregated(+Targets,-Program) is nondet.
%
%	Learn a Program with Louise and Thelma, aggregating results.
%
soil_aggregated(T,Ps):-
	tp_safe_experiment_data(T,Pos,Neg,BK,MS)
	,soil_aggregated(Pos,Neg,BK,MS,Ps).


%!	soil_aggregated(+Pos,+Neg,+BK,+Metarules,-Program) is nondet.
%
%	Learn a Program with Louise and Thelma, aggregating results.
%
soil_aggregated(Pos,Neg,BK,MS,Ps):-
	(   configuration:learning_predicate(F/A)
	->  true
	;   F/A = learn/5
	)
	,debug(soil,'Encapsulating problem...',[])
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,debug(soil,'Learning initial theory with ~w...',[F/A])
	,learning_query(Pos_,Neg_,BK_,MS_,Hs)
	,debug_clauses(soil,'Completing initial theory with Thelma:',Hs)
	,thelma_complete(Pos,Neg,BK,MS,Hs,Ps)
	,debug_clauses(soil,'Completed theory:',Ps).
