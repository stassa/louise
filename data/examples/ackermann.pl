:-module(ackermann,[background_knowledge/2
                   ,metarules/2
                   ,positive_example/2
                   ,negative_example/2
                   ,zero/1
                   ,one/1
                   ,p/2
                   ,peano/2
                   ]).

:-use_module(configuration).

/** <module> Learning the Ackermann function with Top Program Construction.

This example shows how to learn the Ackermann function, or rather its
popular Ackermann-Péter version, from one positive example (and a set of
hand-crafted metarules).

This example shows off the ability of Louise (and Meta-Interpretive
learning in general) to learn recursive programs of arbitrary structure
from single examples. Older approaches to ILP were often severely
restricted in their ability to learn recursive programs. For example
they could only learn recursive programs with one base-case and one
inductive-case, each of a set number of literals, etc.

The experiment shown in this file "cheats" in the sense that the single
training example given to Louise is chosen from a tiny set of inputs to
the function that allow the Function to be computed in a reasonable
amount of time. Larger inputs immediately provoke the fierce
combinatorial fury of the function and make it impossible to learn it,
at least not with any approach that must execute a learned hypothesis to
verify its correctness. To paraphrase Fleming et. al (1939), *Toto, I
do not think we are in PAC-Learning anymore*.

Nevertheless, this remains the only surefire way to learn a function
with high combinatorial complexity, like the Ackermann function: choose
your training examples carefully, informed by domain knowledge.

The experiment in this file uses a set of carefully hand-crafted
metarules that map exactly to the Function's clauses. This is not
cheating, because the purpose of the experiment is to demonstrate
Louise's ability to learn a recursive function with a complex structure.
Louise can also learn metarules via its subsystem TOIL. However, TOIL is
currently implemented using an older, single-clause learning version of
Louise's Top Program Construction algorithm (TPC) and so cannot learn
what Louise's main learning predicates can learn. Accordingly, TOIL
cannot currently learn the metarules required to learn the Ackermann
function. More work is needed to bring TOIL up-to-date with the latest
version of TPC.


Known good configuration and learning problem
---------------------------------------------

Configuration (important options marked with "*"):

==
?- list_config.
* clause_limit(3)
example_clauses(call)
* experiment_file(data/examples/ackermann.pl,ackermann)
fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
listing_limit(10)
* max_error(0,0)
* max_invented(0)
metarule_formatting(quantified)
metarule_learning_limits(none)
minimal_program_size(2,inf)
recursive_reduction(false)
reduce_learned_metarules(false)
* reduction(plotkins)
* resolutions(5000)
theorem_prover(resolution)
unfold_invented(false)
true.
==

Learning problem (single-shot):

==
?- list_mil_problem(ack/3).
Positive examples
-----------------
ack(s(0),s(s(0)),s(s(s(s(0))))).

Negative examples
-----------------
[]

Background knowledge
--------------------
zero/1:
zero(0).

one/1:
one(s(0)).

p/2:
p(s(0),0).
p(s(s(A)),s(A)):-p(s(A),A).

Metarules
---------
(Ack-1) ∃.P,Q,R ∀.x,y,z: P(x,y,z)← Q(x),R(z,y)
(Ack-2) ∃.P,Q ∀.x,y,z,u,v,w: P(x,y,z)← Q(x,u),Q(y,v),P(x,v,w),P(u,w,z)
(Ack-3) ∃.P,Q,R,S ∀.x,y,z,u,v: P(x,y,z)← Q(x,u),R(y),S(v),P(u,v,z)

true.
==

Learning query
--------------

==
?- time(learn(ack/3)).
ack(A,B,C):-p(A,D),p(B,E),ack(A,E,F),ack(D,F,C).
ack(A,B,C):-zero(A),p(C,B).
ack(A,B,C):-p(A,D),zero(B),one(E),ack(D,E,C).
% 42,774 inferences, 0.000 CPU in 0.009 seconds (0% CPU, Infinite Lips)
true.
==


Flattening
----------

The target theory for this example is the popular version of the
Ackermann function. Louise learns it as a definite program with
flattening:

==
% Ackermann-Peter function, without flattening:
ack_th(0,N,s(N)).
ack_th(s(M),0,P):- ack_th(M,s(0),P).
ack_th(s(M),s(N),P):- ack_th(s(M),N,S), ack_th(M,S,P).

% With flattening:
ack_flat(Z,N,M):- zero(Z), p(M,N).
ack_flat(M,Z,P):- p(M,N), zero(Z), one(O), ack_flat(N,O,P).
ack_flat(M,N,P):- p(M,M_), p(N,N_), ack_flat(M,N_,S), ack_flat(M_,S,P).
==

Flattening replaces each functional term in a clause literal with a new
variable and a new literal in the body of the clause sharing that
variable. In the flattened vesrion above, the term "s(N)", secondin the
head (and only) literal of the first clause is replaced by a variable M
shared with th eliteral p(M,N) added to the body of the clause.

Flattening is required when the target theory has functional terms
because metarules are _datalog_, meaning that they have no function
symbols other than constants.


Testing
-------

We can do a bit of testing against the target theory to see if what was
learned is really correct. This makes sense since the flattened
hypothesis learned is a little harder to read than the non-flattened
version.

The test queries below use the predicate peano/2 defined in this module.
This can be used to convert between arabic notation and Peano notation,
just because it's easier to read arabic notation.

Before running the test-queries below, remember to load the learned
program and the target theory (ack_th/3, above) to memory (e.g. paste
them into this file and reload):

==
?- _S = ackermann, _N = 1, _M = 2, _S:peano(_N,N), _S:peano(_M,M), _S:ack_th(N,M,P), _S:ack(N,M,P_), _S:peano(Q,P_), !.
N = s(0),
M = s(s(0)),
P = P_, P_ = s(s(s(s(0)))),
Q = 4.

?- _S = ackermann, _N = 2, _M = 2, _S:peano(_N,N), _S:peano(_M,M), _S:ack_th(N,M,P), _S:ack(N,M,P_), _S:peano(Q,P_), !.
N = M, M = s(s(0)),
P = P_, P_ = s(s(s(s(s(s(s(0))))))),
Q = 7.

?- _S = ackermann, _N = 2, _M = 3, _S:peano(_N,N), _S:peano(_M,M), _S:ack_th(N,M,P), _S:ack(N,M,P_), _S:peano(Q,P_), !.
N = s(s(0)),
M = s(s(s(0))),
P = P_, P_ = s(s(s(s(s(s(s(s(s(0))))))))),
Q = 9.

?- _S = ackermann, _N = 3, _M = 3, _S:peano(_N,N), _S:peano(_M,M), _S:ack_th(N,M,P), _S:ack(N,M,P_), _S:peano(Q,P_), !.
N = M, M = s(s(s(0))),
P = P_, P_ = s(s(s(s(s(s(s(s(s(s(...)))))))))),
Q = 61.

?- _S = ackermann, _N = 3, _M = 4, _S:peano(_N,N), _S:peano(_M,M), _S:ack_th(N,M,P), _S:ack(N,M,P_), _S:peano(Q,P_), !.
N = s(s(s(0))),
M = s(s(s(s(0)))),
P = P_, P_ = s(s(s(s(s(s(s(s(s(s(...)))))))))),
Q = 125.
==

The above are "easy" in they don't trigger the full complexity of the
Ackermann function. Try, for example, with _N = 4, _M = 1. That takes a
lot longer to compute!

*/

% Set to the number of clauses in target theory.
:- auxiliaries:set_configuration_option(clause_limit, [3]).
% No need for predicate invention.
:- auxiliaries:set_configuration_option(max_invented, [0]).

configuration:ack_1 metarule 'P(x,y,z):- Q(x),R(z,y)'.
configuration:ack_2 metarule 'P(x,y,z):- Q(x,u),Q(y,v),P(x,v,w),P(u,w,z)'.
configuration:ack_3 metarule 'P(x,y,z):- Q(x,u),R(y),S(v),P(u,v,z)'.

background_knowledge(ack/3, [zero/1,one/1,p/2]).

metarules(ack/3,[ack_1,ack_2,ack_3]).

positive_example(ack/3,ack(s(0),s(s(0)),s(s(s(s(0)))))).

negative_example(ack/3,_):- fail.


%!      zero(?Zero) is semidet.
%
%       The number Zero.
%
zero(0).


%!      one(?One) is semidet.
%
%       The number One in Peano notation.
%
one(s(0)).


%!      p(?N,?S) is nondet.
%
%       Peano successor relation.
%
p(s(0),0).
p(s(s(N)),s(N)):-
        p(s(N),N).



%!      peano(?Digit,?Peano) is nondet.
%
%       Convert between a Digit and a Peano number.
%
%       Use this program to quickly convert between Peano notation and
%       arabic notation for easy testing of a learned theory.
%
%       Accepted modes are (+,-) and (-,+). In mode (?,?) this predicate
%       returnsonly 0 deterministically.
%
%       Examples:
%       ==
%       ?- ackermann:peano(1,P).
%       P = s(0).
%
%       ?- ackermann:peano(I,s(0)).
%       I = 1.
%
%       ?- ackermann:peano(N,P).
%       N = P, P = 0.
%       ==
%
peano(N,P):-
        peano(P,0,N).

peano(0,N,N):-
        !.
peano(s(P),N,Acc):-
        succ(N,N_)
        ,peano(P,N_,Acc).
