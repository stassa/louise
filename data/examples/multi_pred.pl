:-module(multi_pred, [background_knowledge/2
                     ,metarules/2
                     ,positive_example/2
                     ,negative_example/2
                     ,pred/2
                     ]).

:-use_module(configuration).

/** <module> Example of multi-predicate learning in Louise.

This experiment file shows how to use Louise's ability for
multi-predicate learning to jointly learn mutually recursive hypotheses
of the predicates "odd" and "even". In multi-predicate learning, a
learner is given examples of more than one target predicate and must
learn a hypothesis that represents all learning targets.

There is no special configuration necessary to use multi-predicate
learning in Louise. The learning predicates learn/[1,2] accept a list
of target predicates as an argument, e.g. learn([p/2,q/1,r/3]) and will
learn a hypothesis of all target predicates. learn/5 does not need any
special syntax- passing a list of examples of multiple predicates as the
first argument to learn/5 is all that's needed.

Note that the two MIL problems in this file represent natural numbers as
peano numbers where 0 is the first natural number and each natural
number after zero is a successor of the previous natural number: 0,
s(0), s(s(0)), ... and so on.

Running the experiment
----------------------

1. The learning queries in this file work as presented with the
following configuration options. Important options are marked with an
asterisk (*):

==
?- list_config.
* clause_limit(0)
example_clauses(call)
* experiment_file(data/examples/multi_pred.pl,multi_pred)
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


2. List the elements of the MIL problem.

This experiment file defines the elements of the MIL problems for two
learning targets, even/1 and odd/1. If we list the elements of the MIL
problem with list_mil_problem/1, we'll see that examples include atoms
of both target predicates:

==
?- list_mil_problem([even/1,odd/1]).
Positive examples
-----------------
even(0).
even(s(s(0))).
odd(s(0)).
odd(s(s(s(0)))).

Negative examples
-----------------
:-even(s(0)).
:-even(s(s(s(0)))).
:-odd(0).
:-odd(s(s(0))).

Background knowledge
--------------------
pred/2:
pred(s(0),0).
pred(s(A),s(B)):-pred(A,B).

Metarules
---------
(Postcon-unit) ∃.P,Q,R ∀.x,y: P(x)← Q(x,y),R(y)
(Abduce-unit) ∃.P,X: P(X)←
==


3. Louise can learn mutually recursive definitions of the two target
predicates simultaneously i.e. it can perform multi-predicate learning.
This is shown below:

==
?- learn([even/1,odd/1]).
even(0).
even(A):-pred(A,B),odd(B).
odd(A):-pred(A,B),even(B).
true.
==


4. Neither target can be learned independently as defined:

==
?- learn(even/1).
even(0).
even(s(s(0))).
true.

?- learn(odd/1).
odd(s(0)).
odd(s(s(s(0)))).
true.
==

This is because the definition of even/1 is not yet available as
background knowledge for odd/1, and vice-versa.


5. abduce_unit metarule

The abduce_unit metarule, P(X), used below is not strictly necessary.
Without it, Louise learns the same programs listed above. The difference
is that the base-case of each recursive program, e.g. even(0),
odd(s(0)), etc, are actually examples that are not entailed by the rest
of the program (a.k.a. "atomic residue").

This is possible to see by turning on debugging in the configuration
module. Uncomment the following lines:
==
:-debug(learn). % Debug learning steps.
:-debug(top_program). % Debug Top program construction.
==

Then call learn/1 on the multi-predicate problem:
==
?- learn([odd/1,even/1]).
% Encapsulating problem
% Constructing Top program...
% Constructing Top program...
% Generalised Top program
% m(abduce_unit,even,0)-(m(abduce_unit,A,B):-m(A,B)).
% m(abduce_unit,even,s(s(0)))-(m(abduce_unit,A,B):-m(A,B)).
% m(abduce_unit,odd,s(0))-(m(abduce_unit,A,B):-m(A,B)).
% m(abduce_unit,odd,s(s(s(0))))-(m(abduce_unit,A,B):-m(A,B)).
% m(postcon_unit,even,pred,odd)-(m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E)).
% m(postcon_unit,odd,pred,even)-(m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E)).
% Specialised Top program
% m(abduce_unit,even,0)-(m(abduce_unit,A,B):-m(A,B)).
% m(abduce_unit,even,s(s(0)))-(m(abduce_unit,A,B):-m(A,B)).
% m(abduce_unit,odd,s(0))-(m(abduce_unit,A,B):-m(A,B)).
% m(abduce_unit,odd,s(s(s(0))))-(m(abduce_unit,A,B):-m(A,B)).
% m(postcon_unit,even,pred,odd)-(m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E)).
% m(postcon_unit,odd,pred,even)-(m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E)).
% Applied metarules
% m(even,0).
% m(even,s(s(0))).
% m(odd,s(0)).
% m(odd,s(s(s(0)))).
% m(even,A):-m(pred,A,B),m(odd,B).
% m(odd,A):-m(pred,A,B),m(even,B).
% Reducing Top program...
% Excapsulating hypothesis
even(0).
even(A):-pred(A,B),odd(B).
odd(A):-pred(A,B),even(B).
true.
==

Note that the generalised and specialised Top program includes
metasubstitutions of abduce_unit. Now, remove abduce_unit from the
metarules of the two target predicates:

==
?- list_encapsulated_problem([odd/1,even/1]).
Positive examples
-----------------
m(odd,s(0)).
m(odd,s(s(s(0)))).
m(even,0).
m(even,s(s(0))).

Negative examples
-----------------
:-m(odd,0).
:-m(odd,s(s(0))).
:-m(even,s(0)).
:-m(even,s(s(s(0)))).

Background knowledge
--------------------
m(pred,s(0),0).
m(pred,s(A),s(B)):-m(pred,A,B).

Metarules
---------
m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E).
true.
==

And call learn/1 again:
==
?- learn([odd/1,even/1]).
% Encapsulating problem
% Constructing Top program...
% Constructing Top program...
% Generalised Top program
% m(postcon_unit,even,pred,odd)-(m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E)).
% m(postcon_unit,odd,pred,even)-(m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E)).
% Specialised Top program
% m(postcon_unit,even,pred,odd)-(m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E)).
% m(postcon_unit,odd,pred,even)-(m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E)).
% Applied metarules
% m(even,A):-m(pred,A,B),m(odd,B).
% m(odd,A):-m(pred,A,B),m(even,B).
% Reducing Top program...
% Excapsulating hypothesis
even(0).
even(A):-pred(A,B),odd(B).
odd(A):-pred(A,B),even(B).
true.
==

Note that, while this time there are no metasubstitutions of
abduce_unit in the generalised and specialised Top program, the single
atom of even/1 is kept in the output. That is because it can't be
reduced by Plotkin's algorithm, since it is not entailed by the rest of
the learned program.

*/

configuration:postcon_unit metarule 'P(x):- Q(x,y), R(y)'.
configuration:abduce_unit metarule 'P(X)'.

background_knowledge(even/1, [pred/2]).
background_knowledge(odd/1, [pred/2]).

metarules(even/1,[postcon_unit,abduce_unit]).
metarules(odd/1,[postcon_unit,abduce_unit]).
%metarules(even/1,[postcon_unit]).
%metarules(odd/1,[postcon_unit]).

positive_example(even/1,even(X)):-
        member(X,[0,s(s(0))]).
positive_example(odd/1,odd(X)):-
        member(X,[s(0),s(s(s(0)))]).

negative_example(even/1,even(X)):-
        member(X,[s(0),s(s(s(0)))]).
negative_example(odd/1,odd(X)):-
        member(X,[0,s(s(0))]).


%!      pred(?N,?M) is nondet.
%
%       True when N is a predecessor of M.
%
%       N and M are natural numbers in peano notation: 0, s(0), s(s(0)),
%       s(s(s(0))), ... etc.
%
pred(s(0),0).
pred(s(A),s(B)):-
        pred(A,B).
