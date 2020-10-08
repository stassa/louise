:-module(multi_pred, [background_knowledge/2
                     ,metarules/2
                     ,positive_example/2
                     ,negative_example/2
                     ,predecessor/2
                     ]).

/** <module> Example of multi-predicate learning in Louise.

The two MIL problems in this file are set-up to demonstrate how to use
Louise's ability for multi-predicate learning. In multi-predicate
learning, a learner is given examples of more than one target predicate
and must learn a hypothesis that represents all learning targets.

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
following configuration options:

==
?- list_config.
example_clauses(call)
experiment_file(data/examples/multi_pred.pl,multi_pred)
learner(louise)
max_invented(1)
minimal_program_size(2,inf)
recursion_depth_limit(dynamic_learning,500)
recursive_reduction(false)
reduction(plotkins)
resolutions(5000)
symbol_range(predicate,[P,Q,R,S,T])
symbol_range(variable,[X,Y,Z,U,V,W])
theorem_prover(resolution)
unfold_invented(false)
true.
==

2. This experiment file defines the elements of the MIL problems for two
learning targets, even/1 and odd/1. Louise can learn mutually recursive
definitions of the two target predicates simultaneously i.e. it can
perform multi-predicate learning.

==
?- learn([even/1,odd/1]).
even(0).
even(A):-predecessor(A,B),odd(B).
odd(A):-predecessor(A,B),even(B).
true.
==

3. Neither target can be learned independently as defined:

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

4. It _is_ possible to learn odd/1 by inventing even/1 and vice-versa,
using dynamic learning:

==
?- learn_dynamic(even/1).
even(0).
'$1'(A):-predecessor(A,B),even(B).
even(A):-predecessor(A,B),'$1'(B).
true.

?- learn_dynamic(odd/1).
odd(s(0)).
'$1'(A):-predecessor(A,B),odd(B).
odd(A):-predecessor(A,B),'$1'(B).
true.
==

However, in the case of the learning targets defined in this experiment
file, we have examples of both so it's possible to perform
multi-predicate learning instead.

Note that multi-predicate learning results in a more compact definition
of both targets than predicate invention. This is not _always_ the case,
but it might be a consideration when deciding which learning predicate
to use.

Abduction metarules
-------------------

Note that the abduce_unit metarule, P(X), used below is not strictly
necessary. Without it, Louise learns the same programs listed above. The
difference is that the base-case of each recursive program, e.g.
even(0), odd(s(0)), etc, are actually examples that are not entailed by
the rest of the program (a.k.a. "atomic residue").

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
% m(postcon_unit,even,predecessor,odd)-(m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E)).
% m(postcon_unit,odd,predecessor,even)-(m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E)).
% Specialised Top program
% m(abduce_unit,even,0).
% m(abduce_unit,even,s(s(0))).
% m(abduce_unit,odd,s(0)).
% m(abduce_unit,odd,s(s(s(0)))).
% m(postcon_unit,even,predecessor,odd).
% m(postcon_unit,odd,predecessor,even).
% Reducing Top program...
% Excapsulating hypothesis
even(0).
even(A):-predecessor(A,B),odd(B).
odd(A):-predecessor(A,B),even(B).
true.
==

Note that the generalised and specialised Top program includes
metasubstitutions of unit_abduce. Now, remove unit_abduce from the
metarules of the two target predicates:

==
metarules(even/1,[postcon_unit]).
metarules(odd/1,[postcon_unit]).
==

And call learn/1 again:
==
?- learn([odd/1,even/1]).
% Encapsulating problem
% Constructing Top program...
% Constructing Top program...
% Generalised Top program
% m(postcon_unit,even,predecessor,odd)-(m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E)).
% m(postcon_unit,odd,predecessor,even)-(m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E)).
% Specialised Top program
% m(postcon_unit,even,predecessor,odd).
% m(postcon_unit,odd,predecessor,even).
% Reducing Top program...
% Excapsulating hypothesis
even(0).
even(A):-predecessor(A,B),odd(B).
odd(A):-predecessor(A,B),even(B).
true.
==

Note that, while this time there are no metasubstitutions of
unit_abduced in the generalised and specialised Top program, the single
atom of even/1 is kept in the output because it can't be reduced by
Plotkin's algorithm, since it is not entailed by the rest of the learned
program.

*/

:-use_module(configuration).

configuration:postcon_unit metarule 'P(x):- Q(x,y), R(y)'.
configuration:abduce_unit metarule 'P(X)'.

background_knowledge(even/1, [predecessor/2]).
background_knowledge(odd/1, [predecessor/2]).

metarules(even/1,[postcon_unit,abduce_unit]).
metarules(odd/1,[postcon_unit,abduce_unit]).

positive_example(even/1,even(X)):-
        member(X,[0,s(s(0))]).
positive_example(odd/1,odd(X)):-
        member(X,[s(0),s(s(s(0)))]).

negative_example(even/1,even(X)):-
        member(X,[s(0),s(s(s(0)))]).
negative_example(odd/1,odd(X)):-
        member(X,[0,s(s(0))]).


%!      predecessor(?X,?Y) is nondet.
%
%       True Y is the predecessor of X.
%
%       The predecessor relation is defined recursively as follows:
%
%       0 is the predecessor of s(0).
%       s(N) is the predecessor of s(s(N)).
%
predecessor(s(0),0).
predecessor(s(N),s(M)):-
        predecessor(N,M).
