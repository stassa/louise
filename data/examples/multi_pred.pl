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


Running the experiment
----------------------

1. The learning queries in this file work as presented with the
following configuration options:

==
?- list_config.
experiment_file(data/examples/multi_pred.pl,multi_pred)
learner(louise)
max_invented(1)
minimal_program_size(1,inf)
recursion_depth_limit(dynamic_learning,500)
recursive_reduction(false)
reduction(plotkins)
resolutions(5000)
symbol_range(predicate,[P,Q,R,S,T])
symbol_range(variable,[X,Y,Z,U,V,W])
theorem_prover(resolution)
true.
==

2. This experiment file defines the elements of the MIL problems for two
learning targets, even/1 and odd/1. Louise can learn mutually depending
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
even(2).
true.

?- learn(odd/1).
odd(1).
odd(3).
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
odd(1).
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

Other learning predicates
-------------------------

Currently, multi-predicate learning is only well-supported for the
default learning predicates, learn/[1,2,5]. Other learning predicates,
such as learn_with_examples_invention/[1,2,5], learn_dynamic/[1,2,5] etc
may or may not work well when attempting multi-predicate learning. This
will usually depend on the MIL problem.

For example, while dynamic learning can be used to learn a mutually
dependent definition of even/1 and odd/1, there is no need to invent any
sub-programs, so using learn_dynamic/[1,2,5] will not have a different
result than learn/[1,2,5]:

==
?- learn_dynamic([even/1,odd/1]).
even(0).
even(A):-predecessor(A,B),odd(B).
odd(A):-predecessor(A,B),even(B).
true.
==

And the same goes for learn_with_examples_invention/[1,2,5]:

==
?- learn_with_examples_invention([even/1,odd/1]).
even(0).
even(A):-predecessor(A,B),odd(B).
odd(A):-predecessor(A,B),even(B).
true.
==

*/

:-use_module(configuration).

configuration:postcon_unit metarule 'P(x):- Q(x,y), R(y)'.

background_knowledge(even/1, [predecessor/2]).
background_knowledge(odd/1, [predecessor/2]).

metarules(even/1,[postcon_unit]).
metarules(odd/1,[postcon_unit]).

positive_example(even/1,even(X)):-
        member(X,[0,2]).
positive_example(odd/1,odd(X)):-
        member(X,[1,3]).

negative_example(even/1,even(X)):-
        member(X,[1,3]).
negative_example(odd/1,odd(X)):-
        member(X,[0,2]).


%!      predecessor(?X,?Y) is nondet.
%
%       True when X is a natural number and Y is X - 1.
%
predecessor(X,Y):-
        number(X)
        ,between(0,inf,X)
        ,Y is X - 1.
