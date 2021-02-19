:-module(yamamoto, [background_knowledge/2
                   ,metarules/2
                   ,positive_example/2
                   ,negative_example/2
                   ]).

:-use_module(configuration).

/** <module> Yamamoto's incompleteness result for Inverse Resolution.

Yamamoto gives an example of Inverse Resolution's incompleteness by
showing an example where the target theory does not subsume the bottom
clause. The example is as follows:

==
B2 = (even(0):- AND (even(s(x)):- odd(x))
E2 = odd(s(s(s(0)))):- .
==

Yamamoto proposes the following correct hypothesis:

==
H2 = odd(s(y)):- even(y).
==

Below we look at how Louise and Top Program Construction fare against
this example.

Listed queries work with the following configuration options:
==
?- list_config.
example_clauses(call)
experiment_file(data/drafts/functions/yamamoto.pl,yamamoto)
generalise_learned_metarules(true)
learned_metarules_printing(pretty)
learner(louise)
max_invented(2)
metarule_learning_limits(coverset)
minimal_program_size(2,inf)
recursion_depth_limit(dynamic_learning,5000)
recursive_reduction(true)
reduce_learned_metarules(false)
reduction(plotkins)
resolutions(5000)
symbol_range(predicate,[P,Q,R,S,T])
symbol_range(variable,[X,Y,Z,U,V,W])
theorem_prover(resolution)
unfold_invented(true)
true.
==


1. Louise can invent new examples and learn the target theory:
==
?- examples_invention(odd/1).
m(odd,s(0)).
m(odd,s(s(s(0)))).
true.

?- learn_with_examples_invention(odd/1).
odd(s(s(s(0)))).
odd(A):-s(A,B),even(B).
true.
==


2. Without inventing new examples Louise can't learn H2 from Yamamoto's
setup. The hypothesis learned is an over-specialisation:

==
?- list_encapsulated_problem(odd/1).
Positive examples
-----------------
m(odd,s(s(s(0)))).

Negative examples
-----------------
[]

Background knowledge
--------------------
m(even,0).
m(even,s(A)):-p(odd,A).
m(s,s(A),A).
p(odd,s(s(s(0)))).

Metarules
---------
m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E).
true.

?- learn(odd/1).
odd(s(s(s(0)))).
true.

?- learn_dynamic(odd/1).
odd(A):-s(A,B),s(B,C),s(C,D),even(D).
true.
==


3. Adding the base-case allows Louise to learn the target theory:

==
?- list_encapsulated_problem(odd/1).
Positive examples
-----------------
m(odd,s(0)).
m(odd,s(s(s(0)))).

Negative examples
-----------------
[]

Background knowledge
--------------------
m(even,0).
m(even,s(A)):-p(odd,A).
m(s,s(A),A).
p(odd,s(0)).
p(odd,s(s(s(0)))).

Metarules
---------
m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E).
true.

?- learn(odd/1).
odd(A):-s(A,B),even(B).
true.

?- learn_dynamic(odd/1).
odd(A):-s(A,B),even(B).
true.
==


4. We can move the base-case around and Louise learns the same
hypothesis (but with some overs-pecialised clauses):

==
?- list_encapsulated_problem(odd/1).
Positive examples
-----------------
m(odd,s(s(s(0)))).
m(odd,s(s(s(s(s(0)))))).

Negative examples
-----------------
[]

Background knowledge
--------------------
m(even,0).
m(even,s(A)):-p(odd,A).
m(s,s(A),A).
p(odd,s(s(s(0)))).
p(odd,s(s(s(s(s(0)))))).

Metarules
---------
m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E).
true.

?- learn(odd/1).
odd(s(s(s(0)))).
odd(A):-s(A,B),even(B).
true.

?- learn_dynamic(odd/1).
odd(A):-s(A,B),s(B,C),s(C,D),even(D).
odd(A):-s(A,B),even(B).
true.
==


5. When examples are further apart, the hypothesis over-specialises
again (the more general clause isn't learned anymore):

==
?- list_encapsulated_problem(odd/1).
Positive examples
-----------------
m(odd,s(s(s(0)))).
m(odd,s(s(s(s(s(s(s(0)))))))).

Negative examples
-----------------
[]

Background knowledge
--------------------
m(even,0).
m(even,s(A)):-p(odd,A).
m(s,s(A),A).
p(odd,s(s(s(0)))).
p(odd,s(s(s(s(s(s(s(0)))))))).

Metarules
---------
m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E).
true.

?- learn(odd/1).
odd(s(s(s(0)))).
odd(s(s(s(s(s(s(s(0)))))))).
true.

?- learn_dynamic(odd/1).
odd(A):-s(A,B),s(B,C),s(C,D),even(D).
true.
==

*/

configuration: postcon_unit metarule 'P(x):- Q(x,y), R(y)'.

configuration:learning_predicate(learn_with_examples_invention/2).

:- auxiliaries:set_configuration_option(max_invented, [2]).

background_knowledge(odd/1, [even/1,s/2]).

metarules(odd/1,[postcon_unit]).

positive_example(odd/1,odd(N)):-
        odd(N).

negative_example(_,_):-
        fail.

s(s(X),X).

even(0).
even(s(X)):-
        odd(X).

%/* 1,2. Yamamoto's setup:
odd(s(s(s(0)))).
%*/

/* 3. Adding the base case:
odd(s(0)).
odd(s(s(s(0)))).
*/


/* 4. Giving a different base case:
odd(s(s(s(0)))).
odd(s(s(s(s(s(0)))))).
*/

/* 5. Examples are further apart:
odd(s(s(s(0)))).
odd(s(s(s(s(s(s(s(0)))))))).
*/
