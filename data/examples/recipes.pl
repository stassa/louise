:-module(recipes, [background_knowledge/2
		  ,metarules/2
		  ,positive_example/2
		  ,negative_example/2
		  ,replace/4
		  ,break_eggs/2
		  ,whisk_eggs/2
		  ,heat_oil/2
		  ,fry_eggs/2
		  ,season/2
		  ,replace/4
		  ]).

:-use_module(configuration).

/** <module> Learn how to make an omelette with predicate invention.

__Table of Conents__

==
1. Introduction
2. Known good configuration.
3. MIL problem elements.
4. Make a first learning attempt.
5. Eyballing the results.
6. Applying constraints.
7. Unfolding the reduced program
8. Dynamically setting configuration options
9. Conclusions
==

Search for the titles above to jump to their respective sections.


__1. Introduction__

The examples in this experiment file demonstrates the use of constraints
to improve the learning time for a large program by removing recursive
clauses and controlling predicate invention; how to unfold the learned
program to remove invented predicates and make the learned program
easier to understand; and how to dynamically set configuration options.

This experiment file should be read side-by-side with anbn.pl which
describes similar concepts.

The learning target for this example is a recipe to make an omelette,
learned from a single, positve example. I mean, how many examples do you
really need to make an omelette? This is not deep learning.


__2. Known good configuration.__

The results listed below were obtained with the following configuration
options. Required options are marked with "*":

==
?- list_config.
* clause_limit(4)
example_clauses(call)
* experiment_file(data/examples/recipes.pl,recipes)
fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
listing_limit(10)
* max_error(0,0)
* max_invented(2)
metarule_formatting(quantified)
metarule_learning_limits(none)
minimal_program_size(2,inf)
recursive_reduction(false)
reduce_learned_metarules(false)
* reduction(plotkins)
* resolutions(5000)
theorem_prover(resolution)
* unfold_invented(false)
true.
==

If the output you see when you run the experiment differs from what is
listed in the following sections, make sure your configuration matches
the listing above and try again.

In particular, make sure that the following options are set:
==
clause_limit(4)
max_invented(2)
==

The option clause_limit(4) tells Louise to try and learn up to 4 clauses
from each positive example. This is necessary because each proof that
leadsd to a correct hypothesis goes through 4 clauses resolving with
each other.

The option max_invented(2) tells Louise to attempt to define at least
one invented predicate.


__3. MIL problem elements.__

Make sure that the output of list_mil_problem/1 matches the following:
==
?- list_mil_problem(recipe/2).
Positive examples
-----------------
recipe([egg_whisk,eggs,frying_pan,olive_oil,pepper,salt],[omelette]).

Negative examples
-----------------
[]

Background knowledge
--------------------
break_eggs/2:
break_eggs(A,B):-replace([eggs],A,[egg_whites,egg_yolks],B).

whisk_eggs/2:
whisk_eggs(A,B):-replace([egg_whisk,egg_whites,egg_yolks],A,[whisked_eggs],B).

heat_oil/2:
heat_oil(A,B):-replace([frying_pan,olive_oil],A,[frying_oil],B).

fry_eggs/2:
fry_eggs(A,B):-replace([frying_oil,whisked_eggs],A,[frying_eggs],B).

season/2:
season(A,B):-replace([frying_eggs,pepper,salt],A,[omelette],B).

replace/4:
replace(A,B,C,D):-ground(A),ground(B),ground(C),ord_subset(A,B),ord_subtract(B,A,E),ord_union(C,E,D).

Metarules
---------
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
true.
==


__4. Make a first learning attempt.__

Don't be surprised if this first learning attempt takes a long time. The
hypothesis that can be learned from the elements of the learning problem
above includes both recursive clauses and invented predicates and
completing its proof is expensive.

You can get an idea of how long it will take by looking at the first
line of output, below, listing number of inferences and time in
seconds, among other details, that it took for the learning query to
terminate:

==
?- time(learn(recipe/2,_Ps)), print_clauses(_Ps), length(_Ps,N).
% 81,750,748 inferences, 30.031 CPU in 31.357 seconds (96% CPU, 2722189 Lips)
recipe([egg_whisk,eggs,frying_pan,olive_oil,pepper,salt],[omelette]).
'$1'(A,B):-'$1'(A,C),'$1'(C,B).
'$1'(A,B):-'$1'(A,C),'$2'(C,B).
'$1'(A,B):-'$1'(A,C),fry_eggs(C,B).
'$1'(A,B):-'$1'(A,C),heat_oil(C,B).
'$1'(A,B):-'$1'(A,C),recipe(C,B).
'$1'(A,B):-'$1'(A,C),season(C,B).
'$1'(A,B):-'$1'(A,C),whisk_eggs(C,B).
'$1'(A,B):-'$2'(A,C),'$1'(C,B).
'$1'(A,B):-'$2'(A,C),'$2'(C,B).
'$1'(A,B):-'$2'(A,C),fry_eggs(C,B).
'$1'(A,B):-'$2'(A,C),heat_oil(C,B).
'$1'(A,B):-'$2'(A,C),recipe(C,B).
'$1'(A,B):-'$2'(A,C),season(C,B).
'$1'(A,B):-'$2'(A,C),whisk_eggs(C,B).
'$1'(A,B):-break_eggs(A,C),'$1'(C,B).
'$1'(A,B):-break_eggs(A,C),'$2'(C,B).
'$1'(A,B):-break_eggs(A,C),heat_oil(C,B).
'$1'(A,B):-break_eggs(A,C),recipe(C,B).
'$1'(A,B):-break_eggs(A,C),whisk_eggs(C,B).
'$1'(A,B):-fry_eggs(A,C),season(C,B).
'$1'(A,B):-heat_oil(A,C),'$1'(C,B).
'$1'(A,B):-heat_oil(A,C),'$2'(C,B).
'$1'(A,B):-heat_oil(A,C),break_eggs(C,B).
'$1'(A,B):-heat_oil(A,C),fry_eggs(C,B).
'$1'(A,B):-heat_oil(A,C),recipe(C,B).
'$1'(A,B):-heat_oil(A,C),whisk_eggs(C,B).
'$1'(A,B):-recipe(A,C),'$1'(C,B).
'$1'(A,B):-recipe(A,C),'$2'(C,B).
'$1'(A,B):-recipe(A,C),fry_eggs(C,B).
'$1'(A,B):-recipe(A,C),heat_oil(C,B).
'$1'(A,B):-recipe(A,C),recipe(C,B).
'$1'(A,B):-recipe(A,C),season(C,B).
'$1'(A,B):-recipe(A,C),whisk_eggs(C,B).
'$1'(A,B):-whisk_eggs(A,C),'$1'(C,B).
'$1'(A,B):-whisk_eggs(A,C),'$2'(C,B).
'$1'(A,B):-whisk_eggs(A,C),fry_eggs(C,B).
'$1'(A,B):-whisk_eggs(A,C),heat_oil(C,B).
'$1'(A,B):-whisk_eggs(A,C),recipe(C,B).
'$2'(A,B):-'$1'(A,C),'$1'(C,B).
'$2'(A,B):-'$1'(A,C),'$2'(C,B).
'$2'(A,B):-'$1'(A,C),fry_eggs(C,B).
'$2'(A,B):-'$1'(A,C),heat_oil(C,B).
'$2'(A,B):-'$1'(A,C),recipe(C,B).
'$2'(A,B):-'$1'(A,C),season(C,B).
'$2'(A,B):-'$1'(A,C),whisk_eggs(C,B).
'$2'(A,B):-'$2'(A,C),'$1'(C,B).
'$2'(A,B):-'$2'(A,C),'$2'(C,B).
'$2'(A,B):-'$2'(A,C),fry_eggs(C,B).
'$2'(A,B):-'$2'(A,C),heat_oil(C,B).
'$2'(A,B):-'$2'(A,C),recipe(C,B).
'$2'(A,B):-'$2'(A,C),season(C,B).
'$2'(A,B):-'$2'(A,C),whisk_eggs(C,B).
'$2'(A,B):-break_eggs(A,C),'$1'(C,B).
'$2'(A,B):-break_eggs(A,C),'$2'(C,B).
'$2'(A,B):-break_eggs(A,C),heat_oil(C,B).
'$2'(A,B):-break_eggs(A,C),recipe(C,B).
'$2'(A,B):-break_eggs(A,C),whisk_eggs(C,B).
'$2'(A,B):-fry_eggs(A,C),season(C,B).
'$2'(A,B):-heat_oil(A,C),'$1'(C,B).
'$2'(A,B):-heat_oil(A,C),'$2'(C,B).
'$2'(A,B):-heat_oil(A,C),break_eggs(C,B).
'$2'(A,B):-heat_oil(A,C),fry_eggs(C,B).
'$2'(A,B):-heat_oil(A,C),recipe(C,B).
'$2'(A,B):-heat_oil(A,C),whisk_eggs(C,B).
'$2'(A,B):-recipe(A,C),'$1'(C,B).
'$2'(A,B):-recipe(A,C),'$2'(C,B).
'$2'(A,B):-recipe(A,C),fry_eggs(C,B).
'$2'(A,B):-recipe(A,C),heat_oil(C,B).
'$2'(A,B):-recipe(A,C),recipe(C,B).
'$2'(A,B):-recipe(A,C),season(C,B).
'$2'(A,B):-recipe(A,C),whisk_eggs(C,B).
'$2'(A,B):-whisk_eggs(A,C),'$1'(C,B).
'$2'(A,B):-whisk_eggs(A,C),'$2'(C,B).
'$2'(A,B):-whisk_eggs(A,C),fry_eggs(C,B).
'$2'(A,B):-whisk_eggs(A,C),heat_oil(C,B).
'$2'(A,B):-whisk_eggs(A,C),recipe(C,B).
recipe(A,B):-'$1'(A,C),'$1'(C,B).
recipe(A,B):-'$1'(A,C),'$2'(C,B).
recipe(A,B):-'$1'(A,C),fry_eggs(C,B).
recipe(A,B):-'$1'(A,C),heat_oil(C,B).
recipe(A,B):-'$1'(A,C),recipe(C,B).
recipe(A,B):-'$1'(A,C),season(C,B).
recipe(A,B):-'$1'(A,C),whisk_eggs(C,B).
recipe(A,B):-'$2'(A,C),'$1'(C,B).
recipe(A,B):-'$2'(A,C),'$2'(C,B).
recipe(A,B):-'$2'(A,C),fry_eggs(C,B).
recipe(A,B):-'$2'(A,C),heat_oil(C,B).
recipe(A,B):-'$2'(A,C),recipe(C,B).
recipe(A,B):-'$2'(A,C),season(C,B).
recipe(A,B):-'$2'(A,C),whisk_eggs(C,B).
recipe(A,B):-break_eggs(A,C),'$1'(C,B).
recipe(A,B):-break_eggs(A,C),'$2'(C,B).
recipe(A,B):-break_eggs(A,C),heat_oil(C,B).
recipe(A,B):-break_eggs(A,C),recipe(C,B).
recipe(A,B):-break_eggs(A,C),whisk_eggs(C,B).
recipe(A,B):-fry_eggs(A,C),season(C,B).
recipe(A,B):-heat_oil(A,C),'$1'(C,B).
recipe(A,B):-heat_oil(A,C),'$2'(C,B).
recipe(A,B):-heat_oil(A,C),break_eggs(C,B).
recipe(A,B):-heat_oil(A,C),fry_eggs(C,B).
recipe(A,B):-heat_oil(A,C),recipe(C,B).
recipe(A,B):-heat_oil(A,C),whisk_eggs(C,B).
recipe(A,B):-recipe(A,C),'$1'(C,B).
recipe(A,B):-recipe(A,C),'$2'(C,B).
recipe(A,B):-recipe(A,C),fry_eggs(C,B).
recipe(A,B):-recipe(A,C),heat_oil(C,B).
recipe(A,B):-recipe(A,C),recipe(C,B).
recipe(A,B):-recipe(A,C),season(C,B).
recipe(A,B):-recipe(A,C),whisk_eggs(C,B).
recipe(A,B):-whisk_eggs(A,C),'$1'(C,B).
recipe(A,B):-whisk_eggs(A,C),'$2'(C,B).
recipe(A,B):-whisk_eggs(A,C),fry_eggs(C,B).
recipe(A,B):-whisk_eggs(A,C),heat_oil(C,B).
recipe(A,B):-whisk_eggs(A,C),recipe(C,B).
N = 115.
==

Note the use of time/1 and length/2 to keep track of the time it takes
to learn the hypothesis above and its size.

"N = 115" is the count of clauses in the learned hypothesis. See what I
meant when I said it's large?

If your system is taking considerably longer to complete the learning
attempt than the time listed in the output of time/1 above, you should
probably examine your configuration and the elements of the MIL problem
for recipe/2 to make sure they match the ones listed in the previous
section.

The experiment listed above was run on a seven-year old laptop with an
i7 processor clocked at 2.6 GHz and 16 GB of RAM. You don't need a
supercomputer to repeat it!


__5. Eyballing the results.__

First, find some eydrops.

Ready? Now we can look more closely at the output of the learning
attempt listed above. The first thing we observe is that the learned
hypothesis is an unholly mess. To begin with, it is huge. Who writes one
predicate with 115 clauses? Then, it is full of invented predicates with
meaningless symbols, '$1' and '$2', that don't help us at all to
understand what the heck the learned program is doing. Is it right? Is
it wrong? Who can tell?

You know how people say that one of the advantages of ILP compared to
"black boxes" like deep learning is the inherent explainability of logic
programs? Well, sometimes you get explainable programs. And some times
you get a pile.


We next observe that the single example of recipe/2 is output together
with the learned program, as its first clause:

==
recipe([egg_whisk,eggs,frying_pan,olive_oil,pepper,salt],[omelette]).
==

This is normally a bad sign. Such "atomic residue" means that some
positive examples are not "covered" by the learned program. In this case
though it is a limitation of the implementation of Plotkin's program
reduction in lib/program_reduction, which is very bad at dealing with
lists. This is a known bug in other words.

Plotkin's reduction is a big bottleneck in the cost of learning. Since
it doesn't work well with the lists in the recipe/2 background
knowledge, we can turn it off by setting the configuration option
reduction/1 to the value "none". This can be done in the configuration,
or with a call to the auxiliary predicate set_configuration_option/2,
added as a directive to this experiment file, and executed when the file
is loaded:

==
:- auxiliaries:set_configuration_option(reduction, [none]).
==

This directive is already declared in this experiment file, but it is
commented out. Scroll down after the end of this comment section and
uncomment it.

Now make a new learning attempt. This time our learning query will not
print out the clauses of the learned program:

==
?- time(learn(recipe/2,_Ps)), length(_Ps,N).
% 77,922,935 inferences, 26.437 CPU in 26.589 seconds (99% CPU, 2947440 Lips)
N = 114.
==

Learning time is slightly improved, but more important is to note the
reduction in the number of logical inferences, from 81,750,748 in our
initial learning attempt, above, to 77,922,935. We also now have one
less clause: when reduction(none) is set, no positive examples are
output along with the learned program.


Going back to the initial program, how can we tell that the learned
program is correct, or even what it is doing? That thing is nigh-on
unreadable! Looking more carefully and ceasing our annoying complaints
we can remark that the two invented predicates, '$1' and '$2' are almost
identical in structure, with the difference that they each call the
other. Their literals that don't have invented predicate symbols appear
to be different combinations of preparation methods defined in the
background ("heat oil", "break eggs" etc).

Looking at the clauses with the target predicate's symbol, recipe/2, in
their head, at the bottom of the listing, we can start to feel confident
that the learned program seems to be taking alternative paths to the
same destintation, making an omelette. In turn this means that there's
no point in trying to reduce the program any further. Besides the
limitatiosn of the implementation of Plotkin's program reduction, if
each invented predicate is correctly defined, we won't find any logical
redundancies.

If we want to help Louise learn a smaller program, we have to use
another trick: metarule constraints.


__6. Applying constraints.__

Our learned program includes multiple left-recursive clauses:

==
'$1'(A,B):-'$1'(A,C),'$1'(C,B).
'$1'(A,B):-'$1'(A,C),'$2'(C,B).
'$1'(A,B):-'$1'(A,C),fry_eggs(C,B).
'$1'(A,B):-'$1'(A,C),heat_oil(C,B).
'$1'(A,B):-'$1'(A,C),recipe(C,B).
'$1'(A,B):-'$1'(A,C),season(C,B).
'$1'(A,B):-'$1'(A,C),whisk_eggs(C,B)

'$2'(A,B):-'$2'(A,C),'$1'(C,B).
'$2'(A,B):-'$2'(A,C),'$2'(C,B).
'$2'(A,B):-'$2'(A,C),fry_eggs(C,B).
'$2'(A,B):-'$2'(A,C),heat_oil(C,B).
'$2'(A,B):-'$2'(A,C),recipe(C,B).
'$2'(A,B):-'$2'(A,C),season(C,B).
'$2'(A,B):-'$2'(A,C),whisk_eggs(C,B).

recipe(A,B):-recipe(A,C),'$1'(C,B).
recipe(A,B):-recipe(A,C),'$2'(C,B).
recipe(A,B):-recipe(A,C),fry_eggs(C,B).
recipe(A,B):-recipe(A,C),heat_oil(C,B).
recipe(A,B):-recipe(A,C),recipe(C,B).
recipe(A,B):-recipe(A,C),season(C,B).
recipe(A,B):-recipe(A,C),whisk_eggs(C,B).
==

Prolog programmers are generally taught to avoid left recursion because
its execution with Prolog's standard depth-first-search-based evaluation
can "go infinite".

Left recursion is not a problem in Louise, since its "Vanilla TPC"
meta-interpreter, used to learn recursion and perform predicate
invention, uses tabling to ensure termination when learning
left-recursions. On the other hand, we want to reduce the learned
program so it makes sense to remove left-recursions that can make it
harder to run.

The following constraint will eliminate left-recursions:

==
configuration:metarule_constraints(m(chain,P,P,_),fail).
==

Constraints are checked both during the inductive proof, and once the
proof is complete (just in case), so applying the constraint should have
two effects. First, it will reduce the size of the learned program by
eliminating from it the 21 left recursive clauses listed above. Second,
it will eliminate the _proof branches_ inducing those clauses and so
slightly improve running time.

The constraint above is already added to the source code of this
experiment file, albiet commented-out. Look for it towards the end of
the file, right after this structured comment. Uncomment it.

Now make a new learning attempt:

==
?- time(learn(recipe/2,_Ps)), print_clauses(_Ps), length(_Ps,N).
% 66,078,834 inferences, 20.391 CPU in 20.527 seconds (99% CPU, 3240648 Lips)
'$1'(A,B):-'$2'(A,C),'$1'(C,B).
'$1'(A,B):-'$2'(A,C),'$2'(C,B).
'$1'(A,B):-'$2'(A,C),fry_eggs(C,B).
'$1'(A,B):-'$2'(A,C),heat_oil(C,B).
'$1'(A,B):-'$2'(A,C),recipe(C,B).
'$1'(A,B):-'$2'(A,C),season(C,B).
'$1'(A,B):-'$2'(A,C),whisk_eggs(C,B).
'$1'(A,B):-break_eggs(A,C),'$1'(C,B).
'$1'(A,B):-break_eggs(A,C),'$2'(C,B).
'$1'(A,B):-break_eggs(A,C),heat_oil(C,B).
'$1'(A,B):-break_eggs(A,C),recipe(C,B).
'$1'(A,B):-break_eggs(A,C),whisk_eggs(C,B).
'$1'(A,B):-fry_eggs(A,C),season(C,B).
'$1'(A,B):-heat_oil(A,C),'$1'(C,B).
'$1'(A,B):-heat_oil(A,C),'$2'(C,B).
'$1'(A,B):-heat_oil(A,C),break_eggs(C,B).
'$1'(A,B):-heat_oil(A,C),fry_eggs(C,B).
'$1'(A,B):-heat_oil(A,C),recipe(C,B).
'$1'(A,B):-heat_oil(A,C),whisk_eggs(C,B).
'$1'(A,B):-recipe(A,C),'$1'(C,B).
'$1'(A,B):-recipe(A,C),'$2'(C,B).
'$1'(A,B):-recipe(A,C),fry_eggs(C,B).
'$1'(A,B):-recipe(A,C),heat_oil(C,B).
'$1'(A,B):-recipe(A,C),recipe(C,B).
'$1'(A,B):-recipe(A,C),season(C,B).
'$1'(A,B):-recipe(A,C),whisk_eggs(C,B).
'$1'(A,B):-whisk_eggs(A,C),'$1'(C,B).
'$1'(A,B):-whisk_eggs(A,C),'$2'(C,B).
'$1'(A,B):-whisk_eggs(A,C),fry_eggs(C,B).
'$1'(A,B):-whisk_eggs(A,C),heat_oil(C,B).
'$1'(A,B):-whisk_eggs(A,C),recipe(C,B).
'$2'(A,B):-'$1'(A,C),'$1'(C,B).
'$2'(A,B):-'$1'(A,C),'$2'(C,B).
'$2'(A,B):-'$1'(A,C),fry_eggs(C,B).
'$2'(A,B):-'$1'(A,C),heat_oil(C,B).
'$2'(A,B):-'$1'(A,C),recipe(C,B).
'$2'(A,B):-'$1'(A,C),season(C,B).
'$2'(A,B):-'$1'(A,C),whisk_eggs(C,B).
'$2'(A,B):-break_eggs(A,C),'$1'(C,B).
'$2'(A,B):-break_eggs(A,C),'$2'(C,B).
'$2'(A,B):-break_eggs(A,C),heat_oil(C,B).
'$2'(A,B):-break_eggs(A,C),recipe(C,B).
'$2'(A,B):-break_eggs(A,C),whisk_eggs(C,B).
'$2'(A,B):-fry_eggs(A,C),season(C,B).
'$2'(A,B):-heat_oil(A,C),'$1'(C,B).
'$2'(A,B):-heat_oil(A,C),'$2'(C,B).
'$2'(A,B):-heat_oil(A,C),break_eggs(C,B).
'$2'(A,B):-heat_oil(A,C),fry_eggs(C,B).
'$2'(A,B):-heat_oil(A,C),recipe(C,B).
'$2'(A,B):-heat_oil(A,C),whisk_eggs(C,B).
'$2'(A,B):-recipe(A,C),'$1'(C,B).
'$2'(A,B):-recipe(A,C),'$2'(C,B).
'$2'(A,B):-recipe(A,C),fry_eggs(C,B).
'$2'(A,B):-recipe(A,C),heat_oil(C,B).
'$2'(A,B):-recipe(A,C),recipe(C,B).
'$2'(A,B):-recipe(A,C),season(C,B).
'$2'(A,B):-recipe(A,C),whisk_eggs(C,B).
'$2'(A,B):-whisk_eggs(A,C),'$1'(C,B).
'$2'(A,B):-whisk_eggs(A,C),'$2'(C,B).
'$2'(A,B):-whisk_eggs(A,C),fry_eggs(C,B).
'$2'(A,B):-whisk_eggs(A,C),heat_oil(C,B).
'$2'(A,B):-whisk_eggs(A,C),recipe(C,B).
recipe(A,B):-'$1'(A,C),'$1'(C,B).
recipe(A,B):-'$1'(A,C),'$2'(C,B).
recipe(A,B):-'$1'(A,C),fry_eggs(C,B).
recipe(A,B):-'$1'(A,C),heat_oil(C,B).
recipe(A,B):-'$1'(A,C),recipe(C,B).
recipe(A,B):-'$1'(A,C),season(C,B).
recipe(A,B):-'$1'(A,C),whisk_eggs(C,B).
recipe(A,B):-'$2'(A,C),'$1'(C,B).
recipe(A,B):-'$2'(A,C),'$2'(C,B).
recipe(A,B):-'$2'(A,C),fry_eggs(C,B).
recipe(A,B):-'$2'(A,C),heat_oil(C,B).
recipe(A,B):-'$2'(A,C),recipe(C,B).
recipe(A,B):-'$2'(A,C),season(C,B).
recipe(A,B):-'$2'(A,C),whisk_eggs(C,B).
recipe(A,B):-break_eggs(A,C),'$1'(C,B).
recipe(A,B):-break_eggs(A,C),'$2'(C,B).
recipe(A,B):-break_eggs(A,C),heat_oil(C,B).
recipe(A,B):-break_eggs(A,C),recipe(C,B).
recipe(A,B):-break_eggs(A,C),whisk_eggs(C,B).
recipe(A,B):-fry_eggs(A,C),season(C,B).
recipe(A,B):-heat_oil(A,C),'$1'(C,B).
recipe(A,B):-heat_oil(A,C),'$2'(C,B).
recipe(A,B):-heat_oil(A,C),break_eggs(C,B).
recipe(A,B):-heat_oil(A,C),fry_eggs(C,B).
recipe(A,B):-heat_oil(A,C),recipe(C,B).
recipe(A,B):-heat_oil(A,C),whisk_eggs(C,B).
recipe(A,B):-whisk_eggs(A,C),'$1'(C,B).
recipe(A,B):-whisk_eggs(A,C),'$2'(C,B).
recipe(A,B):-whisk_eggs(A,C),fry_eggs(C,B).
recipe(A,B):-whisk_eggs(A,C),heat_oil(C,B).
recipe(A,B):-whisk_eggs(A,C),recipe(C,B).
N = 93.
==

Note the reduced side of the hypothesis. 114 - 21 = 93. That's the
clauses remaining after removing left-recursions.

The training time has also improved slightly but it's more informative
to look at the logical inferences for the two learning attempts:
77,922,935 with left recursions, 66,078,834 without. We shaved a bit of
complexity off our problem!

We can do better than that. Eyballing our learned problem again, we can
see several more recursive clauses with recipe/2 as the symbol of their
head literal. If we look at our background knowledge we can see that it
only includes five predicates that represent cooking procedures, such as
breaking egggs or heating oil. How many times does our program need to
recurse over those operations to accomplish its goal? Probably not a
one! Let's remove those tail-recursions also.

As before, find and uncomment the following constraint towards the end
of this file:

==
configuration:metarule_constraints(m(chain,P,_,P),fail).
==

Now make another learning attempt to see what we have achieved:

==
?- time(learn(recipe/2,_Ps)), print_clauses(_Ps), length(_Ps,N).
% 34,641,859 inferences, 11.531 CPU in 11.536 seconds (100% CPU, 3004172 Lips)
'$1'(A,B):-'$2'(A,C),'$2'(C,B).
'$1'(A,B):-'$2'(A,C),fry_eggs(C,B).
'$1'(A,B):-'$2'(A,C),heat_oil(C,B).
'$1'(A,B):-'$2'(A,C),recipe(C,B).
'$1'(A,B):-'$2'(A,C),season(C,B).
'$1'(A,B):-'$2'(A,C),whisk_eggs(C,B).
'$1'(A,B):-break_eggs(A,C),'$2'(C,B).
'$1'(A,B):-break_eggs(A,C),heat_oil(C,B).
'$1'(A,B):-break_eggs(A,C),recipe(C,B).
'$1'(A,B):-break_eggs(A,C),whisk_eggs(C,B).
'$1'(A,B):-fry_eggs(A,C),season(C,B).
'$1'(A,B):-heat_oil(A,C),'$2'(C,B).
'$1'(A,B):-heat_oil(A,C),break_eggs(C,B).
'$1'(A,B):-heat_oil(A,C),fry_eggs(C,B).
'$1'(A,B):-heat_oil(A,C),recipe(C,B).
'$1'(A,B):-heat_oil(A,C),whisk_eggs(C,B).
'$1'(A,B):-recipe(A,C),'$2'(C,B).
'$1'(A,B):-recipe(A,C),fry_eggs(C,B).
'$1'(A,B):-recipe(A,C),heat_oil(C,B).
'$1'(A,B):-recipe(A,C),recipe(C,B).
'$1'(A,B):-recipe(A,C),season(C,B).
'$1'(A,B):-recipe(A,C),whisk_eggs(C,B).
'$1'(A,B):-whisk_eggs(A,C),'$2'(C,B).
'$1'(A,B):-whisk_eggs(A,C),fry_eggs(C,B).
'$1'(A,B):-whisk_eggs(A,C),heat_oil(C,B).
'$1'(A,B):-whisk_eggs(A,C),recipe(C,B).
'$2'(A,B):-'$1'(A,C),'$1'(C,B).
'$2'(A,B):-'$1'(A,C),fry_eggs(C,B).
'$2'(A,B):-'$1'(A,C),heat_oil(C,B).
'$2'(A,B):-'$1'(A,C),recipe(C,B).
'$2'(A,B):-'$1'(A,C),season(C,B).
'$2'(A,B):-'$1'(A,C),whisk_eggs(C,B).
'$2'(A,B):-break_eggs(A,C),'$1'(C,B).
'$2'(A,B):-break_eggs(A,C),heat_oil(C,B).
'$2'(A,B):-break_eggs(A,C),recipe(C,B).
'$2'(A,B):-break_eggs(A,C),whisk_eggs(C,B).
'$2'(A,B):-fry_eggs(A,C),season(C,B).
'$2'(A,B):-heat_oil(A,C),'$1'(C,B).
'$2'(A,B):-heat_oil(A,C),break_eggs(C,B).
'$2'(A,B):-heat_oil(A,C),fry_eggs(C,B).
'$2'(A,B):-heat_oil(A,C),recipe(C,B).
'$2'(A,B):-heat_oil(A,C),whisk_eggs(C,B).
'$2'(A,B):-recipe(A,C),'$1'(C,B).
'$2'(A,B):-recipe(A,C),fry_eggs(C,B).
'$2'(A,B):-recipe(A,C),heat_oil(C,B).
'$2'(A,B):-recipe(A,C),recipe(C,B).
'$2'(A,B):-recipe(A,C),season(C,B).
'$2'(A,B):-recipe(A,C),whisk_eggs(C,B).
'$2'(A,B):-whisk_eggs(A,C),'$1'(C,B).
'$2'(A,B):-whisk_eggs(A,C),fry_eggs(C,B).
'$2'(A,B):-whisk_eggs(A,C),heat_oil(C,B).
'$2'(A,B):-whisk_eggs(A,C),recipe(C,B).
recipe(A,B):-'$1'(A,C),'$1'(C,B).
recipe(A,B):-'$1'(A,C),'$2'(C,B).
recipe(A,B):-'$1'(A,C),fry_eggs(C,B).
recipe(A,B):-'$1'(A,C),heat_oil(C,B).
recipe(A,B):-'$1'(A,C),season(C,B).
recipe(A,B):-'$1'(A,C),whisk_eggs(C,B).
recipe(A,B):-'$2'(A,C),'$1'(C,B).
recipe(A,B):-'$2'(A,C),'$2'(C,B).
recipe(A,B):-'$2'(A,C),fry_eggs(C,B).
recipe(A,B):-'$2'(A,C),heat_oil(C,B).
recipe(A,B):-'$2'(A,C),season(C,B).
recipe(A,B):-'$2'(A,C),whisk_eggs(C,B).
recipe(A,B):-break_eggs(A,C),'$1'(C,B).
recipe(A,B):-break_eggs(A,C),'$2'(C,B).
recipe(A,B):-break_eggs(A,C),heat_oil(C,B).
recipe(A,B):-break_eggs(A,C),whisk_eggs(C,B).
recipe(A,B):-fry_eggs(A,C),season(C,B).
recipe(A,B):-heat_oil(A,C),'$1'(C,B).
recipe(A,B):-heat_oil(A,C),'$2'(C,B).
recipe(A,B):-heat_oil(A,C),break_eggs(C,B).
recipe(A,B):-heat_oil(A,C),fry_eggs(C,B).
recipe(A,B):-heat_oil(A,C),whisk_eggs(C,B).
recipe(A,B):-whisk_eggs(A,C),'$1'(C,B).
recipe(A,B):-whisk_eggs(A,C),'$2'(C,B).
recipe(A,B):-whisk_eggs(A,C),fry_eggs(C,B).
recipe(A,B):-whisk_eggs(A,C),heat_oil(C,B).
N = 78.
==

We've reduced the program from 93 clauses to 78 and reduced the number
of inferences needed to learn it almost by half, from 66,078,834 to
34,641,859.

We can do even better than that. In the new program, note that there are
still several clauses that lead to recursion, for example the following
clauses will all cause recipe/2 to be called after '$1', forming a
recursive loop spanning two (or more) clauses:

==
'$1'(A,B):-recipe(A,C),'$2'(C,B).
'$1'(A,B):-recipe(A,C),fry_eggs(C,B).
'$1'(A,B):-recipe(A,C),heat_oil(C,B).
'$1'(A,B):-recipe(A,C),recipe(C,B).
'$1'(A,B):-recipe(A,C),season(C,B).
'$1'(A,B):-recipe(A,C),whisk_eggs(C,B).
==

Similarly, clauses like the following will initiate a recursive loop
when their last literal is reached:

==
'$1'(A,B):-'$2'(A,C),recipe(C,B).
'$1'(A,B):-break_eggs(A,C),recipe(C,B).
'$1'(A,B):-heat_oil(A,C),recipe(C,B).
'$1'(A,B):-whisk_eggs(A,C),recipe(C,B).
==

The following constraints will remove both of those types of long-range
recursive clauses:

==
configuration:metarule_constraints(m(chain, _, _, recipe), fail).
configuration:metarule_constraints(m(chain, recipe, _, _), fail).
==

Uncomment them as before and make a new learning attempt:

==
?- time(learn(recipe/2,_Ps)), print_clauses(_Ps), length(_Ps,N).
% 15,330,321 inferences, 5.906 CPU in 5.907 seconds (100% CPU, 2595610 Lips)
'$1'(A,B):-'$2'(A,C),'$2'(C,B).
'$1'(A,B):-'$2'(A,C),fry_eggs(C,B).
'$1'(A,B):-'$2'(A,C),heat_oil(C,B).
'$1'(A,B):-'$2'(A,C),season(C,B).
'$1'(A,B):-'$2'(A,C),whisk_eggs(C,B).
'$1'(A,B):-break_eggs(A,C),'$2'(C,B).
'$1'(A,B):-break_eggs(A,C),heat_oil(C,B).
'$1'(A,B):-break_eggs(A,C),whisk_eggs(C,B).
'$1'(A,B):-fry_eggs(A,C),season(C,B).
'$1'(A,B):-heat_oil(A,C),'$2'(C,B).
'$1'(A,B):-heat_oil(A,C),break_eggs(C,B).
'$1'(A,B):-heat_oil(A,C),fry_eggs(C,B).
'$1'(A,B):-heat_oil(A,C),whisk_eggs(C,B).
'$1'(A,B):-whisk_eggs(A,C),'$2'(C,B).
'$1'(A,B):-whisk_eggs(A,C),fry_eggs(C,B).
'$1'(A,B):-whisk_eggs(A,C),heat_oil(C,B).
'$2'(A,B):-'$1'(A,C),'$1'(C,B).
'$2'(A,B):-'$1'(A,C),fry_eggs(C,B).
'$2'(A,B):-'$1'(A,C),heat_oil(C,B).
'$2'(A,B):-'$1'(A,C),season(C,B).
'$2'(A,B):-'$1'(A,C),whisk_eggs(C,B).
'$2'(A,B):-break_eggs(A,C),'$1'(C,B).
'$2'(A,B):-break_eggs(A,C),heat_oil(C,B).
'$2'(A,B):-break_eggs(A,C),whisk_eggs(C,B).
'$2'(A,B):-fry_eggs(A,C),season(C,B).
'$2'(A,B):-heat_oil(A,C),'$1'(C,B).
'$2'(A,B):-heat_oil(A,C),break_eggs(C,B).
'$2'(A,B):-heat_oil(A,C),fry_eggs(C,B).
'$2'(A,B):-heat_oil(A,C),whisk_eggs(C,B).
'$2'(A,B):-whisk_eggs(A,C),'$1'(C,B).
'$2'(A,B):-whisk_eggs(A,C),fry_eggs(C,B).
'$2'(A,B):-whisk_eggs(A,C),heat_oil(C,B).
recipe(A,B):-'$1'(A,C),'$1'(C,B).
recipe(A,B):-'$1'(A,C),'$2'(C,B).
recipe(A,B):-'$1'(A,C),season(C,B).
recipe(A,B):-'$2'(A,C),'$1'(C,B).
recipe(A,B):-'$2'(A,C),'$2'(C,B).
recipe(A,B):-'$2'(A,C),season(C,B).
recipe(A,B):-break_eggs(A,C),'$1'(C,B).
recipe(A,B):-break_eggs(A,C),'$2'(C,B).
recipe(A,B):-heat_oil(A,C),'$1'(C,B).
recipe(A,B):-heat_oil(A,C),'$2'(C,B).
N = 42.
==

We've now drastically reduced both the size of the learned program and
the time needed to learn it.

Now that our program is smaller it's easier to see more long-range
recursions that don't look like they're necessary. For example, all the
following clauses will cause the program to loop around the two
invented predicates:

==
'$1'(A,B):-'$2'(A,C),'$2'(C,B).
'$1'(A,B):-'$2'(A,C),fry_eggs(C,B).
'$1'(A,B):-'$2'(A,C),heat_oil(C,B).
'$1'(A,B):-'$2'(A,C),season(C,B).
'$1'(A,B):-'$2'(A,C),whisk_eggs(C,B).

'$2'(A,B):-'$1'(A,C),'$1'(C,B).
'$2'(A,B):-'$1'(A,C),fry_eggs(C,B).
'$2'(A,B):-'$1'(A,C),heat_oil(C,B).
'$2'(A,B):-'$1'(A,C),season(C,B).
'$2'(A,B):-'$1'(A,C),whisk_eggs(C,B).
==

This looping is as unnecessary ars looping around recipe/2 itself. Let's
remove it. Uncomment the following constraint:

==
configuration:metarule_constraints(m(chain, _, Q, _), fail):-
	   memberchk(Q, [recipe, '$1', '$2']).
==

And make a new learning attempt:

==
?- time(learn(recipe/2,_Ps)), print_clauses(_Ps), length(_Ps,N).
% 8,073,185 inferences, 3.594 CPU in 3.613 seconds (99% CPU, 2246451 Lips)
'$1'(A,B):-break_eggs(A,C),'$2'(C,B).
'$1'(A,B):-fry_eggs(A,C),season(C,B).
'$1'(A,B):-heat_oil(A,C),'$2'(C,B).
'$1'(A,B):-whisk_eggs(A,C),'$2'(C,B).
'$2'(A,B):-break_eggs(A,C),'$1'(C,B).
'$2'(A,B):-fry_eggs(A,C),season(C,B).
'$2'(A,B):-heat_oil(A,C),'$1'(C,B).
'$2'(A,B):-whisk_eggs(A,C),'$1'(C,B).
recipe(A,B):-break_eggs(A,C),'$1'(C,B).
recipe(A,B):-break_eggs(A,C),'$2'(C,B).
recipe(A,B):-heat_oil(A,C),'$1'(C,B).
recipe(A,B):-heat_oil(A,C),'$2'(C,B).
N = 12.
==

Better and better! We're down to 12 clauses and 8,073,185 logical
inferences. At this point it only takes a handful of seconds to learn
the program.

With such a small program it is also clear to see that half the clauses
are duplicates of the other half, except with different invented
predicates. Here's one half:

==
'$2'(A,B):-break_eggs(A,C),'$1'(C,B).
'$2'(A,B):-fry_eggs(A,C),season(C,B).
'$2'(A,B):-heat_oil(A,C),'$1'(C,B).
'$2'(A,B):-whisk_eggs(A,C),'$1'(C,B).
recipe(A,B):-break_eggs(A,C),'$2'(C,B).
recipe(A,B):-heat_oil(A,C),'$2'(C,B).
==

And here's the other half:

==
'$1'(A,B):-break_eggs(A,C),'$2'(C,B).
'$1'(A,B):-fry_eggs(A,C),season(C,B).
'$1'(A,B):-heat_oil(A,C),'$2'(C,B).
'$1'(A,B):-whisk_eggs(A,C),'$2'(C,B).
recipe(A,B):-break_eggs(A,C),'$1'(C,B).
recipe(A,B):-heat_oil(A,C),'$1'(C,B).
==

How can we remove this duplication? Let's try to choose one half of our
set of clauses and cut out the other half. The following constraint will
prune the proof branches for clauses that have recipe/2 in their head
literal and '$2' in their tail:

==
configuration:metarule_constraints(m(chain, recipe, _, '$2'), fail).
==

Uncomment it as before and make another learning attempt to see the
result:

==
?- time(learn(recipe/2,_Ps)), print_clauses(_Ps), length(_Ps,N).
% 5,648,881 inferences, 2.891 CPU in 2.901 seconds (100% CPU, 1954207 Lips)
'$1'(A,B):-break_eggs(A,C),'$2'(C,B).
'$1'(A,B):-fry_eggs(A,C),season(C,B).
'$1'(A,B):-heat_oil(A,C),'$2'(C,B).
'$1'(A,B):-whisk_eggs(A,C),'$2'(C,B).
'$2'(A,B):-heat_oil(A,C),'$1'(C,B).
'$2'(A,B):-whisk_eggs(A,C),'$1'(C,B).
recipe(A,B):-break_eggs(A,C),'$1'(C,B).
recipe(A,B):-heat_oil(A,C),'$1'(C,B).
N = 8.
==

It's not that easy to see by the naked eye but some experimentation
would reveal that there is no way to reduce the learned program any
further, and still entail the single example. While there is some
repetition still (but no more recursion) the clauses of the learned
program "cover" all possible paths to making an omelette.

In order to see this more clearly, we can unfold our program to remove
invented predicates. This is described in the next section.


__7. Unfolding the reduced program__

Louise's unfolding facility is controlled by the configuration option
unfold_invented/1. Setting the value of this option to "true" tells
Louise to apply unfolding at the end of learning.

You can set unfold_invented(true) in the configuration file, or with a
call to the auxiliary predicate set_configuration_option/2. This can be
added to this experiment file as a directive, to be called when you
reload the file:

==
:- auxiliaries:set_configuration_option(unfold_invented, [true]).
==

As with constraints, this directive is already declared near the end of
the file, but commented-out. Comment it back in and make a new learning
attempt.

Remember to call make/0 first to reload the experiment file in memory!

You should see output like the following:

==
?- time(learn(recipe/2,_Ps)), print_clauses(_Ps), length(_Ps,N).
% 5,677,459 inferences, 2.922 CPU in 2.938 seconds (99% CPU, 1943088 Lips)
recipe(A,B):-break_eggs(A,C),heat_oil(C,D),whisk_eggs(D,E),fry_eggs(E,F),season(F,B).
recipe(A,B):-break_eggs(A,C),whisk_eggs(C,D),heat_oil(D,E),fry_eggs(E,F),season(F,B).
recipe(A,B):-heat_oil(A,C),break_eggs(C,D),whisk_eggs(D,E),fry_eggs(E,F),season(F,B).
N = 3.
==

The unfolding adds a bit of overhead in terms of increased logical
inferences. It's worth it! Finally, we can see that our
constraint-reduced, 8-clause program with two invented predicates,
learned in the last step, amounts to three variants of a recipe to make
an omelette. Each variant takes the same steps, and in fact all three
end with the same steps (frying eggs and then seasoning them) but there
are a few different options about the order in which intermediate steps
are taken.

For example, we can see in our program that we need to break eggs, of
course, and we need to do that before we whisk the broken eggs. But we
can heat the oil before or after either of those two steps. We must
always fry the egggs and season the omelette as the final two steps of
the procedure. Obviously, all this is true in the context of the
definitions of the background predicates break_eggs/2, heat_oil/2 etc,
that require specific inputs to succeed. For example, the break_eggs/2
predicate expects the atom "eggs" to be in its input list, etc.

Note also that the unfolding has resulted in three clauses, each with
six literals. This is the result of unfolding the instances of the Chain
metarule in our folded program. Unfolding, i.e. resolving, two instances
of Chain with each other yields a clause with four literals. To arrive
at a clause with six literals, Louise's unfolding code resolved 4
clauses of chain.

For example, to produce the first clause above:

==
recipe(A,B):-break_eggs(A,C),heat_oil(C,D),whisk_eggs(D,E),fry_eggs(E,F),season(F,B).
==

The following clauses were unfolded by resolving with each other:

==
recipe(A,B):-break_eggs(A,C),'$1'(C,B).
'$1'(A,B):-heat_oil(A,C),'$2'(C,B).
'$2'(A,B):-whisk_eggs(A,C),'$1'(C,B).
'$1'(A,B):-fry_eggs(A,C),season(C,B).
==

You can see how there is a resolution-proof branch that unifies the last
literal of each clause with the one below it. Unfolding collecst the
literals of the resolvents constructed during the proof and "stitches"
them all together to form a longer clause.


__8. Dynamically setting configuration options__

We saw above the use of the auxiliary predicate
set_configuration_option/2 to dynamically manipulate configuration
options.

For example, in the source code below, the configuration option
max_invented/1 is set dynamically using set_configuration_option/2 as a
directive:

==
:- auxiliaries:set_configuration_option(max_invented, [2]).
==

Note that setting a configuration option dynamically using
set_configuration_option/2 will _not_ reset the configuration option
after any learning attempt. This means that subsequent learning attempts
will retain the value of the dynamically changed option. This will
usually not be what is expected and may well cause some confusion.

For the time being the only sure-fire way to reset a configuration
option to its original value is to edit the value of that option in the
configuration file and then reload the configuration file with make/0.


__9. Conclusions__

We have progressively sculpted an inductive proof by application of
metarule constraints, until we reduced the monstrosity regurgitated
initially to a small program, easy to learn, and that unfolds to
something reasonably readable.

The workflow demonstrated above is typical of learning large programs
with recursion and predicate invention, with Louise's Top Program
Construction algorithm (TPC). Unlike the original Meta-Interpretive
Learning algorithm, in Metagol, TPC does not operate under an Occamist
bias, trying to learn the smallest program that is consistent with its
training examples. Rather, TPC learns a Top Program, the set of clauses
in _all_ correct hypotheses.

Counter-intuitively, this maximalist approach confers an exponential
improvement in efficiency compared to Metagol, just because each clause
is learned by proving a single positive example at a time, instead of
trying to learn a set of clauses that proves _all_ the examples, which
causes a lot of backtracking.

The downside of this is that the Top Program can be very, very large and
look like nothing that a human programmer would code by hand. However,
in this example we have seen how we can use constraints to bring order
to the chaos, and improve performance overall.

*/


%configuration:metarule_constraints(m(chain,P,P,_),fail).
%configuration:metarule_constraints(m(chain,P,_,P),fail).
%configuration:metarule_constraints(m(chain, _, _, recipe), fail).
%configuration:metarule_constraints(m(chain, _, recipe, _), fail).
%configuration:metarule_constraints(m(chain, _, Q, _), fail):-
%	   memberchk(Q, ['$1', '$2']).
%configuration:metarule_constraints(m(chain, recipe, _, '$2'), fail).

% Can replace the last two constraints:
%configuration:metarule_constraints(m(chain, _, Q, _), fail):-
%	   memberchk(Q, [recipe, '$1', '$2']).

:- auxiliaries:set_configuration_option(clause_limit, [4]).
:- auxiliaries:set_configuration_option(max_invented, [2]).
:- auxiliaries:set_configuration_option(reduction, [plotkins]).
:- auxiliaries:set_configuration_option(unfold_invented, [false]).

background_knowledge(recipe/2,[break_eggs/2
			      ,whisk_eggs/2
			      ,heat_oil/2
			      ,fry_eggs/2
			      ,season/2
			      ,replace/4
			      ]).

metarules(recipe/2,[chain]).

% Replace clause above with this one to obtain more specific recipes
% only working for making omelette
% metarules(recipe/2,[chain_abduce_y]).

positive_example(recipe/2,E):-
	member(E, [recipe([egg_whisk,eggs,frying_pan,olive_oil,pepper,salt],[omelette])
		  ]).

negative_example(recipe/2,_):-
	fail.

break_eggs(Xs,Ys):-
	replace([eggs],Xs,[egg_whites,egg_yolks],Ys).
whisk_eggs(Xs,Ys):-
	replace([egg_whisk,egg_whites,egg_yolks],Xs,[whisked_eggs],Ys).
heat_oil(Xs,Ys):-
	replace([frying_pan,olive_oil],Xs,[frying_oil],Ys).
fry_eggs(Xs,Ys):-
	replace([frying_oil,whisked_eggs],Xs,[frying_eggs],Ys).
season(Xs,Ys):-
	replace([frying_eggs,pepper,salt],Xs,[omelette],Ys).


%!	replace(+Set1,+Set2,+Set3,+Set4) is det.
%
%	Replace Set1 in Set3 with Set2 to make Set4.
%
%	Set1, Set2, Set3 and Set4 are ordered sets, i.e. list sorted to
%	the standard order of terms and without any duplicates.
%
%	Set4 is Set3 subtracting Set1 and adding Set2. Or, more
%	formally:
%	==
%	Set4 = (Set3 \ Set1) U Set2
%	==
%
%	@tbd Makes no attempt to test whether any of its arguments is an
%	ordered set.
%
replace(Xs,Is,Ys,Os):-
	ground(Xs)
	,ground(Is)
	,ground(Ys)
	,ord_subset(Xs,Is)
	,ord_subtract(Is,Xs,Zs_)
	,ord_union(Ys,Zs_,Os).


% Target theory for an omelette recipe.
recipe_(As,Fs):-
	break_eggs(As,Bs)
	,whisk_eggs(Bs,Cs)
	,heat_oil(Cs,Ds)
	,fry_eggs(Ds,Es)
	,season(Es,Fs).
