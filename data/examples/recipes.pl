:-module(recipes, [background_knowledge/2
		  ,metarules/2
		  ,positive_example/2
		  ,negative_example/2
		  %,recipe/2
		  ,replace/4
		  ,break_eggs/2
		  ,whisk_eggs/2
		  ,heat_oil/2
		  ,fry_eggs/2
		  ,season/2
		  ,replace/4
		  ]).

:-use_module(configuration).

/* <module> Learn how to make an omelette with predicate invention.

This experiment file demonstrates the use of predicate invention to
learn a program without recursion; how to unfold the learned program
to remove invented predicates and make the originally large program
easier to understand; and how to dynamically set configuration options.


Running the example
-------------------

1. Known good configuration.

The results listed below were obtained with the following configuration
options, defined in configuration.pl and set in this file with
set_configuration_option/2.
==
?- list_config.
depth_limits(2,1)
example_clauses(call)
experiment_file(data/examples/recipes.pl,recipes)
fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
learning_predicate(learn_dynamic/1)
max_invented(2)
metarule_formatting(user_friendly)
metarule_learning_limits(none)
minimal_program_size(2,inf)
prove_recursive(invented)
recursion_depth_limit(dynamic_learning,none)
recursion_depth_limit(metasubstitution,none)
recursive_reduction(false)
reduce_learned_metarules(false)
reduction(plotkins)
resolutions(5000)
test_constraints(clause)
theorem_prover(resolution)
unfold_invented(false)
true.
==

If the output you see when you run the experiment differs from what is
listed below make sure your configuration matches the listing above.

In particular, make sure that the following options are set:
==
max_invented(2)
prove_recursive(invented)
==

The max_invented(2) option tells Louise to try and define no more than 2
invented predicates. prove_recursive(invented) tells Louise to learn by
resolution with invented predicates. The latter option is the option
that turns "on" predicate invention, proper, in Louise.


2. MIL problem elements.

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


3. Make a learning attempt:
==
?- time(learn(recipe/2,_Ps)), print_clauses(_Ps), length(_Ps,N).
% 4,387,466 inferences, 1.516 CPU in 1.596 seconds (95% CPU, 2894823 Lips)
recipe([egg_whisk,eggs,frying_pan,olive_oil,pepper,salt],[omelette]).
'$1'(A,B):-'$2'(A,C),'$2'(C,B).
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
N = 41.
==

Note the use of time/1 and length/2 to keep track of the time it takes
to learn the hypothesis above and its size. If your system is taking
considerably longer to complete the learning attempt, you should
probably examine your configuration and the elements of teh MIL problem
for recipe/2 to make sure they match the ones listed above. The
experiment listed above was run on a seven-year old laptop with an i7
processor clocked at 2.6 GHz and 16 GB of RAM. You don't need a
supercomputer to repeat it!


3. Eyballing the result.

First, find some eydrops. Ready? Now we can look more closely at the
output of the learning attempt listed above. What we observe initially
is that it is an unholly mess of invented predicates with meaningless
symbols like '$1' that don't help us at all to understand what the heck
the learned program is doing. Is it right? Is it wrong? Who can tell?

We next observe that the single example of recipe/2 is output together
with the learned program:
==
recipe([egg_whisk,eggs,frying_pan,olive_oil,pepper,salt],[omelette]).
==

This is normally a bad sign. Such "atomic residue" means that some
positive examples are not "covered" by the learned program. In this case
though it is a limitation of the implementation of Plotkin's program
reduction in lib/program_reduction, which is very bad at dealing with
lists. This is a known bug in other words.

Still, how can we tell that the learned program is correct, or even what
it is doing? That thing is nigh-on unreadable! Looking more carefully
and ceasing our annoying complaints we can remark that the two invented
predicates, '$1' and '$2' are almost identical in structure, with the
difference that they each call the other. Their literals that don't have
invented predicate symbols appear to be different combinations of
preparation methods defined in the background ("heat oil", "break eggs"
etc). Looking at the clauses with the target predicate's symbol,
recipe/2, in their head, at the bottom of the listing, we can start to
feel confident that the learned program seems to be taking alternative
paths to the same destintation, making an omelette. In turn this means
that there's no point in trying to reduce the program any further.
Besides the limitatiosn of the implementation of Plotkin's program
reduction, if each invented predicate is correctly defined, we won't
find any logical redundancies.


4. Unfolding to restore sanity and peace.

Set the configuration option unfold_invented/1 to "true" to attempt to
unfold the learned monstrosity regurgitated on our console in the
previous section, and see if we can finally understand what it's trying
to do:
==
?- time(learn(recipe/2,_Ps)), print_clauses(_Ps), length(_Ps,N).
% 4,518,082 inferences, 1.625 CPU in 1.745 seconds (93% CPU, 2780358 Lips)
recipe([egg_whisk,eggs,frying_pan,olive_oil,pepper,salt],[omelette]).
recipe(A,B):-break_eggs(A,C),heat_oil(C,D),whisk_eggs(D,E),fry_eggs(E,F),season(F,B).
recipe(A,B):-break_eggs(A,C),whisk_eggs(C,D),heat_oil(D,E),fry_eggs(E,F),season(F,B).
recipe(A,B):-heat_oil(A,C),break_eggs(C,D),whisk_eggs(D,E),fry_eggs(E,F),season(F,B).
N = 4.
==

Ah! What relief! The entire 41 clauses is now reduced to only 4, without
any incomprehensible dollar-signs.

Inspecting the unfolded program we can verify that it consists of three
clauses that define different ways to make an omelette. We need to break
eggs, of course, and we need to do that before we whisk the broken eggs.
But we can heat the oil before or after either of those two steps. We
must always fry the egggs and season the omelette as the final two steps
of the procedure. Obviously, all this is true in the context of the
definitions of the background predicates break_eggs/2, heat_oil/2 etc,
that require specific inputs to succeed. For example, the break_eggs/2
predicate expects the atom "eggs" to be in its input list, etc.


Note that the reduction in size is the result of unfolding, only. While
unfolded programs _are_ reduced at the end of er unfolding them, the
original program did not include any reductions. You can verify this by
setting the configuration option reduction/1 to "none" and repeating
the learning query above:
==
?- time(learn(recipe/2,_Ps)), print_clauses(_Ps), length(_Ps,N), resolutions(Resolutions).
% 2,777,803 inferences, 1.375 CPU in 1.466 seconds (94% CPU, 2020220 Lips)
recipe([egg_whisk,eggs,frying_pan,olive_oil,pepper,salt],[omelette]).
recipe(A,B):-break_eggs(A,C),heat_oil(C,D),whisk_eggs(D,E),fry_eggs(E,F),season(F,B).
recipe(A,B):-break_eggs(A,C),whisk_eggs(C,D),heat_oil(D,E),fry_eggs(E,F),season(F,B).
recipe(A,B):-heat_oil(A,C),break_eggs(C,D),whisk_eggs(D,E),fry_eggs(E,F),season(F,B).
N = 4,
Resolutions = 0.
==


Dynamically setting configuration options
-----------------------------------------

This experiment file demonstrates the use of the auxiliary predicate
set_configuration_option/2 to dynamically manipulate configuration
options.

In the source code below, the configuration option max_invented/1 is set
dynamically using set_configuration_option/2 as a directive:

==
auxiliaries:set_configuration_option(max_invented, [2]).
==

Note that setting a configuration option dynamically using
set_configuration_option/2 will _not_ reset the configuration option
after any learning attempt. This means that subsequent learning attempts
will retain the value of the dynamically changed option. This will
usually not be what is expected and may well cause some confusion.

For the time being the only sure-fire way to reset a configuration
option to its original value is to edit the value of that option in the
configuration file and then reload the configuration file with make/0.
*/

:- auxiliaries:set_configuration_option(max_invented, [2]).

% Tells list_learning_results/0 to use the right learning predicate.
configuration:learning_predicate(learn_dynamic/1).

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
