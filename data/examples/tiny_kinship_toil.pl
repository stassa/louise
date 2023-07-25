:-module(tiny_kinship_toil, [background_knowledge/2
			    ,metarules/2
			    ,positive_example/2
			    ,negative_example/2
			    ,ancestor/2
			    ,grandparent/2
			    ,grandfather/2
			    ,grandmother/2
			    ,parent/2
			    ,husband/2
			    ,wife/2
			    ,child/2
			    ,son/2
			    ,daughter/2
			    ,father/2
			    ,mother/2
			    ,male/1
			    ,female/1
			    ]).

/** <module> Examples of learning metarules with TOIL.

This is a variant of tiny_kinship.pl where we learn family relationships
(how quaint) by learning the necessary metarules (wait, wat?) with
Louise's subsystem TOIL.

1. Known good configuration (salient options marked with an asterisk,
"*"):

==
?- list_config.
* clause_limit(0)
example_clauses(call)
* experiment_file(data/examples/tiny_kinship_toil.pl,tiny_kinship_toil)
fold_recursive(false)
* generalise_learned_metarules(false)
learner(louise)
* learning_predicate(learn_meta/1)
listing_limit(10)
* max_error(0,0)
* max_invented(0)
* metarule_formatting(quantified)
* metarule_learning_limits(none)
minimal_program_size(2,inf)
recursive_reduction(false)
* reduce_learned_metarules(false)
* reduction(plotkins)
* resolutions(5000)
theorem_prover(resolution)
unfold_invented(false)
true.
==

Notice in particular the metarule-learning specific configuration
options:
==
generalise_learned_metarules(false)
learning_predicate(learn_meta/1)
metarule_formatting(quantified)
metarule_learning_limits(none)
reduce_learned_metarules(false)
==

These are the default options for TOIL, and for this experiment file.

2. List results of learning the target predicates in this example file
with TOIL:
==
?- list_learning_results.
(Meta-dyadic-1) ∃.P,P,P ∀.x,y,z: P(x,y)← P(x,z),P(z,y)
(Meta-dyadic-2) ∃.P,P,P ∀.x,y,z: P(x,y)← P(z,y),P(x,z)
(Meta-dyadic-3) ∃.P,P,Q ∀.x,y,z: P(x,y)← P(x,z),Q(y,z)
(Meta-dyadic-4) ∃.P,P,Q ∀.x,y,z: P(x,y)← P(x,z),Q(z,y)
(Meta-dyadic-5) ∃.P,P,Q ∀.x,y,z: P(x,y)← P(z,y),Q(x,z)
(Meta-dyadic-6) ∃.P,P,Q ∀.x,y,z: P(x,y)← P(z,y),Q(z,x)
(Meta-dyadic-7) ∃.P,Q,P ∀.x,y,z: P(x,y)← Q(x,z),P(z,y)
(Meta-dyadic-8) ∃.P,Q,P ∀.x,y,z: P(x,y)← Q(y,z),P(x,z)
(Meta-dyadic-9) ∃.P,Q,P ∀.x,y,z: P(x,y)← Q(z,x),P(z,y)
(Meta-dyadic-10) ∃.P,Q,P ∀.x,y,z: P(x,y)← Q(z,y),P(x,z)
(Meta-dyadic-11) ∃.P,Q,Q ∀.x,y,z: P(x,y)← Q(x,z),Q(z,y)
(Meta-dyadic-12) ∃.P,Q,Q ∀.x,y,z: P(x,y)← Q(z,y),Q(x,z)
(Meta-monadic-13) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)

(Meta-dyadic-1) ∃.P,Q,Q ∀.x,y,z: P(x,y)← Q(x,z),Q(z,y)
(Meta-dyadic-2) ∃.P,Q,Q ∀.x,y,z: P(x,y)← Q(z,y),Q(x,z)

(Meta-dyadic-1) ∃.P,Q,Q ∀.x,y,z: P(x,y)← Q(x,z),Q(z,y)
(Meta-dyadic-2) ∃.P,Q,Q ∀.x,y,z: P(x,y)← Q(z,y),Q(x,z)
(Meta-dyadic-3) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Meta-dyadic-4) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(x,z)

(Meta-dyadic-1) ∃.P,Q,Q ∀.x,y,z: P(x,y)← Q(x,z),Q(z,y)
(Meta-dyadic-2) ∃.P,Q,Q ∀.x,y,z: P(x,y)← Q(z,y),Q(x,z)
(Meta-dyadic-3) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Meta-dyadic-4) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(x,z)

(Meta-monadic-1) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)

(Meta-dyadic-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(y,z)
(Meta-dyadic-2) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(y,z),R(x,z)

(Meta-dyadic-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(y,z)
(Meta-dyadic-2) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(y,z),R(x,z)

(Meta-monadic-1) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)

(Meta-precon-1) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)

(Meta-precon-1) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)

(Meta-precon-1) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)

(Meta-precon-1) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)

(Meta-projection-21-1) ∃.P,P ∀x: P(x,x)← P(x)

(Meta-projection-21-1) ∃.P,P ∀x: P(x,x)← P(x)

true.
==

3. That's a lot of metarules learned for ancestor/2! Let's reduce those
a bit. Set the following options in the configuration:
==
generalise_learned_metarules(true)
metarule_learning_limits(coverset)
==

Then call list_learning_results/0 again:
==
?- list_learning_results.
(Meta-monadic-1) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
(Meta-dyadic-2) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(z,x)
(Meta-dyadic-3) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(x,z)
(Meta-dyadic-4) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(y,z)

(Meta-dyadic-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)

(Meta-dyadic-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)

(Meta-dyadic-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(x,z)

(Meta-monadic-1) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)

(Meta-dyadic-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(y,z)

(Meta-dyadic-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(y,z),R(x,z)

(Meta-monadic-1) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)

(Meta-precon-1) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)

(Meta-precon-1) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)

(Meta-precon-1) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)

(Meta-precon-1) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)

(Meta-projection-21-1) ∃.P,Q ∀x: P(x,x)← Q(x)

(Meta-projection-21-1) ∃.P,Q ∀x: P(x,x)← Q(x)
==

Thaat's better.

4. A known bug: the option reduce_learned_metarules/1 currently doesn't
do anything:
==
% These two are the same.
reduce_learned_metarules(false)
reduce_learned_metarules(true)
==

5. You can use learn_metarules/[1,2,5] to suggest new metarules, or you
can use learn_meta/[1,2,5] to automatically pass the learned metarules
to the currently defined learning predicate. If that is
learn_meta/[1,2,5] itself, learned metarules are passed to learn/5.

Comment out this line in the source below:
==
configuration:learning_predicate(learn_metarules/1).
==

And uncomment this one:
==
%configuration:learning_predicate(learn_meta/1).
==

Now run list_learning_results/0 again:
==
?- list_learning_results.
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(C,B),parent(C,A).
ancestor(A,B):-ancestor(C,B),ancestor(A,C).
ancestor(A,B):-ancestor(C,B),parent(A,C).
ancestor(A,B):-parent(C,B),ancestor(A,C).
ancestor(A,B):-parent(C,B),parent(A,C).
ancestor(A,B):-ancestor(A,C),parent(B,C).

grandparent(A,B):-parent(A,C),parent(C,B).

grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).

grandmother(A,B):-parent(C,B),mother(A,C).

parent(A,B):-father(A,B).
parent(A,B):-mother(A,B).

husband(A,B):-father(A,C),mother(B,C).

wife(A,B):-father(B,C),mother(A,C).

child(A,B):-daughter(A,B).
child(A,B):-son(A,B).

son(A,B):-male(A),child(A,B).

daughter(A,B):-female(A),child(A,B).

father(stathis,kostas).
father(stefanos,dora).
father(kostas,stassa).
father(A,B):-male(A),parent(A,B).

mother(alexandra,kostas).
mother(paraskevi,dora).
mother(dora,stassa).
mother(A,B):-female(A),parent(A,B).

male(A,A):-male(A).

female(A,A):-female(A).

true.
==

6. TOIL can learn new metarules from examples, background knowledge and
one of two kinds of generalised metarules: maximally general
second-order metarules, or third-order metarules.

In the source below, comment-out the maximally general second-order
metaurles that look like this:
==
% Maximally general second-order metarules.
metarules(ancestor/2,[meta_dyadic,meta_monadic]).
metarules(grandparent/2,[meta_dyadic]).
metarules(grandfather/2,[meta_dyadic]).
% ...
==

And uncomment the third-order metarules that look like this:
==
% Third-order metarules.
metarules(ancestor/2,[higher_order(2,3)]).
metarules(grandparent/2,[higher_order(2,3)]).
metarules(grandfather/2,[higher_order(2,3)]).
% ...
==

Also change back the learning predicate to learn_metarules/1:
==
configuration:learning_predicate(learn_metarules/1).
==

Then list_learning_results/0 again:
==
?- list_learning_results.
(Hom-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(z,x)
(Hom-2) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(x,z)
(Hom-3) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(y,z)
(Hom-4) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)

(Hom-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)

(Hom-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)

(Hom-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(x,z)

(Hom-1) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)

(Hom-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(y,z)

(Hom-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(y,z),R(x,z)

(Hom-1) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)

(Hom-1) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)
(Hom-2) ∃.P,Q,R ∀.x,y: P(x,y)← Q(y),R(x)

(Hom-1) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)

(Hom-1) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)
(Hom-2) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(y)

(Hom-1) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)

(Hom-1) ∃.P,Q ∀x: P(x,x)← Q(x)

(Hom-1) ∃.P,Q ∀x: P(x,x)← Q(x)

true.
==

Change the leaarning predicate again to learn_meta/1:
==
configuration:learning_predicate(learn_meta/1).
==

And list_learning_results/0:
==
?- list_learning_results.
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(C,B),parent(C,A).
ancestor(A,B):-ancestor(C,B),ancestor(A,C).
ancestor(A,B):-ancestor(C,B),parent(A,C).
ancestor(A,B):-parent(C,B),ancestor(A,C).
ancestor(A,B):-parent(C,B),parent(A,C).
ancestor(A,B):-ancestor(A,C),parent(B,C).

grandparent(A,B):-parent(A,C),parent(C,B).

grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).

grandmother(A,B):-parent(C,B),mother(A,C).

parent(A,B):-father(A,B).
parent(A,B):-mother(A,B).

husband(A,B):-father(A,C),mother(B,C).

wife(A,B):-father(B,C),mother(A,C).

child(A,B):-daughter(A,B).
child(A,B):-son(A,B).

son(A,B):-male(A),child(A,B).
son(A,B):-male(B),male(A).

daughter(A,B):-female(A),child(A,B).

father(stefanos,dora).
father(kostas,stassa).
father(A,B):-male(A),parent(A,B).
father(A,B):-male(A),male(B).

mother(alexandra,kostas).
mother(paraskevi,dora).
mother(dora,stassa).
mother(A,B):-female(A),parent(A,B).

male(A,A):-male(A).

female(A,A):-female(A).

true.
==

Note that TOIL over-generates metarules for ancestor/2. There are many
metarules that entail the positive examples (and none of the negative
examples) that are possible to construct for this target predicate. This
over-generation also causes an over-verbose program to be learned with
some over _special_ clauses, overfitting to positive examples.
Currently, there isn't much that can be done about this, other than to
inspect the metarules learned by TOIL and use human intuition to select
a good subset.
*/


:-use_module(configuration).

% It's possible to learn metarules from not-fully-generalised second
% order metarules, for example try this one out;
%configuration:meta_dyadic_1 metarule 'P(x,y):- Q(z,u), R(v,w)'.

% Tells list_learning_results/0 to use the right learning predicate.
configuration:learning_predicate(learn_metarules/1).
%configuration:learning_predicate(learn_meta/1).

% Background knowledge declarations
background_knowledge(ancestor/2,[parent/2]).
background_knowledge(grandparent/2,[parent/2]).
%background_knowledge(grandfather/2,[father/2,parent/2]).
%background_knowledge(grandfather/2,[father/2,mother/2,parent/2]).
background_knowledge(grandfather/2,[father/2,parent/2,husband/2,grandmother/2]).
background_knowledge(grandmother/2,[mother/2,parent/2]).
background_knowledge(parent/2,[father/2,mother/2]).
background_knowledge(husband/2,[father/2,mother/2]).
background_knowledge(wife/2,[father/2,mother/2]).
%background_knowledge(child/2,[father/2,mother/2,parent/2]).
background_knowledge(child/2,[son/2,daughter/2]).
background_knowledge(son/2,[child/2,male/1]).
background_knowledge(daughter/2,[child/2,female/1]).
background_knowledge(father/2,[parent/2,male/1]).
background_knowledge(mother/2,[parent/2,female/1]).
background_knowledge(male/2,[male/1]).
background_knowledge(female/2,[female/1]).

% Metarules
%/* Maximally general second-order metarules.
metarules(ancestor/2,[meta_dyadic,meta_monadic]).
metarules(grandparent/2,[meta_dyadic]).
metarules(grandfather/2,[meta_dyadic]).
metarules(grandmother/2,[meta_dyadic]).
metarules(parent/2,[meta_monadic]).
metarules(husband/2,[meta_dyadic]).
metarules(wife/2,[meta_dyadic]).
%metarules(child/2,[meta_dyadic]).
metarules(child/2,[meta_monadic]).
metarules(son/2,[meta_precon]).
metarules(daughter/2,[meta_precon]).
metarules(father/2,[meta_precon,meta_projection_21]).
metarules(mother/2,[meta_precon,meta_projection_21]).
metarules(male/2,[meta_monadic,meta_projection_21]).
metarules(female/2,[meta_monadic,meta_projection_21]).
%*/
/* Third-order metarules.
metarules(ancestor/2,[higher_order(2,3)]).
metarules(grandparent/2,[higher_order(2,3)]).
metarules(grandfather/2,[higher_order(2,3)]).
metarules(grandmother/2,[higher_order(2,3)]).
metarules(parent/2,[higher_order(2,3)]).
metarules(husband/2,[higher_order(2,3)]).
metarules(wife/2,[higher_order(2,3)]).
metarules(child/2,[higher_order(2,3)]).
metarules(son/2,[higher_order(3,3)]).
metarules(daughter/2,[higher_order(3,3)]).
metarules(father/2,[higher_order(3,3)]).
metarules(mother/2,[higher_order(3,3)]).
metarules(male/2,[higher_order(2,2)]).
metarules(female/2,[higher_order(2,2)]).
*/


% Positive and negative examples generators.
positive_example(ancestor/2,ancestor(A,B)):-
	ancestor(A,B).
positive_example(grandparent/2,grandparent(A,B)):-
	grandparent(A,B).
positive_example(grandfather/2,grandfather(A,B)):-
	grandfather(A,B).
positive_example(grandmother/2,grandmother(A,B)):-
	grandmother(A,B).
positive_example(parent/2,parent(A,B)):-
	parent(A,B).
positive_example(husband/2,husband(A,B)):-
	husband(A,B).
positive_example(wife/2,wife(A,B)):-
	wife(A,B).
positive_example(child/2,child(A,B)):-
	child(A,B).
positive_example(son/2,son(A,B)):-
	son(A,B).
positive_example(daughter/2,daughter(A,B)):-
	daughter(A,B).
positive_example(father/2,father(A,B)):-
	father(A,B).
positive_example(mother/2,mother(A,B)):-
	mother(A,B).
positive_example(male/2,male(A,A)):-
	male(A).
positive_example(female/2,female(A,A)):-
	female(A).

negative_example(ancestor/2,ancestor(A,B)):-
	ancestor(B,A).
negative_example(grandparent/2,grandparent(A,B)):-
	grandparent(B,A).
negative_example(grandfather/2,grandfather(A,B)):-
	grandmother(A,B).
negative_example(grandmother/2,grandmother(A,B)):-
	grandfather(A,B).
negative_example(parent/2,parent(A,B)):-
	parent(B,A).
negative_example(husband/2,husband(A,B)):-
	wife(A,B).
negative_example(wife/2,wife(A,B)):-
	husband(A,B).
negative_example(child/2,child(A,B)):-
	parent(A,B).
negative_example(son/2,son(A,B)):-
	daughter(A,B).
negative_example(daughter/2,daughter(A,B)):-
	son(A,B).
negative_example(father/2,father(A,B)):-
	mother(A,B).
negative_example(mother/2,mother(A,B)):-
	father(A,B).
negative_example(male/2,male(A,A)):-
	female(A).
negative_example(female/2,female(A,A)):-
	male(A).


% Background knowledge definitions
ancestor(X,Y):-
	parent(X,Y).
ancestor(X,Y):-
	parent(X,Z)
	,ancestor(Z,Y).

grandparent(X,Y):-
	grandfather(X,Y).
grandparent(X,Y):-
	grandmother(X,Y).

grandfather(A,B):-
	father(A,C)
	,parent(C,B).

grandmother(A,B):-
	mother(A,C)
	,parent(C,B).

parent(X, Y):-
	father(X,Y).
parent(X, Y):-
	mother(X,Y).

husband(X,Y):-
	father(X,Z)
	,mother(Y,Z).

wife(X,Y):-
	mother(X,Z)
	,father(Y,Z).

child(X,Y):-
	parent(Y,X).
/*
child(X,Y):-
	son(X,Y).
child(X,Y):-
	daughter(X,Y).
%Causes learning child with son/2, daughter/2 and identity to go
%infinite!
*/

son(X,Y):-
	male(X)
	,child(X,Y).

daughter(X,Y):-
	female(X)
	,child(X,Y).

father(stathis, kostas).
father(stefanos, dora).
father(kostas, stassa).

mother(alexandra, kostas).
mother(paraskevi, dora).
mother(dora, stassa).

male(stathis).
male(stefanos).
male(kostas).

female(dora).
female(stassa).
female(alexandra).
female(paraskevi).
