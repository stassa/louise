Contents of this directory
==========================

In this directoyr (`data/examples`) you will find a number of examples that show
how to use Louise. The examples cover basic use cases and more advanced use,
including how to learn recursion, predicate invnetion, how to use metarule
constraints, and more.

Table of Contents
-----------------

The following is a listing of all experiment files in this directory, in
alphabetical order:

  `abduced.pl`
  `ackermann.pl`
  `anbn.pl`
  `constraints.pl`
  `dummy.pl`
  `even_odd.pl`
  `example_invention.pl`
  `findlast.pl`
  `hello_world.pl`
  `list_processing.pl`
  `lsystem.pl`
  `mtg_fragment.pl`
  `multi_pred.pl`
  `recipes.pl`
  `recursive_folding.pl`
  `tiny_kinship.pl`
  `tiny_kinship_meta.pl`
  `tiny_kinship_toil.pl`
  `user_metarules.pl`
  `yamamoto.pl`


Files in this folder
====================

Following is a listing of the examples in this directory and a short description
of what they show.

`abduced.pl`
------------

Subject: Learning Constants.

Shows how to learn theories with constants using the `abduce` metarule.

`ackermann.pl`
--------------

Subjects: One-Shot Learning, Recursion, Flattening.

Shows how to learn the Péter-Ackerman function from a single example, some
background knowledge counting over the integers in Peano notation, and a set of
very, very specific metarules. The learned program is in "flattened" form with
new predicates replacing the use of functional terms (i.e. terms with arity more
than 0).

This contrived example is meant as a proof of concept of Louise's (and, more
generally, Meta-Interpretive Learning's) ability to learn logic programs
calculating the results of non-primitive recursive functions and, therefore,
_arbitrary recursive programs_.

`anbn.pl`
---------

Subjects: Grammar Induction, Predicate Invention.

Shows how to learn a grammar of the Context-Free `a^b^n` language using
predicate invention.

`constraints.pl`
----------------

Subject: Metarule Constraints

Shows how to use constraints, defined with `metarule_constraints/2`, to control
the programs that are learned. This example specifically shows how to eliminate
left-recursive hypotheses from the set of learned hypotheses.

`dummy.pl`
----------

Subject: none

Dummy experiment file meant to be copied, and modified, by the user. Use
`dummy.pl` to quickly set up your own experiment files with the necessary
experiment file interface predicates.

`even_odd.pl`
-------------

Subjects: Parity, Mutual Recursion, Predicate Invention

Shows how to learn a theory of parity with two, mutually recursive clauses,
representing "even" and "odd", from one positive and one negative example of
"even". While no examples are given of "odd" (other than the negative example of
"even"), the concept of odd is invented during learning. 


`example_invention.pl`
----------------------

Subject: Examples Invention

Shows how to learn new examples with Louise's examples invention facility.

`findlast.pl`
-------------

Subjects: List Processing, One-Shot Learning, Recursion

Shows an example of learning a recursive predicate to find the last element in a
list from a single example of a list, and its last element. Example proposed by
Dr. Andrew Cropper who pointed out that an earlier version of Louise could not
learn recursion one-shot without an example of the base-case. Well, now it can.

`hello_world.pl`
----------------

Subject: first example of using Louise

Shows how to learn a recursive theory of the ancestor family relation, a classic
example used as a motivating example in numerous Inductive Logic Programming
publications (and discussions). Stands in for a "hello world" first example.

`list_processing.pl`
--------------------

Subject: List Processing, Flattening

Shows how to learn four standard list-processing predicates, to scan, append,
reverse, and count the elements of, a list. The learned predicates are in
"flattened" form, replacing the use of functional terms with new predicates. The
four learning taregts are classic exercises for students learning Prolog.

`mtg_fragment.pl`
-----------------

Subject: Grammar Induction

Shows how to learn a fragment of the grammar for the ability text on the cards
used to play the Magic: the Gathering Collectible Card Game.

`multi_pred.pl`
---------------

Subject: Multi-Predicate Learning

Shows how to learn multiple targets at once. In the example, two, mutually
rcursive, definitions of even/1 and odd/1 are learned from examples of both and
using each other as background knowledge.

`recipes.pl`
------------

Subjects: Omelettes, Recursion, Predicate Invetion, Metarule Constraints,
Unfolding

Shows how to learn a plan to make an omelette from a list of ingredients and
utensils. The resulting program starts large and incomprehensibly complex, with
unnecessary recursion and inscrutable predicate invention. By the application of
metarule constraints, the example shows how to sculpt the learned hypothesis
into something more palatable (and also speed up its learning) using constraints
and unfolding to remove invented predciates.

`recursive_folding.pl`
----------------------

Subjects: List Processing, Folding Overspecialised Hypotheses

Shows how to use folding to introduce recursive literals to a hypothesis that is
both over-special and does not use recursion. Maybe be a useful alterantive when
learning a recursive program is too computationally expensive.

`tiny_kinship.pl`
-----------------

Subjects: Family Relations, Multiple Learning Targets in one Experiment File

Shows an extended example of learning family relations, similar to
`hello_world.pl` but with many more relations using each other as background
knowledge. As `hello_world.pl` this is also a classic motivating example in ILP.

The example also demonstrates how multiple learning targets with their own
examples, background knowledge an metarules can be defined in one experiment
file.

Finally, the example shows how to use the predicate `list_learning_results/0` to
learn from each of the learning targets in a file with one top-level call, which
can be useful as a sanity check.

`tiny_kinship_toil.pl`
----------------------

Subjects: Family Relations, Multiple Learning Targets, Learning Metarules

Shows how to learn metarules for the family relations in `tiny_kinship.pl` using
Louise's metarule-learning substystem TOIL.

`user_metarules.pl`
-------------------

Subjects: Defining own Metarules

Shows how a user can define their own metarules by adding them to an experiment
file.

`yamamoto.pl`
-------------

Subjects: Parity, One-Shot Learning, Recursion

Shows how to learn Yamamoto's famous example of the incompleteness of Inverse
Entailment (IE) from his 2005 paper, `What Hypotheses Can be Found with Inverse
Entailment?`. Demonstrates that Louise (and MIL in general) can learn programs
impossible to learn with earlier approaches, such as IE
