Louise - efficient Meta-Interpretive Learning of large programs
===============================================================

Louise is an new Meta-Interpretive Learning (MIL) system that is efficient
enough to learn large programs, in particular, programs too large to be learned
by current MIL learners like [Metagol].

Meta-Interpretive Learning is a new paradigm for Inductive Logic Programming
(ILP). See the reference section for pubications on MIL. Publications on Louise
are coming soon.

Example of Use
--------------

The following is an example showing how to learn the "ancestor" relation using
Louise and the examples, background knowledge and metarules defined in
`examples/tiny_kinship.pl`.


1. Consult the project's load file to load the project:
   
   ```prolog
   ?- [load_project].
   ```

   This should start the Swi-Prolog IDE and open the project source files in the
   editor, including `configuration.pl` used in the next step. It should also start
   the documentation server and open this README file in your browser.

2. Edit the project's configuration file to select an experiment file:

   Edit `configuration.pl` in the Swi-Prolog editor (or your favourite text
   editor) and make sure the name of the current experiment file is set to
   `anbn.pl`:
   
   ```prolog
   experiment_file('data/examples/kinship/tiny_kinship.pl',tiny_kinship).
   ```

3. Recompile the project:

   ```prolog
   ?- make.
   ```

4. Run a query to train Louise on ancestor/2, one of the learning targets
   defined in the chosen experiment file:

   ```
   ?- learn(ancestor/2).
   ancestor(A,B):-parent(A,B).
   ancestor(A,B):-ancestor(A,C),ancestor(C,B).
   true.
   ```

   Louise learns a recursive hypothesis of ancestor/2 from the background
   knowledge, metarules and examples defined for that target in
   'tiny\_kinship.pl'.


Structure of the experiment file
--------------------------------

Louise expects the background knowledge, metarules and positive and negative
examples for a learning target to be defined in an _experiment file_ a Prolgo
module file with a specific structure.

An experiment file is a module that must export _at least_ the following set of
Prolog predicates, which we will call the _experiment file interface_:

```
background_knowledge/2
metarules/2
positive_example/2
negative_example/2
```

For example, in `tiny_kinship.pl` the following clauses of the interface
predicates are defined for `ancestor/2`:

```
background_knowledge(ancestor/2,[father/2,mother/2,parent/2]).
metarules(ancestor/2,[tailrec,identity]).
positive_example(ancestor/2,ancestor(A,B)):-
	ancestor(A,B).
negative_example(ancestor/2,ancestor(A,B)):-
	ancestor(B,A).
```

For convenience, a single experiment file may hold the training data for more
than one learning target. 

For each learning target in an experiment file, a clause of each of the
interface predicates above must be declared.

Additionally, the definitions of each of the background knowledge predicates
listed in a `background_knowledge/2` clause, the metarules named in a
metarules/2 clause and any predicates used by `positive_examples/2` and
`negative_examples/2` must be accessible to the module user. 

For small experiments, it will be most convenient to define background
predicates in the experiment file itself and add them to that module's
export-list. Larger experiments can import additional files into the experiment
file, or directly into module user.

For example, in the `tiny_kinship.pl` experiment file, the following background
knowledge predicates are defined, along with the target theory for `ancestor/2`
that is used to geenrate positive and negative examples:

```
ancestor(X,Y):-
	parent(X,Y).
ancestor(X,Y):-
	parent(X,Z)
	,ancestor(Z,Y).

parent(X, Y):-
	father(X,Y).
parent(X, Y):-
	mother(X,Y).

father(stathis, kostas).
father(stefanos, dora).
father(kostas, stassa).

mother(alexandra, kostas).
mother(paraskevi, dora).
mother(dora, stassa).
```

The metarules for `ancestor/2`, _tailrec_ and _identity_ are defined in the
configuration file, along with other common metarules that are useful for
learning many different targets.

If special metarules are required, they can be defined in an experiment, with a
module qualifier for the configuration module. This is done as follows:

```
configuration:metarule(postcorn,P,Q,R):- m(P,X,Y), m(Q,X,Y), m(R,X).
```

More detailed information about experiment files will follow. For the time
being, the examples provided in louise/examples can be inspected for help.


Inspecting the steps of Louise's learning procedure
---------------------------------------------------

Louise's learning algorithm proceeds in the following steps:

1. Encapsulation of the MIL problem
2. Construction of the Top prorgam
3. Reduction of the Top program
4. Unfolding and excapsulation of the reduced Top program.

Steps 1 and 2 can be listed for debugging. Step 4 is the output of the learning
procedure. Step 3 does not have an explicit listing predicate.

### Listing the MIL problem

A MIL problem consists of a set of examples, background knowledge and metarules.
In Louise, those are defined in an experiment file as described in a previous
section. The MIL problem for a learning target can be inspected with a call to
the predicate `list_mil_problem/1`. The following is a listing of the MIL
problem for `ancestor/2` defined in `tiny_kinship.pl`:

```
?- list_mil_problem(ancestor/2).
Positive examples
-----------------
ancestor(alexandra,kostas).
ancestor(alexandra,stassa).
ancestor(dora,stassa).
ancestor(kostas,stassa).
ancestor(paraskevi,dora).
ancestor(paraskevi,stassa).
ancestor(stathis,kostas).
ancestor(stathis,stassa).
ancestor(stefanos,dora).
ancestor(stefanos,stassa).

Negative examples
-----------------
:-ancestor(dora,paraskevi).
:-ancestor(dora,stefanos).
:-ancestor(kostas,alexandra).
:-ancestor(kostas,stathis).
:-ancestor(stassa,alexandra).
:-ancestor(stassa,dora).
:-ancestor(stassa,kostas).
:-ancestor(stassa,paraskevi).
:-ancestor(stassa,stathis).
:-ancestor(stassa,stefanos).

Background knowledge
--------------------
father/2:
father(stathis,kostas).
father(stefanos,dora).
father(kostas,stassa).

mother/2:
mother(alexandra,kostas).
mother(paraskevi,dora).
mother(dora,stassa).

parent/2:
parent(A,B):-father(A,B).
parent(A,B):-mother(A,B).

Metarules
---------
metarule(tailrec,A,B,A):-m(A,C,D),m(B,C,E),m(A,E,D).
metarule(identity,A,B):-m(A,C,D),m(B,C,D).
true.
```

### Listing the Encapsulation of a MIL problem.

The first step in Louise's learning procedure transforms a MIL problem into an
_encapsulated_ representation. The encapsulation of a MIL problem can be
inspected with a cll to the predicate `list_encapsulated_problem/1` 

An example of listing the encapsulated MIL problem for `ancestor/2` is as
follows:

```
?- list_encapsulated_problem(ancestor/2).
Predicate signature
-------------------
s(ancestor).
s(father).
s(mother).
s(parent).

Positive examples
-----------------
m(ancestor,alexandra,kostas).
m(ancestor,alexandra,stassa).
m(ancestor,dora,stassa).
m(ancestor,kostas,stassa).
m(ancestor,paraskevi,dora).
m(ancestor,paraskevi,stassa).
m(ancestor,stathis,kostas).
m(ancestor,stathis,stassa).
m(ancestor,stefanos,dora).
m(ancestor,stefanos,stassa).

Negative examples
-----------------
:-m(ancestor,dora,paraskevi).
:-m(ancestor,dora,stefanos).
:-m(ancestor,kostas,alexandra).
:-m(ancestor,kostas,stathis).
:-m(ancestor,stassa,alexandra).
:-m(ancestor,stassa,dora).
:-m(ancestor,stassa,kostas).
:-m(ancestor,stassa,paraskevi).
:-m(ancestor,stassa,stathis).
:-m(ancestor,stassa,stefanos).

Background knowledge
--------------------
m(father,stathis,kostas).
m(father,stefanos,dora).
m(father,kostas,stassa).
m(mother,alexandra,kostas).
m(mother,paraskevi,dora).
m(mother,dora,stassa).
m(parent,A,B):-m(father,A,B).
m(parent,A,B):-m(mother,A,B).

Metarules
---------
m(tailrec,A,B,A):-(s(A),s(B),s(A)),m(A,C,D),m(B,C,E),m(A,E,D).
m(identity,A,B):-(s(A),s(B)),m(A,C,D),m(B,C,D).
true.
```

### Listing the Top program for a MIL problem

The second step in Louise's learning procedure constructs the _Top prorgam_, the
most general program that entails each positive example and none of the negative
examples given the background knowledge and metarules in a MIL problem.

The Top program can be inspected with a call to `list_top_program/1`.

The following is a listing of the Top program constructed for `grandfather/2`,
unlike in the previous examples, where we are listing `ancestor/2` results. The
top program for ancestor/2 does not change between the two construction steps
(at least not with the data in `tiny_kinship.pl` and so is not very illustrative
of the top program construction process:

```
?- list_top_program(grandfather/2).
Generalisation:
---------------
m(grandfather,A,B):-m(father,A,C),m(father,C,B).
m(grandfather,A,B):-m(father,A,C),m(mother,C,B).
m(grandfather,A,B):-m(father,A,C),m(parent,C,B).
m(grandfather,A,B):-m(parent,A,C),m(father,C,B).
m(grandfather,A,B):-m(parent,A,C),m(mother,C,B).
m(grandfather,A,B):-m(parent,A,C),m(parent,C,B).
Length:6

Specialisation:
---------------
m(grandfather,A,B):-m(father,A,C),m(father,C,B).
m(grandfather,A,B):-m(father,A,C),m(mother,C,B).
m(grandfather,A,B):-m(father,A,C),m(parent,C,B).
Length:3
true.
```

The top program construction actually creates a set of _metasubstitutions_ of
the existentially quantified variables in metarules. These are unfolded to a set
of definite clauses before being handed to the Top program reduction step.

`list_top_program/1` outputs the unfolded set, but the pre-unfolding set of
metasubstitutions can still be inspected with a call to `list_top_program/2`,
with the second paramter (the "unfold" paramter) set to "true", as follows:

```
?- list_top_program(grandfather/2, true).
Generalisation:
---------------
m(grandfather,A,B):-m(father,A,C),m(father,C,B).
m(grandfather,A,B):-m(father,A,C),m(mother,C,B).
m(grandfather,A,B):-m(father,A,C),m(parent,C,B).
m(grandfather,A,B):-m(parent,A,C),m(father,C,B).
m(grandfather,A,B):-m(parent,A,C),m(mother,C,B).
m(grandfather,A,B):-m(parent,A,C),m(parent,C,B).
Length:6

Specialisation:
---------------
m(grandfather,A,B):-m(father,A,C),m(father,C,B).
m(grandfather,A,B):-m(father,A,C),m(mother,C,B).
m(grandfather,A,B):-m(father,A,C),m(parent,C,B).
Length:3
true.
```

### Listing the reduced Top program

The third step in Louise's learning procedure is the reduction of the Top
program and the MIL problem by Gordon Plotkin's program reduction algorithm,
from his doctoral thesis (see references at end).

The reduction step, too, can be inspected by a call to the predicate
`list_top_program_reduction/1`. 

The following is a listing of the reduction step for `ancestor/2`:

```
?- list_top_program_reduction(ancestor/2).
Program clauses:
----------------
s(ancestor)
s(father)
s(mother)
s(parent)
m(father,stathis,kostas)
m(father,stefanos,dora)
m(father,kostas,stassa)
m(mother,alexandra,kostas)
m(mother,paraskevi,dora)
m(mother,dora,stassa)
m(parent,A,B):-m(father,A,B)
m(parent,A,B):-m(mother,A,B)
m(ancestor,alexandra,kostas)
m(ancestor,alexandra,stassa)
m(ancestor,dora,stassa)
m(ancestor,kostas,stassa)
m(ancestor,paraskevi,dora)
m(ancestor,paraskevi,stassa)
m(ancestor,stathis,kostas)
m(ancestor,stathis,stassa)
m(ancestor,stefanos,dora)
m(ancestor,stefanos,stassa)
m(ancestor,A,B):-m(ancestor,A,B)
m(ancestor,A,B):-m(father,A,B)
m(ancestor,A,B):-m(mother,A,B)
m(ancestor,A,B):-m(parent,A,B)
m(ancestor,A,B):-m(ancestor,A,C),m(ancestor,C,B)
m(ancestor,A,B):-m(father,A,C),m(ancestor,C,B)
m(ancestor,A,B):-m(mother,A,C),m(ancestor,C,B)
m(ancestor,A,B):-m(parent,A,C),m(ancestor,C,B)
m(tailrec,A,B,A):-(s(A),s(B),s(A)),m(A,C,D),m(B,C,E),m(A,E,D)
m(identity,A,B):-(s(A),s(B)),m(A,C,D),m(B,C,D)

Program reduction:
------------------
s(ancestor)
s(father)
s(mother)
s(parent)
m(father,stathis,kostas)
m(father,stefanos,dora)
m(father,kostas,stassa)
m(mother,alexandra,kostas)
m(mother,paraskevi,dora)
m(mother,dora,stassa)
m(parent,A,B):-m(father,A,B)
m(parent,A,B):-m(mother,A,B)
m(ancestor,A,B):-m(parent,A,B)
m(ancestor,A,B):-m(ancestor,A,C),m(ancestor,C,B)
m(tailrec,A,B,A):-(s(A),s(B),s(A)),m(A,C,D),m(B,C,E),m(A,E,D)
m(identity,A,B):-(s(A),s(B)),m(A,C,D),m(B,C,D)

Redundant clauses:
------------------
m(ancestor,alexandra,kostas)
m(ancestor,alexandra,stassa)
m(ancestor,dora,stassa)
m(ancestor,kostas,stassa)
m(ancestor,paraskevi,dora)
m(ancestor,paraskevi,stassa)
m(ancestor,stathis,kostas)
m(ancestor,stathis,stassa)
m(ancestor,stefanos,dora)
m(ancestor,stefanos,stassa)
m(ancestor,A,B):-m(ancestor,A,B)
m(ancestor,A,B):-m(father,A,B)
m(ancestor,A,B):-m(mother,A,B)
m(ancestor,A,B):-m(father,A,C),m(ancestor,C,B)
m(ancestor,A,B):-m(mother,A,C),m(ancestor,C,B)
m(ancestor,A,B):-m(parent,A,C),m(ancestor,C,B)
```

Note that, in order to list the reduction of the Top program,
`list_top_program_reduction/1` must first construct the Top program, and then
reduce it. Therefore, calling this listing predicate entails actually training
Louise. This means that, if training must take a long time, so will the listing
of the Top program reduction.

### Listing the learned hypothesis

The last step in Louise's learning procedure is the unfolding and excapsulation
of the reduced Top program as a set of definite clauses. The result of this step
is output directly by the learning predicates, `learn/[1,2,5]` and so this step,
too, has no explicit listing predicate.

More detailed information about Louise's learning procedure is soon to follow.


Bibliography and References
===========================

1. S.H. Muggleton, D. Lin, N. Pahlavi, and A. Tamaddoni-Nezhad. _Meta-interpretive learning: application to grammatical inference_. [Machine Learning, 94:25-49, 2014](https://link.springer.com/article/10.1007/s10994-013-5358-3)

2. S.H. Muggleton, D. Lin, and A. Tamaddoni-Nezhad. _Meta-interpretive learning of higher-order dyadic datalog: Predicate invention revisited_. [Machine Learning, 100(1):49-73, 2015](https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf)

3. Plotkin, Gordon, _Automatic Methods of Inductive Inference_. Doctoral thesis. The University of Edinburgh, 1972.

[Metagol]: https://github.com/metagol/metagol "Metagol"
