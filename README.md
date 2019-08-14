Louise - polynomial-time Meta-Interpretive Learning
===================================================

Louise is a Meta-Interpretive Learning (MIL) system based on a polynomial-time
learning algorithm. Louise is efficient enough to learn programs that are too
large to be larned by current MIL learners like [Metagol].

Meta-Interpretive Learning is a new paradigm for Inductive Logic Programming
(ILP). ILP algorithms learn logic programs from examples and background
knowledge. The examples and background knowledge are also defined as logic
programs. In MIL, a set of clause templates, called _metarules_ are also used to
direct the search for hypotheses. See the reference section for pubications on
MIL. Publications on Louise are coming soon. An example of learning with Louise
follows in the next section.

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
   `tiny_kinship.pl`:
   
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
examples for a learning target to be defined in an _experiment file_ a Prolog
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
metarules/2 clause and any predicates used by `positive_example/2` and
`negative_example/2` must be accessible to the module `user`. 

For small experiments, it will be most convenient to define background
predicates in the experiment file itself and add them to that module's
export-list. Larger experiments can import additional files into the experiment
file, or directly into module `user`.

For example, in the `tiny_kinship.pl` experiment file, the following background
knowledge predicates are defined, along with the target theory for `ancestor/2`
that is used to geenrate positive and negative examples:

```
ancestor(X,Y):-
	parent(X,Y).
ancestor(X,Y):-
	parent(X,Z)
	,ancestor(Z,Y).

parent(X,Y):-
	father(X,Y).
parent(X,Y):-
	mother(X,Y).

father(stathis, kostas).
father(stefanos, dora).
father(kostas, stassa).

mother(alexandra, kostas).
mother(paraskevi, dora).
mother(dora, stassa).
```

The metarules declared for `ancestor/2`, _tailrec_ and _identity_, are defined
in the configuration file, `configuration.pl`, along with other common metarules
that are useful for learning many different targets.

If special metarules are required, they can be defined in an experiment file,
with a module qualifier for the configuration module. This is done as follows:

```
configuration:metarule(postcorn,P,Q,R):- m(P,X,Y), m(Q,X,Y), m(R,X).
```

More detailed information about experiment files will follow. For the time
being, the examples provided in louise/examples can be inspected for help.

Inspecting the steps of Louise's learning procedure
---------------------------------------------------

Louise's learning algorithm proceeds in the following steps:

  1. Encapsulation of the MIL problem.
  2. Construction of the Top prorgam.
  3. Reduction of the Top program.
  4. Unfolding and excapsulation of the reduced Top program.

Steps 1, 2 and 3 can be listed for debugging. Step 4 is the output of the
learning procedure.

### Listing the MIL problem

A MIL problem consists of sets of positive and negative examples, definitions of
background knowledge predicates and a set of metarules. In Louise, those are
defined in an experiment file as described in a previous section. The MIL
problem for a learning target can be inspected with a call to the predicate
`list_mil_problem/1`. The following is a listing of the MIL problem for
`ancestor/2` defined in `tiny_kinship.pl`:

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
inspected with a call to the predicate `list_encapsulated_problem/1` 

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

In an encapsulated MIL problem, each literal `L(t_1,...,t_n)` in the examples,
background knowledge and metarules is replaced with a literal
`m(L,t_1,...,t_n)`. Encapsulation of the MIL problem allows simple and efficient
construction and reduction of the Top program.

### Listing the Top program for a MIL problem

The second step in Louise's learning procedure constructs the _Top prorgam_, the
most general program that entails each positive example and none of the negative
examples given the background knowledge and metarules in a MIL problem. Each
clause in a correct hypothesis is also a clause in the Top program.

The Top program can be inspected with a call to `list_top_program/1`.

The following is a listing of the Top program constructed for `grandfather/2`
(the Top program for `ancestor/2`, that we used as an example until now, does
not change between the two construction steps -at least not with the data in
`tiny_kinship.pl`- and so is not very illustrative of the Top program
construction procedure).

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
with the second parameter (the "unfold" paramter) set to "false", as follows:

```
?- list_top_program(grandfather/2, false).
Generalisation:
---------------
m(chain,grandfather,father,father).
m(chain,grandfather,father,mother).
m(chain,grandfather,father,parent).
m(chain,grandfather,parent,father).
m(chain,grandfather,parent,mother).
m(chain,grandfather,parent,parent).
Length:6

Specialisation:
---------------
m(chain,grandfather,father,father).
m(chain,grandfather,father,mother).
m(chain,grandfather,father,parent).
Length:3
true.
```

### Listing the reduced Top program

The third step in Louise's learning procedure is the reduction of the Top
program along with the MIL problem by application of Gordon Plotkin's program
reduction algorithm from his doctoral thesis (see references at end).

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
`list_top_program_reduction/1` must first construct the Top program and then
reduce it. Therefore, calling this listing predicate entails actually training
Louise. This means that, if training must take a long time, so will the listing
of the Top program reduction. To clarify, what is likely to take a long time is
the _reduction_ step.

Optionally, Louise can reduce the Top program recursively, by feeding back each
reduction to Plotkin's algorithm until the size of the reduction stops changing.
`list_top_program_reduction/1` currently performs only the _first_ step of this
process. This means the reduction listed by `list_top_program_reduction/1` might
simultaneously be faster and weaker (in the sense of leaving more redundant
clauses in the learned hypothesis) than the result obtained with a call to
`learn/1` etc.

### Listing the learned hypothesis

The last step in Louise's learning procedure is the _excapsulation_ of the
reduced Top program. Excapsulation is the opposite process of encapsulation. The
result of excapsulation is a set of definite clauses output directly by the
learning predicates, `learn/[1,2,5]`. Therefore this step has no explicit
listing predicate.

More detailed information about Louise's learning procedure is soon to follow.


Bibliography and References
===========================

1. S.H. Muggleton, D. Lin, N. Pahlavi, and A. Tamaddoni-Nezhad. _Meta-interpretive learning: application to grammatical inference_. [Machine Learning, 94:25-49, 2014](https://link.springer.com/article/10.1007/s10994-013-5358-3)

2. S.H. Muggleton, D. Lin, and A. Tamaddoni-Nezhad. _Meta-interpretive learning of higher-order dyadic datalog: Predicate invention revisited_. [Machine Learning, 100(1):49-73, 2015](https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf)

3. Plotkin, Gordon, _Automatic Methods of Inductive Inference_. Doctoral thesis. The University of Edinburgh, 1972.

[Metagol]: https://github.com/metagol/metagol "Metagol"
