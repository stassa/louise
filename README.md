Louise - polynomial-time Meta-Interpretive Learning
===================================================

The most important thing
------------------------

Louise's author can be reached by email at ep2216@ic.ac.uk. Please use this
email to ask for any help you might need with using Louise.

Louise is brand new and, should you choose to use it, you will most probably
encounter errors and bugs. The author has no way to know of what bugs and errors
you encoutner unless you report them. Please use the author's email to contact
the author regarding bugs and errors. Alternatively, you are welcome to open a
github Issue or send a pull request. 

The author specifically pledges never to direct a user to RTFM or in any other
way dismiss the user's problems with installing or using Louise as trivial.

Table of contents
-----------------

[Introduction] (## What Louise, is)
[A first example of learning with Louise](## A first example of learning with Louise)
[Structure of the experiment file](## Structure of the experiment file)
[Learning with second-order metarules](## Learning with second-order metarules)
[Learning by Top program construction and reduction] (## Learning by Top program construction and reduction)
[Using Louise: useful auxiliary predicates](## Using Louise: useful auxiliary predicates)
[Bibliography and References](## Bibliography and References)


What Louise, is
---------------

Louise is a machine learning system that learns Prolog programs.

Louise is based on a new Meta-Interpretive Learning algorithm that runs in
polynomial time. 

Meta-Interpretive Learning (MIL) is a new paradigm for Inductive Logic
Programming (ILP). ILP is the branch of machine learning that studies algorithms
learning logic programs from examples and background knowledge. In ILP, the
examples and background knowledge are also defined as logic programs. In MIL, in
addition to examples and background knowledge, a set of clause templates called
_metarules_ are also used. Examples, background knowledge and metarules are
discussed later in this README file. 

Louise is intersting because its polynomial-time learning algorithm is efficient
enough to learn programs that are too large to be learned by current MIL
learners, like [Metagol].

A first example of learning with Louise
---------------------------------------

### Running the example in Swi-Prolog.

Louise was written for Swi-Prolog. To run the example in this section you will
need to install Swi-Prolog. You can download Swi-Prolog from the following URL:

`https://www.swi-prolog.org/Download.html`

Louise runs with any of the latest stable or development releases listed on that
page. Choose the one you prefer to download.

It is recommended that you run the examples using the Swi-Prolog graphical IDE,
rather than in a system console.

### Learning the "ancestor" relation.

In this section, we assume you have cloned this project into a directory called
`louise`. Paths to various files will assume that 'louise' is the root directory
of the project.

The following is an example showing how to use Louise to learn the "ancestor"
relation from the examples, background knowledge and metarules defined in the
_experiment file_ `louise/data/examples/tiny_kinship.pl`. Experiment files are
discussed in more detail in a later section.

In summary, there are four steps to running the example: a) start Louise; b)
edit the configuration file to select `tiny_kinship.pl` as the experiment file;
c) load the experiment file into memory; d) run a learning query. These four
steps are discussed in detail below.

 1. Consult Louise's load file to load necessary source files.

    The path to the load file is `louise/load_project.pl`. To consult it, start
    Swi-Prolog inside the `louise/` root directory and enter the following query
    at the Swi-Prolog console:
    
    ```prolog
    ?- [load_project].
    ```

    Alternatively, you can (double) click the load file.

    Either of these methods will start the Swi-Prolog IDE and open the project's
    source files in the Swi-Prolog editor. One of the files opened in the editor
    will be the project's configuration file, `configuration.pl` used in the
    next step. The documentation server will also start and open this README
    file in your web browser.

 2. Edit the project's configuration file to select an experiment file.

    Edit `louise/configuration.pl` in the Swi-Prolog editor (or your favourite
    text editor) and make sure the name of the current experiment file is set to
    `tiny_kinship.pl`:

    ```prolog
    experiment_file('data/examples/tiny_kinship.pl',tiny_kinship).
    ```

    The above line will already be in the configuration file when you first get
    Louise. There will be a few more clauses of `experiment_file/2`, each on a
    seprate line and commented-out. Make sure that only a single
    `experiment_file/2` clause is loaded in memory (i.e. don't uncomment any
    other line except for the one above).

 3. Reload the configuration file.

    The easiest way to reload the configuration file is to use Swi-Prolog's `make/0`
    predicate to recompile the project (don't worry- this takes less than a
    second). To recompile the project with `make/0` enter the following query in
    the Swi-Prolog console:

    ```prolog
    ?- make.
    ```

    Note again: this is the Swi-Prolog predicate `make/0`. It's not the _make_
    build automation tool!

 4. Train Louise on the examples, background knowledge and metarules for
    `ancestor/2`.

    Execute the following query in the Swi-Prolog console; you should see the
    listed output:
 
    ```prolog
    ?- learn(ancestor/2).
    ancestor(A,B):-parent(A,B).
    ancestor(A,B):-ancestor(A,C),ancestor(C,B).
    true.
    ```
 
    The predicate `learn/2` takes as argument the predicate symbol and arity of
    a _learning target_ defined in the currently loaded experiment file.
    `ancestor/2` is one of the learning targets defined in `tiny_kinship.pl`,
    the experiment file selected in step 2. The same experiment file defines a
    number of other learning targets from a typical kinship relations domain.

    The two clauses of `ancestor/2` listed in the output of the example query
    above are the clauses of the Prolog program that Louise has learned from the
    examples, background knowledge and metarules declared for `ancestor/2` in
    `tiny_kinship.pl`

    The following section lists the contents of the `tiny_kinship.pl` experiment
    file that pertain to the `ancestor/2` learning target and discusses the
    structure of an experiment file in general.


Structure of the experiment file
--------------------------------

### Experiment file interface predicates

An _experiment file_ is a Swi-Prolog module file that contains the examples,
background knowledge and metarules for one or more _learning targets_. Learning
targets are predicates that can be learned from the data in an experiment file.

Experiment files have a common public interface. Each experiment file module
must export at least the following set of Prolog predicates, which we will call
the experiment file _interface predicates_:

```
% Experiment file interface predicates
background_knowledge/2
metarules/2
positive_example/2
negative_example/2
```

For example, the module declaration for `tiny_kinship.pl` includes the
experiment file interface predicates:

```prolog
% Exporting experiment file interface predicates
:-module(tiny_kinship, [background_knowledge/2
		       ,metarules/2
		       ,positive_example/2
		       ,negative_example/2
		       ,ancestor/2
		       % ... more exports
```

### The dummy experiment file

You can create a new experiment file quickly by copying and renaming the example
experiment file `louise/data/examples/dummy.pl`. `dummy.pl` defines "empty"
definitions of the experiment file interface predicates:

```prolog
% Contents of the dummy.pl example experiment file.
:-module(dummy, [background_knowledge/2
		,metarules/2
		,positive_example/2
		,negative_example/2]).

background_knowledge(_, _).

metarules(_,_).

positive_example(_,_).

negative_example(_,_).
```

You can replace the definitions of the experiment file interface predicates with
the ones that you need, as explained in the following sections.

### Defining a MIL problem

Clauses of the interface predicates in an experiment file define a _MIL problem_
for each learning target in the experiment file. A MIL problem is a tuple,
`(Pos,Neg,BK,MS)`, where `Pos` is a set of positive examples, `Neg` is a set of
negative examples, `BK` is a set of definitions of background knowledge
predicates and `MS` is a set of metarules. The components of a MIL problem are
discussed in detail in a later section.

In `tiny_kinship.pl` the following clauses of the interface predicates are
defined for `ancestor/2`:

```prolog
% Experiment file interface predicates defined in tiny_kinship.pl for
% ancestor/2.
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
interface predicates must be declared.

Additionally, the definitions of each of the background knowledge predicates
listed in a `background_knowledge/2` clause, the metarules named in a
`metarules/2` clause and any predicates used by `positive_example/2` and
`negative_example/2` must be accessible to the module `user`. 

### Inspecting a MIL problem

The predicate `experiment_data/5` is the recommended interface to experiment
files. Use this predicate whenever you wish to obtain the examples, background
knowledge and metarules declared for a learning target in the experiment file
currently selected in the configuration (i.e. the one named in
`experiment_file/2`).

For example, the following query will collect the components of the MIL problem
declared for `ancestor/2` in `tiny_kinship.pl` (provided that `kinship.pl` is
the current experiment file):

```prolog
% Calling experiment_data/5
?- experiment_data(ancestor/2,Pos,Neg,BK,MS).
Pos = [ancestor(alexandra, kostas), ancestor(alexandra, stassa), ancestor(dora, stassa), ancestor(kostas, stassa), ancestor(paraskevi, dora), ancestor(paraskevi, stassa), ancestor(stathis, kostas), ancestor(stathis, stassa), ancestor(..., ...)|...],
Neg = [(:-ancestor(dora, paraskevi)),  (:-ancestor(dora, stefanos)),  (:-ancestor(kostas, alexandra)),  (:-ancestor(kostas, stathis)),  (:-ancestor(stassa, alexandra)),  (:-ancestor(stassa, dora)),  (:-ancestor(stassa, kostas)),  (:-ancestor(..., ...)),  (:- ...)|...],
BK = [parent/2],
MS = [tailrec, identity].
```

To clarify: _it is not recommended_ to use the experiment file interface
predicates themselves to obtain examples, background knowledge or metarules.
Prefer `experiment_data/5` for this job.

Below we discuss each of the interface predicates in turn.

### Positive and negative example generators

The interface predicates `positive_example/2` and `negative_example/2` are
defined as generators of positive and negative examples, respectively. That they
are "generators" means that a call to one of these predicates returns each
example of the appropriate kind on backtracking.

The first, or input, argument of each of the two example generators must be the
predicate symbol and arity of a learning target. The second, or output, argument
must be bound nondeterministically to each of the appropriate kind of examples,
positive or negative, of the learning target.

For instance, given the definition of `positive_example/2` listed above, for
`ancestor/2` the following query will return each positive atom of `ancestor/1`
on successive backtracking:

```prolog
% Generating positive examples
?- configuration:positive_example(ancestor/2, E).
E = ancestor(stathis, kostas) ;
E = ancestor(stefanos, dora) ;
% More results on backtracking
```

The examples generators for `ancestor/2` listed in the previous section make use
of the _target theory_ for `ancestor/2`. The "target theory" is what we take to
be the correct hypothesis for a learning target. In the case of simple
experiments, like the kinship relations experiments in `tiny_kinship.pl`, the
target theory for each learning target, `T`, is known and so we can use it as
background knowledge for other targets, or to generate positive and negative
examples for `T` itself. However, that will not always be the case and positive
and negative examples will have to be obtained elsewhere.

If you have lists of positive and negative examples that you want to make
available through an examples generator, you can use member/2 to output each
example in the list nondeterministically:

```prolog
% A positive examples generator using a list of examples.
positive_example(p/2,E):-
	member(E, [p(a,b)
		  ,p(b,c)
		  ,p(c,d)
		  ]).

% A negative examples generator using a list of examples.
negative_example(p/2,E):-
	member(E, [p(b,a)
		  ,p(c,b)
		  ,p(d,c)
		  ]).
```

Note that, while the negative examples returned by `experiment_data/5` are
ground Horn goals, i.e. atoms prefixed by `:-`, the negative examples generator
should only return positive atoms, i.e. atoms _not_ prefixed with `:-`. Atoms
returned by `negative_example/2` are returned as Horn goals by
`experiment_data/5`.

### Background knowledge

The interface predicate `background_knowledge/2` declares the predicates to be
used as background knowledge for a learning target. We repeat here the
`background_knowledge/2` declaration for `ancestor/2` in `tiny_kinship.pl` for
ease of reference:

```prolog
% Background knowledge declaration for ancestor/2
background_knowledge(ancestor/2,[father/2,mother/2,parent/2]).
```

The first argument of `background_knowledge/2` is the symbol and arity of a
learning target. The second argument is a list of the names and arities of the
background predicates for that learning target. The names and arities in the
list in the second argument of `background_knowledge/2` are used to find and
collect the clauses of the background predicates for the learning target in its
first argument.

Louise will look for the definitions of background knowledge predicates listed
in the second argument of `background_knowledge/2` in module `user` (the
top-level module in Swi-Prolog's module system; every other module imports
module `user`). For the definitions of background predicates to be accessible by
Louise, they must be exported to module `user`. The simplest way to do this is
to add those predicates to the export list of the experiment file module.
Louise experiment file loading procedures will handle the rest.

For example, if you look at the export list of the `tiny_kinship.pl` experiment
file you will find that each predicate declared as background knowledge for one
of the learning targets in that experiment file is exported, along with the
experiment file interface predicates:

```prolog
% Export list of tiny_kinship.pl.
:-module(tiny_kinship, [% Experiment file interface predicates
			background_knowledge/2
		       ,metarules/2
		       ,positive_example/2
		       ,negative_example/2
		       % Exported background predicates
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
```

Note that in the case of `tiny_kinship.pl` most of the predicates declared as
background knowledge for a learning target are also, themselves, learning
targets. This will not raise any errors and you can do the same in your own
experiment files if you need to.

For small experiments, like `tiny_kinship.pl`, it will be most convenient to
define background predicates in the experiment file itself and add them to the
export-list of the experiment file module, as above. For larger experiments it
may be necessary to import additional files into the experiment file, or
directly into module `user`.

Refer to the Swi-Prolog documentation module for more details on using modules.

### Metarules

MIL systems like Louise need a set of _metarules_ to perform learning. Metarules
for a learning target are declared with a clause of the experiment file
interface predicate, `metarules/2`. We repeat here the `metarules/2` declaration
for `ancestor/2` in `tiny_kinship.pl` listed earlier, for ease of reference:

```prolog
% Metarules for ancestor/2
metarules(ancestor/2,[tailrec,identity]).
```

The first argument of `metarules/2` is the name of a learning target and the
second argument is a list of metarule _identifiers_.

Similar to the predicate symbols of background predicates declared for a
learning target in a `background_knowledge/2` clause, metarule identifiers are
used by Louise to find the definitions of metarules in Prolog's dynamic
database.

For example, given the metarule declaration above, and given that the
definitions of the two metarules, _Tailrec_ and _Identity_ are in the program
database, Louise's learning predicates will find the following definitions of
the two metarules:

```prolog
% Definitions of tailrec and identity metarules.
metarule(tailrec,P,Q,P):- m(P,X,Y), m(Q,X,Z), m(P,Z,Y).
metarule(identity,P,Q):- m(P,X,Y), m(Q,X,Y).
```

Definitions of metarules used by Louise, like, _Tailrec_ and _Identity_ above,
are in Louise's _encapsulated_ form. This is discussed in more detail in the
following section.

A number of standard metarules from the MIL bibliography are defined in Louise's
configuration file, `louise/configuration.pl`. For example, the following are
some metarules defined in the configuration file, `configuration.pl`:

```prolog
% Some metarules from the configuration:
metarule(abduce,P,X,Y):- m(P,X,Y).
metarule(identity,P,Q):- m(P,X,Y), m(Q,X,Y).
metarule(inverse,P,Q):- m(P,X,Y), m(Q,Y,X).
metarule(chain,P,Q,R):- m(P,X,Y), m(Q,X,Z), m(R,Z,Y).
metarule(tailrec,P,Q,P):- m(P,X,Y), m(Q,X,Z), m(P,Z,Y).
metarule(precon,P,Q,R):- m(P,X,Y), m(Q,X), m(R,X,Y).
metarule(postcon,P,Q,R):- m(P,X,Y), m(Q,X,Y), m(R,Y).
metarule(switch,P,Q,R):- m(P,X,Y), m(Q,X,Z), m(R,Y,Z).
```

Metarules defined in the configuration file can be used immediately by adding
their names to the second argument of a `metarules/2` declaration for a learning
target.

The metarules declared for `ancestor/2`, _tailrec_ and _identity_, are defined
in the configuration file, `configuration.pl`, along with other common metarules
that are useful for learning many different targets.

### Defining your own metarules.

If you have an intuition about the structure of the hypothesis that you want
Louise to learn, you may wish to define your own metarules that better match
this structure, compared to the ones given in the configuration. You can do this
by adding your own metarules in the configuration file, or in an experiment
file.

Unlike background predicates, metarules are all exported by the _configuration_
module. For this reason, if you wish to declare your own metarules in an
experiment file, you have to prefix each metarule/n clause with the
configuration module's module qualifier, 'configuration'. 

The following example of a user-provided metarule is taken from the example
experiment file `data/examples/special_metarules.pl`:

```prolog
% Defining a new metarule
:-dynamic m/3.
configuration:metarule(special_chain,P,Q,R):- m(P,X,Y), m(Q,X,Z), m(R,Z,Y).
```

The predicate symbols and arities of literals in the body of metarules declared
in an experiment file must also be declared dynamic, as `m/3`, above.

Refer to the documentation of the example experiment file
`data/examples/special_metarules.pl` for more details on how to declare your own
metarules in an experiment file.

A following section gives a high-level overview of metarules and how they are
used in MIL, and Louise.

### Experiment file compatibility

The experiment file format used in Louise is identical with that used in
[Thelma]. Thelma is an implementation, by Louise's author, of the original MIL
learner, [Metagol]. Given that Thelma and Louise both understand the same
experiment files, it is easy to run the same experiment in both learners: simply
copy the experiment file from the `data/` directory of one, to that of the other
(Thelma and Louise also have similar directory structures).

Note that while Thelma and Louise understand the same experiment file format
they differ in subtle ways otherwise. Importantly, Thelma and Louise have
different internal representations for metarules. While both Thelma and Louise
will understand the same `metarule/2` declaration, user-defined metarules
added to an experiment file in the format used in Thelma will not be usable by
Louise, and vice-versa.

If you want to define your own metarules in an experiment file to use in
experiments with both Thelma and Louise, there are two options.

The first option is to add two definitions of the new metarule in the experiment
file, one in Thelma's representation and one in Louise's. For example you could
add both of the following in the same experiment file and share it between
Thelma and Louise:

```prolog
% User-defined metarule in Thelma's format
configuration:metarule(special_chain, [P,Q,R], [X,Y,Z], (mec(P,X,Y) :- mec(Q,X,Z), mec(R,Z,Y))).
% Order constraints for user-defined metarule in Thelma.
configuration:order_constraints(special_chain,[P,Q,R],_Fs,[P>Q,P>R],[]).

% The same metarule in Louise's format
:-dynamic m/3.
configuration:metarule(special_chain,P,Q,R):- m(P,X,Y), m(Q,X,Z), m(R,Z,Y).
```

The second option is to add the definitions of the new metarules to each
learner's configuration file. Thelma and Louise can then find the new metarules
from their identifiers in a `metarule/2` declaration.

The second option is the simplest and most straightforward, but you run the risk
of "leaving behind" your metarule definitions as you change an experiment file,
or deleting them by mistake when editing configuration files. The first option
is more bothersome, but ensures that your metarules go with the rest of your
experiment data.

Learning with second-order metarules
------------------------------------

Briefly, metarules are _second-order definite datalog_ clauses that define the
language of hypotheses for a learning target in Louise, and MIL in general.
Metarules will be explained in depth in the upcoming Louise manual. The
following is a short introduction.

That metarules are second-order means that some of their variables have values
that range over the set of predicate symbols, the _predicate signature_. That
they are definite clauses means that they have no literals negated by
negation-as-failure (.e.g `\+ p(x)` etc). That they are datalog means that the
language of metarules is a subset of Prolog with no functions of arity more than
0 (i.e. they have only constants). This is in principle; in practice such
restrictions are not strictly enforced in Louise, but definite datalog offers
some theoretical guarantees about learnability that are nice to have. These are
discussed in the MIL literature and in the upcoming Louise manual.

The following are some common metarules from the MIL literature:

```prolog
% Examples of metarules from the MIL literature
P(X,Y)  % Abduce
P(x,y):- Q(x,y) % Identity
P(x,y):- Q(y,x) % Inverse
P(x,y):- Q(x,z), R(z,y) % Chain
P(x,y):- Q(x), R(x,y) % Precon
P(x,y):- Q(x,y), R(y) % Postcon
```

In the listing above, `P,Q,R` are second-order, existentially quantified
variables, `X,Y` are first-order, existentially quantified variables and `x,y,z`
are first-order, universally quantified variables. `P,Q,R` take values from the
predicate signature while `X,Y,x,y,z` take values from the _constant signature_,
the set of constants in the background knowledge and examples given in a MIL
problem.

### Encapsulation of second-order metarules

Given that metarules are second-order they cannot be used directly in a
first-order logic language like Prolog. Instead, Louise represents metarules in
an _encapsulated_ form, as first-order clauses. The following is the list of
metarules above, in Louise's internal representation:

```prolog
% Examples of encapsulated metarules in Louise's notation.
metarule(abduce,P,X,Y):- m(P,X,Y).
m(identity,P,Q):- m(P,X,Y), m(Q,X,Y).
m(inverse,P,Q):- m(P,X,Y), m(Q,Y,X).
m(chain,P,Q,R):- m(P,X,Y), m(Q,X,Z), m(R,Z,Y).
m(precon,P,Q,R):- m(P,X,Y), m(Q,X), m(R,X,Y).
m(postcon,P,Q,R):- m(P,X,Y), m(Q,X,Y), m(R,Y).
```

In the encapsulation of the _Identity_ metarule listed above, the head literal,
`metarule(identity,P,Q)` is a _metasubstitution atom_ where `identity` is the
metarule's identifier and `P,Q` are second-order, existentially quantified
variables, whereas `X,Y,Z` are first-order, universally quantified variables. In
the encapsulation of the _Abduce_ metarule, `abduce` is the metarule's
identifier, `P` is a second-order, existentially quantified variable, whereas
`X,Y` are first-order, existentially quantified variables. In other words, in
this encapsulated form, existentially quantified variables, of either order, are
arguments of the metasubstitution atom.

A _metasubstitution_ is a substitution of existentially quantified variables in
a metarule. Existentially quantified variables in metarules may be first- or
second-order. Second order, existentially quantified variables are substituted
for symbols in the predicate signature, so they take values from the set of
predicate symbols of the target predicate and predicates in the background
knowledge (and also _invented predicates_, discussed later). First-order
existentially quantified variables are substituted for constants in the
_constant signature_, the set of constants in the background knowledge and
examples.

In practice what all this mens is that existentially quantified, second-order
variables in metasubstitution atoms become the predicate symbols of clauses in a
learned hypothesis and existentially quantified, first-order variables become
constants in clauses of a learnerd hypothesis. An example of this is given in a
following section.

### Expanded metarules

The perceptive reader will have noticed that what we call a "metarule" in
Louise's internal notation is actually a clause representing two separate
things: a metasubstitution of the existentially quantified variables in the
metarule, given as the metasubstitution atom in the head literal of the clause,
and the literals of the metarule itself given as the encapsulated body literals
of the clause. To avoid the confusion that using the term "metarule" to refer to
this complex data structure can cause, we will use the following terminology.

We will refer to a metarule in Louise's internal notation as an _expanded_
metarule. The metasubstitution atom at the head literal of an expanded metarule
clause will be the _metasubstitution atom_ of the expanded metarule. The body
literals of an expanded metarule clause will be the _encapsulated literals_ of
the metarule. The first body literal of an expanded metarule clause will be the
_encapsulated head literal_ of the metarule; and remaining body literals, after
the encapsulated head literal, will be the _encapsulated body literals_ of the
metarule.

The following listing illustrates the terminology with an example.

```prolog
% Parts of the expanded Chain metarule
m(chain,P,Q,R):-		% Metasubstitution atom
	m(P,X,Y), 		% Encapsulated head literal of Chain
	m(Q,X,Z), m(R,Z,Y).	% Encapsulated body literals of Chain
```

Note that this internal representation is different than the notation used for
metarules declared in the configuration, discussed in the previous section.
Metarules declared in the configuration have the predicate symbol `metarule`.
This is a bit of syntactic sugar to help make metarules easier to read and
write. You should always use the syntactic sugar'd cofiguration notation to
declare your own metarules, otherwise they may not be picked up by Louise.

### Applying metasubstitutions to metarules

Metasubstitution atoms are a compact way to store the clauses of a hypothesis
learned by Louise. A ground metasubstitution atom can be applied to its
corresponding metarule simply by collecting the encapsulated body literals of
the expanded metarule and joining the head literal to the body literals by the
Prolog "neck" symbol, `:-`, thus forming a definite clause. Each clause in a
hypothesis learned by Louise is the result of applying a metasubstitution to its
corresponding metarule in this manner.

For instance, suppose that, in the process of learning a hypothesis for
`ancestor/2`, Louise's learning procedure has obtained the metasubstitution
`m(identity,ancestor,parent)`. Note that this is a _ground_ metasubstitution, in
the sense that the existentially quantified variables in the metasubstitution
atom are unified to predicate symbols in the background knowledge for
`ancestor/2` (i.e. "parent") and the predicate symbol of "ancestor" itself.

Given that ground metasubstitution, Louise can apply it to the _Identity_
metarule, matching its identifier, and produce the following definite clause:

```prolog
% An applied metasubstitution
m(ancestor,A,B):- m(parent,A,B)
```

Of course, the result of applying a metasubstitution to the encapsulated
literals of a metarule in this way results in an _encapsulated_ first-order
clause. In fact, the resulting clause has the predicate symbol `m`. We normally
want a clause of a hypothesis to have the predicate symbol of a learning target.
Such clauses are obtained with a further transformation, an _excapsulation_ of
encapsulated clauses in a hypothesis. Given that the result of applying a ground
metasubstitution atom to its corresponding metarule is a first order clause,
excapsulation is straightforward. Excapsuating the clause listed above gives us
the following definite clause of `ancestor/2`:

```prolog
% An excapsulated clause
ancestor(A,B):- parent(A,B)
```

Application of metasubstitutions to their corresponding metarules and
excapsulation of the resulting clauses is handled internally by Louise's
learning procedures. There is no need for the user to do any of that. However,
it is useful to understand the process to get an idea of how metarules "work" in
Louise.

Learning by Top program construction and reduction
--------------------------------------------------

Louise learns by constructing the set of all clauses in correct hypotheses that
can be formed by the predicates defined in the background knowledge and the
metarules for the MIL problem. We will call this set of clauses the _Top
program_, because it is the most general program that is correct with respect to
the training examples. In that sense, it is the "top" of the entailment lattice
of hypotheses in the hypothesis space defined by the MIL problem. 

The "hypothesis space" is the set of all hypotheses that can be formed by the
background knowledge and metarules. Hypotheses are sets of clauses and MIL
systems must "solve" a MIL problem by returning a hypothesis that is correct
with respect to the training examples. A hypothesis is "correct" with respect to
the training examples iff, given the background knowledge, it entails all of the
positive examples and does not entail any of the negative examples.

As stated above, the Top program for a MIL problem is the most general
hypothesis that is correct with respect to the training examples. The Top
program is more general than any other hypothesis in the hypothesis space in the
sense that every other hypothesis is a subset of the clauses of the Top program.
That means that the Top program is not only the most general, but also the
largest hypothesis, i.e. the one with the greatest number of clauses.

Ironically, the Top program is easier to obtain than any of the hypotheses in
the hypothesis space, that are its subsets. This is because the Top program is
the set of clauses in all hypotheses and the hypothesis space is the powerset of
this set. In fact, the Top program for a MIL problem can be assembled in time
polynomial in the number of clauses in all hypotheses that can be formed with
the predicates in the background knowledge and the metarules, while searching
the hypothesis space for one correct hypothesis can, in the worst case, take
time exponential to the number of clauses.

This relation, between the size of the Top program and the hypothesis space is
the source of Louise's efficiency compared to the original MIL learner, [Metagol].
[Metagol] conducts a search of the hypothesis space for one hypothesis that
explains all the examples. This search of the hypothesis space takes time
proportional to the size of the hypothesis space, i.e. the powerset of the
clauses in all hypotheses.

As stated above, the Top program is the largest hypothesis in the hypothesis
space of a MIL problem. In a sense, the Top program is an "overhypothesis"
encompassing multiple sub-hypotheses. There are several reasons why we might
want to select a sub-hypothesis from the Top program:

  1. The Top program may include redundant clauses.
  2. It is easier to store a smaller program than a large program.
  3. A shorter program may require fewer computational resources to execute.
  4. A shorter program may be easier to read and understand than a large
     program.

With this in mind, Louise follows Top program construction with a _reduction_
step, during which the Top program construction is reduced in size.

There are two reduction procedures currently available in Louise: reduction by
Plotkin's program reduction algorithm; and reduction by sub-hypothesis
selection.

The two reduction methods can be chosen by setting the configuration option
`reduction/1` to one of the following values: `[plotkins, subhypothesis]`. For
example:

```prolog
% Select Top program reduction by Plotkin's program reduction algorithm:
reduction(plotkins).

% Select Top program reduction by sub-hypothesis selection:
reduction(subhypothesis).
```

Remember to set only one of the two `reduction/1` values above, i.e. leave one
of them commented-out or only add one to the configuration file.

### Top program reduction by Plotkin's algorithm

Plotkin's program reduction algorithm, first defined in Gordon Plotkin's
doctoral thesis [(Plotkin, 1972)] removes redundant claues from a logic program.
Redundant clauses are clauses that are subsumed by the rest of the program.
Plotkin's algorithm is defined as follows:

```
% H is a set of arbitrary clauses.
% H' is the reduction of H
1) Set H' to H.
2) Stop if every clause in H' is marked.
3) Choose an unmarked clause C, in H.
4) If H' \{C} subsumes {C} then change H' to H' \{C}. Otherwise, mark C.
5) Go to 2).
```

Plotkin's algorithm deals with arbitrary logic clauses, but our implementation
only allows for Horn clauses. In principle, the Top program learned by Louise
consists of only definite clauses. In practice, the Top program may include
arbitrary program clauses (i.e. Horn clauses including negation as failure).

Reducing the Top program by Plotkin's algorithm is deterministic, in the sense
that, given the same Top program, it will always return the same reduced
hypothesis.

### Controlling reduction strength

In Louise, Plotkin's program reduction is implemented by means of a Prolog
meta-interpreter. The meta-interpreter imposes a limit on the number of
derivations in order to avoid infinite recursion. This limit is set in the
configuration option `resolutions/1` (it is slightly misnamed):

```prolog
% Unfortunately named resolutions/1 option 
resolutions(5000).
```

The `resolutions/1` option must be set manually and it's often necessary to
discover its optimal setting by a process of trial-and-error. A too-low setting
results in some positive examples being included in the learned hypothesis
(normally, Plotkin's algorithm will discard positive examples as redundant,
given that they are entailed by the learned hypothesis). The following example
illustrates this with an attempt to learn `grandmother/2`, defined in
`tiny_kinship.pl`, when `resolutions/1` is set to "0" (effectively turning
reduction off):

```prolog
% Result of a too-low resolutions/1 setting
% resolutions/1:
resolutions(0).

% Learning result
?- learn(grandmother/2).
grandmother(alexandra,stassa).
grandmother(paraskevi,stassa).
grandmother(A,B):-mother(A,C),mother(C,B).
grandmother(A,B):-mother(A,C),parent(C,B).
true.
```

In the above listing, the first two, ground atoms of `grandmother/2` are
positive examples entailed by the following two non-unit clauses, however these
two positive examples were not removed by Plotkin's algorithm because of the low
setting of `resolutions/1`.

Such un-reduced, redundant positive examples are referred to as _atomic
residue_.

The result of learning `grandmother/2` with a higher `resolutions/1` setting is
listed below: 

```prolog
% Result of a sensible resolutions/1 setting
% resolutions/1:
resolutions(5000).

% Learning result
?- learn(grandmother/2).
grandmother(A,B):-mother(A,C),parent(C,B).
true.
```

Note that this time, not only atomic residue is gone, but so is the clause
`grandmother(A,B):-mother(A,C),mother(C,B)`, previously included in the learned
hypothesis. This clause is redundant given the clause
`grandmother(A,B):-mother(A,C),parent(C,B)` and so it is excluded from the
learned hypothesis now that the `resolutions/1` setting is sufficiently high.

The trade-off between a low and high `resolutions/1` setting is that, the higher
the setting, the longer reduction can take (especially for recursive
predicates). Conversely, the lower the setting, the more the atomic residue.

To alleviate the burden of having to find an optimal `resolutions/1` setting to
maximally reduce the Top program, Louise implements _recursive reduction_. In
recursive reduction, the reduced Top program is passed again to the
implementation of Plotkin's reduction, until its cardinality stops changing.

Recursive reduction can reduce the time it takes to reduce the Top program and
can result in a stronger reduction (i.e. resulting in a smaller hypothesis) than
when not using recursive reduction.

Recursive reduction is enabled by setting the configuration option
`recursive_reduction/1` to "true":

```prolog
% Enabling recursive reduction
recursive_reduction(true).
```

Note that enabling recursive reduction has no effect on reduction by
sub-hypothesis selection, discussed in the next section.

### Top program reduction by sub-hypothesis selection

Plotkin's program reduction algorithm can only remove a clause `C` from the Top
program when `C` is _logically_ redundant, i.e., when `C` is subsumed by the
rest of the program. This may still leave behind _inductively_ redundant
clauses, i.e. clauses that entail the same set of positive examples as other
clauses in the Top program.

For example, listed below is the result of learning the predicate
`grandfather/2`, defined in the `tiny_kinship.pl` experiment file, when
`reduction(plotkins)` is set in the configuration:

```prolog
% grandfather/2 reduced by Plotkin's program reduction
?- learn(grandfather/2).
grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).
true.
```

Given the MIL problem for `grandfather/2` defined in `tiny_kinship.pl`, both of
the clauses in the learned hypothesis are correct and neither of them is
subsumed by each other. Therefore, Plotkin's algorithm cannot reduce this
hypothesis further.

A hypothesis including inductively redundant clauses can be further reduced by
sub-hypothesis selection, the second reduction method used by Louise. The
sub-hypothesis selection algorithm implemented in Louise is as follows:

```
% H is a set of Horned clauses, a learned hypothesis 
% B is a set of background knowledge definitions
% E+ is the set of positive examples in a MIL problem.
% H' is the reduction of H
1) Set H' to {}
2) Select a clause, C, in H, at random and without replacement
3) Find the set E'+ such that B ∪ H entails each e+ in E'+
4) Add C to H', a sub-hypothesis of H
5) Set H to H \ {C} and E+ to E+ \ E'+
6) Repeat from (2)
7) When E+ = {} or H = {}, return H'
```

Sub-hypothesis selection is stochastic, in the sense that each time the same Top
program is reduced by sub-hypothesis selection, a different reduction may be
returned (this is the result of the random selection of a clause, C, in step (1)
in the sub-hypothesis selection algorithm listed above).

For example, listed below are the results of four successive attempts to learn
the predicate `grandfather/2` as in the previous example, but this time with
`reduction(subhypothesis)` set in the configuration:

```prolog
% Reductions of grandfather/2 by subhypothesis selection
?- learn(grandfather/2).
grandfather(A,B):-husband(A,C),grandmother(C,B).
true.

?- learn(grandfather/2).
grandfather(A,B):-father(A,C),parent(C,B).
true.

?- learn(grandfather/2).
grandfather(A,B):-husband(A,C),grandmother(C,B).
true.
```

Note that each learning attempt returns a different hypothesis- this is the
result of stochastic reduction of the Top program by sub-hypothesis selection.

Although this is not clear from the above example, a sub-hypothesis may still
include _logically_ redundant clauses.

In general, there is a trade-off between the two reduction methods. Reduction by
Plotkin's algorithm can be computationally costly, because determining
subsumption is an expensive procedure (Louise's implementation actually tests
_entailment_ which is undecidable for arbitrary clauses and expensive even for
definite datalog clauses). Sub-hypothesis selection is efficient and can reduce
a large Top program faster than our implementation of Plotkin's (entailment is
decided in sub-hypothesis selection by instantiating the head of a clause to a
positive example and resolving its partially-instantiated body literals against
the background knowledge and positive examples). However, sub-hypothesis
selection is not guaranteed to be complete, or, in other words, it's possible
that a sub-hypothesis will include _only some_ of the correct clauses in the Top
program.

Better Top-program reduction procedures may become available in the future. In
that case, they will be incorporated in Louise.

Using Louise: useful auxiliary predicates
-----------------------------------------

The following are some auxiliary predicates, mostly defined in
`louise/src/auxiliaries.pl`, that can be useful to setup and debug a MIL
problem or a learning attempt, etc.

### Learning predicate variants

Besides `learn/1` exemplified in the worked example at the start of this README
file, there are another two learning predicates, variants of `learn/1` accepting
different sets of arguments.

`learn/2` is a variant of `learn/1` that takes as its first, input, argument,
the predicate symbol and arity of a learning target defined in the currently
loaded experiment file, identically to `learn/1`. Rather than print the learned
hypothesis to the Swi-Prolog top-level, like `learn/1` does, `learn/2` outputs
the learned hypothesis bound to its second argument:

```prolog
% Calling learn/2
?- learn(ancestor/2, _Ps), print_clauses(_Ps).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
true.
```

`learn/2` is useful when you wish to do some further processing to a learned
hypothesis, such as write to a file, etc.

A third variant of `learn/1` is `learn/5`. This takes as input arguments lists
of ground positive and negative example atoms, predicate symbols and arities of
background knowledge predicates and identifiers of metarules. It outputs bound
to its last argument the hypothesis learned from these elements of a MIL
problem:

```prolog
% Calling learn/5
?- experiment_data(ancestor/2,_Pos,_Neg,_BK,_MS), learn(_Pos,_Neg,_BK,_MS,_Ps), print_clauses(_Ps).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
true.
```

In the example listed above we have used `experiment_data/5` to collect the MIL
problem elements defined in the currently loaded experiment file for the
learning target `ancestor/2`. In practice, you will want to pass in different
sets of examples, background predicate indicators and metarule identifiers,
rather than the ones already defined in an experiment file. For instance, one
use of `learn/5` is to perform experiments with varying sub-samples of examples,
or background predicates, etc.

### Extracting metarules from a program's structure

One typical problem when working with MIL-learners is how to choose the
metarules for a MIL problem.

One option, implemented by Louise, is to derive metarules from the structure of
the clauses of existing Prolog programs. The predicate `program_metarules/2` can
be used to extract metarules from a list of clauses:

```prolog
% Extracting metarules from a list of clauses
?- _Ps = [p(_A,_B):- q(_A,_C), r(_C,_B)], program_metarules(_Ps,_MS), print_metarules(_MS).
m(metarule_1,P,Q):-m(P,X,Y),m(Q,X,Z)
true.
```

`program_metarules/2` takes as an input argument a list of program clauses and
outputs in its second argument a list of metarules such that each clause in the
input list is an insantiation of one of the metarules in the output list.
Metarules output by `program_metarules/2` have automatically assigned names and
the user will probably want to change them to something more meaningful,
perhaps, or at least more easily identifiable.

Alternatively, if the target program is loaded in program memory, metarules can
be extracted from its clauses by a call to `symbols_metarules/3`. This takes as
a first argument a list of predicate symbols and arities of predicates in the
program and as a second argument the name of a module whence this program is
visible. The last, output, argument is again bound to a list of metarules
extracted from the clauses in the definitions of the predicates referenced in
the first input argument. The following calls `symbols_metarules/3` passing a
reference to itself:

```prolog
% Extracting metarules from a list of predicate symbols
?- symbols_metarules([symbols_metarules/3],user, _MS), print_metarules(_MS).
m(metarule_1,P,Q,R):-m(P,X,Y,Z),m(Q,X,Y,U),m(R,U,Z)
true.
```
Any Prolog program can be used to extract metarules in this way, but of course
it makes more sense to use programs with common structures, or that are
generally useful. Such programs can be found online, in websites such as [99 Prolog programs](https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/)
or textbooks and scholarly articles, such as the Prolog _skeletons_ listed in
[(Shapiro, 2002)].

In particular, useful metarules may be extracted from the structure of
background predicates. Louise automates this procedure with the predicate
`bk_metarules/2`. An example query follows, where the current experiment file is
`tiny_kinship.pl`:

```prolog
% Extracting metarules from background predicate definitions
?- bk_metarules(grandfather/2, _MS), print_metarules(_MS).
m(metarule_1,P,Q):-m(P,X,Y),m(Q,X,Y)
m(metarule_2,P,Q,R):-m(P,X,Y),m(Q,X,Z),m(R,Y,Z)
m(metarule_3,P,Q,R):-m(P,X,Y),m(Q,X,Z),m(R,Z,Y)
true.
```

The first argument of `bk_metarules/2` is the predicate symbol and arity of a
learning target defined in the currently loaded configuration file. Its second,
output, variable is bound to a list of metarules extracted by that background
predicate's definition in the loaded experiment file.

Intuitively, the structure of clauses in definitions of background knowledge
predicates is likely to be similar to the structure of clauses in the learned
hypothesis. This intuition is encapsulated in `bk_metarules/2`.

### Listing configuration options

To quickly inspect the configuration options currently loaded in memory, the
predicate `list_config/0` can be used at the Swi-Prolog top-level:

```prolog
% Listing the currently loaded configuration options
?- list_config.
dynamic_generations(1)
experiment_file(data/examples/tiny_kinship.pl,tiny_kinship)
learner(louise)
max_invented(1)
recursion_depth_limit(dynamic_learning,500)
recursive_reduction(false)
reduction(subhypothesis)
resolutions(5000)
symbol_range(predicate,[P,Q,R,S,T])
theorem_prover(resolution)
true.
```

### Restoring defaule configuration options

Configuration options defined in `louise/configuration.pl` have a set of
"default" values, that are designed to allow the examples in
`louise/data/examples/` to be run without errors. The default configuration
options can be restored by a call to the predicate `reset_defaults/0`. 

```prolog
% Resetting configuration options to default values
?- reset_defaults.
true.
```

Following a call to `reset_defaults/0`, `list_config/0` should list the default
configuration options.

Note that calling `reset_defaults/0` will only modify the values of
configuration options currently loaded in the program's memory. It will _not_
change the contents of the configuration file. Reloading the configuration file
after calling `reset_defaults/0` will again load the configuration options
defined in the configuration file.

Default configuration options are stored in the file `louise/src/defaults.pl`.
It is advisable to keep those safe and not change them. 

### Cleaning up an experiment

Regrettably, some of the predicates in Louise use impure Prolog predicates such
as `assert/1` and `retract/1` to manipulate the Prolog dynamic database. In
particular, such database manipulation predicates are used to make the
components of a MIL problem defined in an experiment file available to Louise's
learning procedure. This has the unfortunate effect that, if learning fails for
some reason, garbage may be left behind and the dynamic database may be left in
an unsafe state. A hint that this has occurred is either infinite recursion
following a call to `learn/1` and friends, or strange, disturbing errors spewing
out in shocking bright red at the Swi-Prolog top-level.

If you find yourself unable to make any progress with Louise and you keep
getting weird errors, or things keep going infinite, the safest thing to do is
to exit the Swi-Prolog session and start again. If you don't want to restart
your session for some reason, you can use the predicate `cleanup_experiment/0`
to remove garbage left behind by failed learning attempts from the dynamic
database:

```prolog
% Cleaning up after a failed learning attempt
?- cleanup_experiment.
true.
```

### Listing learning targets

A common error when using Louise is to try and learn a predicate that is not
defined as a learning target in the current experiment file. This may be because
you have loaded the wrong experiment file, or because you have pressed Up-Arrow
to go back in the Swi-Prolog console's history and re-enter an earlier learning
query.

When Louise cannot find the learning target given in a learning predicate it
will raise an error, as in the following listing:

```prolog
% Attempting to learn an unknown learning target
?- learn(undefined_learning_target/100).
ERROR: Unhandled exception: 'Unknown learning target':undefined_learning_target/100
?-
```

You can check what the current experiment file is by a call to `list_config/0`,
as described in an earlier section.

You can inspect the learning targets defined in the current experiment file by
looking in the experiment file itself, of course. Alternatively, you can call
the predicate `learning_targets/1`:

```prolog
% Listing learning targets
?- learning_targets(Ts).
Ts = [ancestor/2, grandparent/2, grandfather/2, grandmother/2, parent/2, husband/2, wife/2, child/2, ... / ...|...].
```

A query to `learning_targets/1` will output a list of the predicate symbols and
arities of predicates defined as learning targets in the current experiment
file, in particular, those having a `background_knowledge/2` declaration with
their predicate symbol and arity as the first argument. As above, if the output
list is long you may not see the entire list in the Swi-Prolog console. In that
case, pass the list to a printing predciate, like `writeln/1`:

```prolog
% Printing list of learning targets
?- learning_targets(_Ts), writeln(_Ts).
[ancestor/2,grandparent/2,grandfather/2,grandmother/2,parent/2,husband/2,wife/2,child/2,son/2,daughter/2,father/2,mother/2,male/2,female/2]
true.
```

### Listing learning results for all learning targets

Another use of the predicate `learning_targets/1` described in the previous
section is to test each of the learning targets defined in an experiment file,
successively:

```prolog
% Learning all known learning targets
?- learning_targets(_Ts), member(_T, _Ts), learn(_T).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
true ;
grandparent(A,B):-parent(A,C),parent(C,B).
true ;
grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).
true .
% ... more rersults
```

This functionality is automated in the predicate `list_learning_results/0`:

```prolog
% Listing the results of learning with all known learning targts
?- list_learning_results.
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).

grandparent(A,B):-parent(A,C),parent(C,B).

grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).

grandmother(A,B):-mother(A,C),parent(C,B).

parent(A,B):-father(A,B).
parent(A,B):-mother(A,B).

husband(A,B):-father(A,C),mother(B,C).

wife(A,B):-mother(A,C),father(B,C).

child(A,B):-daughter(A,B).
child(A,B):-son(A,B).

son(A,B):-male(A),child(A,B).

daughter(A,B):-female(A),child(A,B).

father(A,B):-male(A),parent(A,B).

mother(A,B):-female(A),parent(A,B).

male(A,A):-male(A).

female(A,A):-female(A).

true.
```

`list_learning_results/0` is particularly useful to test the effects of changing
one or more confirguration options. Changing some configuration options may not
affect some learning targets, while it may make a big difference for others.
Listing the results of learning for all targets in an experiment file can help
expose such differences.

### List all the metarules known to the system

The predicate `known_metarules/1` can be used to list all the metarules known to
Louise. These include all the metarules defined in the configuration, plus all
the user-defined metarules added to the currently loaded experiment file:

```prolog
% Listing all known metarules
?- known_metarules(MS).
MS = [abduce, chain, double_identity, identity, inverse, postcon, precon, projection, projection_12|...].
```

As with `learning_targets/1` the list in the output of `known_metarules/1` may
be too long and be replaced by ellipses in the Swi-Prolog console. In that case,
you can print it out in its full glory with a call to an appropriate printing
predicate.

### Pretty-printing logic prorgams

The learning predicate variant `learn/2` takes as input the name of a learning
target and outputs a learned hypothesis. A learned hypothesis is, of course, a
set of clauses and may be difficult to read in list form, especially given the
temporary "names" assigned to variables by the Prolog top-level:

```prolog
% Output of learn/2
?- learn(grandfather/2, Ps).
Ps = [(grandfather(_10954, _10956):-father(_10954, _10970), parent(_10970, _10956)),  (grandfather(_10912, _10914):-husband(_10912, _10928), grandmother(_10928, _10914))].
```

To pretty-print a set of clauses, such as a hypothesis returned by `learned/2`,
you can use the predicate `print_clauses/1`:

```prolog
% Pretty printing the output of learn/2
?- learn(grandfather/2, _Ps), print_clauses(_Ps).
grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).
true.
```

The variant `print_clauses/2` may be used to prepend a message to the
pretty-printed clauses:

```prolog
% Pretty printing with an accompanying message
?- learn(grandfather/2, _Ps), print_clauses('% Learned from tiny_kinship.pl', _Ps).
% Learned from tiny_kinship.pl
grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).
true.
```

This may be useful when you want to copy the output of `print_clauses/2` and
paste it into a file for sharing or safekeeping etc, and you want to associate a
friendly explanatory message to the learned hypothesis.

### Pretty-printing metarules

In the previous section, the use of `print_clauses/[1,2]` was used to
pretty-print a learned hypothesis. Any arbitrary set of clauses can be printed
in this way, however printing metarules with `print_clauses/2` results in
metarules that are not that easy to read:

```prolog
% Printing metarules
?- expanded_metarules([chain],_MS), print_clauses(_MS).
m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E).
true.
```

Although there is nothing wrong with the above-listed output, note that the
variables in the metarule are printed with names that correspond to their order
of appearance in the expanded metarule clause. Normally, we would prefer to see
variables take names according to their order and quantification. This is common
when notating metarules (or any clause, in general) in the literature.

Louise provides two predicates to pretty-print metarules with more appropriate
variable names: `print_metarules/1` and `print_quantified_metarules/1`.

`print_metarules/1` takes as argument a list of metarule identifiers and prints
out their expanded forms with variables named according to their (first- or
second-) order:

```prolog
% Pretty-printing metarules
?- expanded_metarules([chain],_MS), print_metarules(_MS).
m(chain,P,Q,R):-m(P,X,Y),m(Q,X,Z),m(R,Z,Y)
true.
```

The names used for each order of variable, first- or second-order, are defined
in a configuration option, `symbol_range/2`:

```prolog
% symbol_range/2 configuration option
symbol_range(predicate, ['P','Q','R','S','T']).
symbol_range(variable, ['X','Y','Z','U','V','W']).
```

The clause of `symbol_range/2` where the first argument is the atom "predicate"
is used to define the symbols used for second-order variables. 

The clause of `symbol_range/2` where the first argument is the atom "variable"
is used to define the symbols used for first-order variables. 

You can choose your own names for first- and second-order varaibles in metarules
pretty-printed with `print_metarules/1` by modifying the `symbol_range/2`
option. You can have some fun in this way: 

```prolog
% Being silly with the names of metarule variables
% Configuration options
symbol_range(predicate, ['Alice','Bob','Carol']).
symbol_range(variable, ['Smith','Brown','Carpenter','Miller','Green']).

% Pretty-printing
?- expanded_metarules([chain],_MS), print_metarules(_MS).
m(chain,Alice,Bob,Carol):-m(Alice,Smith,Brown),m(Bob,Smith,Carpenter),m(Carol,Carpenter,Brown)
true.
```

The second predicate used to pretty-print metarules is
`print_quantified_metarules/1`. Like `print_metarules/1`, this also takes a list
of metarule identifiers. It prints out a list of those metarules in their
second-order, quantified form. Additionally, each metarule is preceded by its
identifier capitalised in parentheses.

```prolog
% Pretty-printing metarules with quantifiers
?- expanded_metarules([chain,inverse],_MS), print_quantified_metarules(_MS).
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Inverse) ∃.P,Q ∀.x,y: P(x,y)← Q(y,x)
true.
```

Note that if you are reading this README file in your browser you may not be
able to see the quantifier and left-arrow symbols in the listing above rendered
correctly, but you should be able to see them if you open the README file in a
text editor that supports unicode.

The quantifiers and left-arrow symbols will also not render correctly in the
Swi-Prolog top-level unless you select a font capable of representing them, for
example, Courrier New.

### Listing Louise's learning procedure

In summary, Louise's learning algorithm proceeds in the following steps:

  1. Encapsulation of the MIL problem.
  2. Construction of the Top prorgam.
  3. Reduction of the Top program.
  4. Excapsulation of the reduced Top program.

Steps 1, 2 and 3 can be listed for debugging using a family of listing
predicates, described in the following sections. Step 4 is the output of the
learning procedure and so does not have a dedicated listing predicate (it's
listed, or output, by learning predicates).

#### Listing the MIL problem

The elements of a MIL problem defined for a learning target in the currently
loaded experiment file can be inspected with a call to the predicate
`list_mil_problem/1`. The following is a listing of the MIL problem for
`grandfather/2` defined in `tiny_kinship.pl`:

```prolog
% Listing a MIL problem
?- list_mil_problem(grandfather/2).
Positive examples
-----------------
grandfather(stathis,stassa).
grandfather(stefanos,stassa).

Negative examples
-----------------
:-grandfather(alexandra,stassa).
:-grandfather(paraskevi,stassa).

Background knowledge
--------------------
father/2:
father(stathis,kostas).
father(stefanos,dora).
father(kostas,stassa).

parent/2:
parent(A,B):-father(A,B).
parent(A,B):-mother(A,B).

husband/2:
husband(A,B):-father(A,C),mother(B,C).

grandmother/2:
grandmother(A,B):-mother(A,C),parent(C,B).

Metarules
---------
metarule(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E).
true.
```

### Listing the Encapsulation of a MIL problem.

The first step in Louise's learning procedure transforms a MIL problem into an
encapsulated representation. The encapsulation of a MIL problem can be
inspected with a call to the predicate `list_encapsulated_problem/1` 

The following is an example of listing the encapsulated MIL problem for
`grandfather/2` defined in `tiny_kinship.pl`:

```prolog
% Listing an encapsulated MIL problem
?- list_encapsulated_problem(grandfather/2).
Positive examples
-----------------
m(grandfather,stathis,stassa).
m(grandfather,stefanos,stassa).

Negative examples
-----------------
:-m(grandfather,alexandra,stassa).
:-m(grandfather,paraskevi,stassa).

Background knowledge
--------------------
m(grandmother,A,B):-p(mother,A,C),m(parent,C,B).
m(husband,A,B):-m(father,A,C),p(mother,B,C).
m(parent,A,B):-m(father,A,B).
m(parent,A,B):-p(mother,A,B).
m(father,kostas,stassa).
m(father,stathis,kostas).
m(father,stefanos,dora).
p(mother,alexandra,kostas).
p(mother,dora,stassa).
p(mother,paraskevi,dora).

Metarules
---------
m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E).
true.
```

#### Listing the Top program for a MIL problem

The second step in Louise's learning procedure constructs the Top prorgam. The
Top program can be inspected with a call to `list_top_program/1`.

```prolog
% Listing the Top program
?- list_top_program(grandfather/2).
Generalisation:
---------------
m(grandfather,A,B):-m(father,A,C),m(father,C,B).
m(grandfather,A,B):-m(father,A,C),m(parent,C,B).
m(grandfather,A,B):-m(husband,A,C),m(grandmother,C,B).
m(grandfather,A,B):-m(parent,A,C),m(father,C,B).
m(grandfather,A,B):-m(parent,A,C),m(parent,C,B).
Length:5

Specialisation:
---------------
m(grandfather,A,B):-m(father,A,C),m(father,C,B).
m(grandfather,A,B):-m(father,A,C),m(parent,C,B).
m(grandfather,A,B):-m(husband,A,C),m(grandmother,C,B).
Length:3
true.
```

Top program construction actually creates a set of metasubstitutions. These are
applied to their corresponding metarules to preoduce a set of definite clauses
before being handed to the Top program reduction step.

In some cases it might be useful to inspect the metasubstitutions in the Top
program before their application to their corresponding metarules. The
pre-application set of metasubstitutions can still be inspected with a call to
`list_top_program/2`, with the second parameter (the "apply" parameter) set to
"false", as follows:

```prolog
% Listing the metasubstitutions in the Top program
?- list_top_program(grandfather/2, false).
Generalisation:
---------------
m(chain,grandfather,father,father).
m(chain,grandfather,father,parent).
m(chain,grandfather,husband,grandmother).
m(chain,grandfather,parent,father).
m(chain,grandfather,parent,parent).
Length:5

Specialisation:
---------------
m(chain,grandfather,father,father).
m(chain,grandfather,father,parent).
m(chain,grandfather,husband,grandmother).
Length:3
true.
```

Calling `list_top_program/2` with the second parameter set to "true" prints the
same output as `list_top_program/1`.

#### Listing the reduced Top program

The third step in Louise's learning procedure is the reduction of the Top
program along with the MIL problem, by the reductio method determined in the
configuration option `reduction/1`.

If `reduction/1` is set to "plotkins" (i.e. reduction by Plotkin's prorgam
reduction algorithm) the reduction step can be inspected by a call to the
predicate `list_top_program_reduction/1`. 

The following is a listing of the reduction step for `grandfather/2`:

```prolog
% Listing the Top program reduction step
?- list_top_program_reduction(grandfather/2).
Program clauses:
----------------
m(grandfather,stathis,stassa)
m(grandfather,stefanos,stassa)
m(grandmother,A,B):-p(mother,A,C),m(parent,C,B)
m(husband,A,B):-m(father,A,C),p(mother,B,C)
m(parent,A,B):-m(father,A,B)
m(parent,A,B):-p(mother,A,B)
m(father,kostas,stassa)
m(father,stathis,kostas)
m(father,stefanos,dora)
p(mother,alexandra,kostas)
p(mother,dora,stassa)
p(mother,paraskevi,dora)
m(grandfather,A,B):-m(father,A,C),m(father,C,B)
m(grandfather,A,B):-m(father,A,C),m(parent,C,B)
m(grandfather,A,B):-m(husband,A,C),m(grandmother,C,B)
m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E)

Program reduction:
------------------
m(grandmother,A,B):-p(mother,A,C),m(parent,C,B)
m(husband,A,B):-m(father,A,C),p(mother,B,C)
m(parent,A,B):-m(father,A,B)
m(parent,A,B):-p(mother,A,B)
m(father,kostas,stassa)
m(father,stathis,kostas)
m(father,stefanos,dora)
p(mother,alexandra,kostas)
p(mother,dora,stassa)
p(mother,paraskevi,dora)
m(grandfather,A,B):-m(father,A,C),m(parent,C,B)
m(grandfather,A,B):-m(husband,A,C),m(grandmother,C,B)
m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E)

Redundant clauses:
------------------
m(grandfather,stathis,stassa)
m(grandfather,stefanos,stassa)
m(grandfather,A,B):-m(father,A,C),m(father,C,B)

true.
```

Note that the reduction of the Top program includes not only the irredundant
clauses of the learned hypothesis, but also the clauses of the background
knowledge. These are separated from the learned hypothesis at the end of
learning. However, any positive examples that were not reduced by the hypothesis
are _not_ removed.

Another thing to note is that, in order to list the reduction of the Top
program, `list_top_program_reduction/1` must first construct the Top program and
then reduce it. Therefore, calling this listing predicate entails actually
training Louise. This means that, if training must take a long time, so will the
listing of the Top program reduction.

Finally, note that `list_top_program_reduction/1` only lists a "default ish"
version of Top program reduction and only for non-dynamic learning. For example,
options such as `recursive_reduction(true)` will be ignored and only a single
step of program reduction will be performed. This discrepancy will most likely
be addressed in future versions of Louise.

#### Listing the learned hypothesis

The last step in Louise's learning procedure is the excapsulation of the reduced
Top program. The result of excapsulation is a set of definite clauses output
directly by the learning predicates, `learn/[1,2,5]`. Since these predicates can
be used to list the learned hypothesis, this step has no explicit listing
predicate.

Dynamic learning
----------------

A limitation of Top program construction is that clauses are learned
one-at-a-time and do not take into account clauses already derived in a previous
iteration. In some cases it is necessary to take into account previously-
learned clauses, in particular when learning some recursive predicates. This is
possible using _dynamic learning_. 

Briefly, dynamic learning proceeds in discrete learning episodes; the hypothesis
learned in episode `k` is added to the background knowledge for episode `k+1`.
The dynamic learning process exits when the hypothesis learned in step `k` is
the same as the hypothesis learned in step `k-1`. 

Dynamic learning starts with a call to the learning predicates
`learn_dynamic/[1,2,6]`. 

The following is an example of learning a grammar for the Contex-Free `a^nb^n`
language, with a call to the dynamic learning predicate, `learn_dynamic/1`. The
example is taken from the experiment file `louise/data/examples/anbn.pl`:

```prolog
% Dynamic learning of a^nb^n grammar
?- learn_dynamic('S'/2).
'S'(A,B):-'A'(A,C),'B'(C,B).
'S'(A,B):-'S_1'(A,C),'B'(C,B).
'S_1'(A,B):-'A'(A,C),'S'(C,B).
true.
```

The `a^nb^n` grammar in the above listing cannot be learned by Louise without
dynamic learning, because the first clause, `'S'(A,B):-'A'(A,C),'B'(C,B)` is not
present in the background knowledge and so cannot be used to construct the third
clause, `'S_1'(A,B):-'A'(A,C),'S'(C,B)` recursively calling the first.  With
dynamic learning, the first clause is learned in the first dynamic episode, then
added to the background knowledge, and then the second and third clauses are
learned in the third dynamic episode.

### Predicate invention

The example dynamic learning attempt listed in the previous section includes a
definition of an _invented predicate_, `S_1/2`. An invented predicate is a
predicate defined in a learned hypothesis that is not defined in the background
knowledge and is not the learning target. Predicate invention may allow learning
some predicates when the background knowledge and metarules given in the initial
MIL Problem are insufficient to complete learning.

Predicate invention is performed in dynamic learning by means of a second Top
program construction step, following the normal Top program construction. During
this second Top proram construction step metarules are _extended_ by unfolding
on their shared literals, and a Top program is constructed with the extended
metarules. Then, the original metarules in an extension pair are kept and their
non-ground predicate variables replaced with an invented predicate symbol.


The number of predicates invented during a dynamic learning attempt is
controlled by the configuration option `max_invented/1`. For example, setting
this option to "1" will result in learned hypotheses with at most one predicate:

```prolog
% Maximum number of nvented predicates
max_invented(1)
```

### Metarule generations

If _Chain_ and _Inverse_ are the only metarules in the original MIL problem,
once dynamic learning exits, a new dynamic learning cycle begins where a new set
of metarules is generated, each having one more body literal than the metarules
in the previous generation. The process stops when the number of metarule
"generations" is equal to the value set for the configuration option
`dynamic_generations/1`:

```prolog
% Maximum number of metarule generations
dynamic_generations(1)
```

The dynamic learning process is further detailed in the module documentation at
the source file `louise/src/dynamic_learning.pl` where dynamic learning is
implemented. The metarule generation process is further detailed in the module
documentation of the source file `louise/src/metagen.pl`, where metarule
generatin and extension are defined. A more complete explanation of dynamic
learning and metarule extension will be included in the upcoming Louise manual.

Examples invention.
-------------------

In some cases, Louise is capable of inventing new examples that allow learning a
more accurate hypothesis than the examples given in the initial MIL problem. An
example of using examples invention is given in the experiment file
`data/examples/examples_invention.pl`. 

Examples invention is invoked with a call to one of the predicates
`examples_invention/[2,5]`. A learning predicate,
`learn_with_examples_invention/2` can be used to automate the process of
inventing new examples and adding them to the set of positive examples for a
learning attempt. Refer to the above listed example, `examples_invention.pl` for
instructions of how to use the example invention predicates.

Further details on examples invention will be included in Louise's upcoming
manual.


Bibliography and References
===========================

1. S.H. Muggleton, D. Lin, N. Pahlavi, and A. Tamaddoni-Nezhad. _Meta-interpretive learning: application to grammatical inference_. [Machine Learning, 94:25-49, 2014](https://link.springer.com/article/10.1007/s10994-013-5358-3)

2. S.H. Muggleton, D. Lin, and A. Tamaddoni-Nezhad. _Meta-interpretive learning of higher-order dyadic datalog: Predicate invention revisited_. [Machine Learning, 100(1):49-73, 2015](https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf)

3. Plotkin, Gordon, _Automatic Methods of Inductive Inference_. Doctoral thesis. The University of Edinburgh, 1972.

4. Sterling, Leon, _Patterns for Prolog Programming_. [Logic Programming and Beyond pp 374-401, Lecture Notes in Computer Science, vol 2407, 2002](https://link.springer.com/chapter/10.1007/3-540-45628-7_15)

[Metagol]: https://github.com/metagol/metagol "Metagol"
[Thelma]: https://github.com/stassa/thelma "Thelma"
[(Plotkin, 1972)]: https://era.ed.ac.uk/handle/1842/6656 "(Plotkin, 1972)"
[(Shapiro, 2002)]: https://link.springer.com/chapter/10.1007/3-540-45628-7_15 "(Shapiro, 2002)"
