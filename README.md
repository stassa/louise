Louise - polynomial-time Program Learning
=========================================

Getting help with Louise
------------------------

Louise's author can be reached by email at ep2216@ic.ac.uk. Please use this
email to ask for any help you might need with using Louise.

Louise is still new and, should you choose to use it, you will most probably
encounter errors and bugs. The author has no way to know of what bugs and errors
you encounter unless you report them. Please use the author's email to contact
the author regarding bugs and errors. Alternatively, you are welcome to open a
github Issue or send a pull request. 

Table of contents
-----------------

[Overview](#overview)  
[Capabilities](#capabilities)  
[Learning logic programs with Louise](#learning-logic-programs-with-louise)  
[Learning with metarules](#learning-with-metarules)  
[Learning metarules with TOIL](#learning-metarules-with-toil)  
[Pretty-printing metarules](#pretty-printing-metarules)  
[Controlling the Vanilla meta-interpreter](#controlling-the-vanilla-meta-interpreter)  
[Debugging training data](#debugging-training-data)  
[Debugging learning attempts](#debugging-learning-attempts)  
[Experiment scripts](#experiment-scripts)  
[Further documentation](#further-documentation)  
[Citing Louise](#citing-louise)  
[Bibliography](#bibliography)  

Overview
--------

Louise [(Patsantzis & Muggleton 2021)] is a machine learning system that learns
Prolog programs.

Louise is a Meta-Interpretive Learning (MIL) system. MIL [(Muggleton et al.
2014)], [(Muggleton et al. 2015)], is a new setting for Inductive Logic
Programming (ILP) [(Muggleton, 1991)]. ILP is a form of weakly-supervised
machine learning of logic programs from examples of program behaviour (meaning
examples of the inputs and outputs of the programs to be learned). Unlike
conventional, statistical machine learning algorithms, ILP approaches do not
need to see examples of programs to learn new programs and instead rely on
_background knowledge_, a library of pre-existing logic programs that they reuse
to compose new programs. In MIL, the background knowledge is a higher-order
logic program that includes both first- and second-order clauses, the latter
called _metarules_. MIL systems like Louise learn by specialising their
metarules by SLD-resolution. Examples and background knowledge, including
metarules, are provided by the user, but Louise can learn its own background
knowledge, including metarules.

Louise is based on a MIL meta-interpreter based on a Prolog "vanilla"
meta-interpreter. Accordingly, the MIL meta-interpreter in Louise is called
Vanilla. Vanilla can be used to implement MIL algorithms like the original MIL
algorithm in Metagol, and Louise's signature MIL algorithm, called _Top Program
Construction_ (TPC), that runs in polynomial time. TPC avoids an expensive
search of the program search space and instead learns by construcing a unique
object that is a correct hypothesis, consistent with the training examples.

In this manual we show simple examples where Louise is trained on small, "toy"
problems, designed to demonstrate its use. Louise is still new and actively
being worked on and so has not yet been used in large-scale real-world
applications. Published work on Louise has so far focused on describing the
working principles behind Louise's underlying TPC algorithm rather than
demonstrating its full potential as a learning system. Louise is maintained by a
single PhD student, currently writing her PhD thesis. New developments should be
expected to come at a leisurely pace.

Capabilities
------------

Here are some of the things that Louise can do.

1. Louise can learn recursive programs, including left-recursive programs:

   ```prolog
   ?- learn(ancestor/2).
   ancestor(A,B):-parent(A,B).
   ancestor(A,B):-ancestor(A,C),ancestor(C,B).
   true.
   ```
   
   See `data/examples/tiny_kinship.pl` for the `ancestor` example (and other
   simple, toy examples of learning kinship relations, ideal for first time
   use).
   
   See the section [Learning logic programs with
   Louise](#learning-logic-programs-with-louise) for more information on
   learning logic programs with Louise.

2. Louise can learn recursive programs one-shot:

   ```prolog
   % Single example given:
   ?- experiment_file:positive_example(list_last/2, E).
   E = list_last([a, b, c, d, e, f, g, h|...], i).

   ?- learn(list_last/2).
   list_last(A,B):-tail(A,C),list_last(C,B).
   list_last(A,B):-tail(A,C),empty(C),head(A,B).
   true.
   ```

   In the example above, the learned program consists of a recursive clause and
   a "base-case" that terminates the recursion. Both clauses were learned from
   the single provided example that neither clause suffices to prove on its own.

   Note that this is true one-shot learning, from a single example of inputs and
   outputs of the target program, not examples of _programs_, and without
   pre-training on billions of examples or anything silly like that. Note also
   that the single positive example given is an example of the inductive case of
   the recursion. Louise must learn the "base-case" on its own.

   See `data/examples/findlast.pl` for the `list_last/2` one-shot recursion
   learning example.

   The above experiment was proposed by Andrew Cropper, maintainer of [Metagol].

3. Louise can simultaneously learn multiple dependent programs, including
   mutuallly recursive programs (this is called multi-predicate learning):

   ```prolog
   ?- learn([even/1,odd/1]).
   even(0).
   even(A):-predecessor(A,B),odd(B).
   odd(A):-predecessor(A,B),even(B).
   true.
   ```

   In the example above, `even/1` and `odd/1` are "dependent" on each other in
   the sense that `even/1` is defined in terms of `odd/1` and `odd/1` is defined
   in terms of `even/1`. Neither definition was given by the user prior to
   training and so neither program could be learned independently.

   See `data/examples/multi_pred.pl` for the `odd/1` and `even/1`
   multi-predicate learning example. 

4. Louise can discover relevant background knowledge. In the `odd/1` and
   `even/1` example above, each predicate is only explicitly given
   `predecessor/2` as a background predicate. The following are the background
   knowledge declarations for `even/1` and `odd/1` in
   `data/examples/multi_pred.pl`:

   ```prolog
   background_knowledge(even/1, [predecessor/2]).
   background_knowledge(odd/1, [predecessor/2]).
   ```

   Louise figures out that `odd/1` is necessary to learn `even/1` and vice-versa
   on its own. Aaw. Isn't it smart?

5. Louise can perform _predicate invention_ to increase its background knowledge
   with new predicates that are necessary for learning. In the following example
   Louise learns a grammar of the context-free a^nb^n language in Prolog's
   Definite Clause Grammars form. In the learned program the predicate `'$1'/2`
   is an _invented_ predicate. That means that `'$1'/2` was not given by the
   user as background knowledge, nor did the user provide examples of `'$1'/2`,
   rather Louise invented it independently in the process of learning the target
   predicate, `s/2`:

   ```prolog
   % Learning a grammar of the a^nb^n context-free language
   ?- experiment_file:positive_example(s/2,E).
   E = s([a, a, b, b], []).

   ?- experiment_file:negative_example(s/2,E).
   E = s([a, a], []) ;
   E = s([b, b], []) ;
   E = s([a, a, b], []) ;
   E = s([a, b, b], []).

   ?- experiment_file:background_knowledge(s/2,BK).
   BK = [a/2, b/2].

   ?- experiment_file:metarules(s/2,MS), print_metarules(MS).
   (Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
   MS = [chain].

   ?- learn(s/2).
   '$1'(A,B):-a(A,C),a(C,B).
   '$1'(A,B):-a(A,C),s(C,B).
   '$1'(A,B):-b(A,C),b(C,B).
   '$1'(A,B):-s(A,C),b(C,B).
   s(A,B):-'$1'(A,C),'$1'(C,B).
   s(A,B):-'$1'(A,C),b(C,B).
   s(A,B):-a(A,C),'$1'(C,B).
   s(A,B):-a(A,C),b(C,B).
   true.
   ```

   With predicate invention Louise can shift its inductive bias to learn
   programs that are not possible to learn from its initial set of background
   knowledge and metarules.

   See `data/examples/anbn.pl` for the `s/2` example and discussion of predicate
   invention in Louise.

6. Louise can learn new metarules from examples of a target predicate. In the
   following example, Louise learns a new metarule from examples of the
   predicate `s/2` (as in item 5, above):

   ```Prolog
   ?- learn_metarules(s/2).
   (Meta-dyadic-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
   true.
   ```

   The new metarule, Meta-dyadic-1 corresponds to the common _Chain_ metarule
   that we saw used in item 5 above to learn a grammar of the a^nb^n language.

   Louise can learn new metarules by specialising the most-general metarule in
   each language class. In the example above, the language class is H(2,2), the
   language of metarules having exactly three literals of arity 2. The most
   general metarule in H(2,2) is Meta-dyadic:

   ```Prolog
   ?- print_metarules(meta_dyadic).
   (Meta-dyadic) ∃.P,Q,R ∀.x,y,z,u,v,w: P(x,y)← Q(z,u),R(v,w)
   true.
   ```

   Louise can also learn new metarules given only an upper and lower bound on
   their numbers of literals as a _third order_ metarule. In the example of
   learning _Chain_ above, instead of specifying Meta-dyadic, we can instead
   give an upper and lower bound of 3, with a declaration of
   `higher_order(3,3)`:

   ```Prolog
   ?- print_metarules(higher_order(3,3)).
   (TOM-3) ∃.P,Q,R: P ← Q,R
   true.
   ```

   ```Prolog
   ?- learn_metarules(s/2).
   (Hom-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
   true.
   ```

   See the section [Learning metarules with TOIL](#learning-metarules-with-toil)
   for more information on learning metarules with Louise.

7. Louise can unfold programs to eliminate invented predicates. In the query
   listed below, Louise learns the same program as in the a^nb^n example in item
   5 above, but this time the invented predicate `'$1'/2` is eliminated by
   unfolding:

   ```prolog
   ?- learn(s/2).
   s(A,B):-a(A,C),b(C,B).
   s(A,B):-a(A,C),s(C,D),b(D,B).
   true.
   ```

   Eliminating invented predicates can sometimes improve comprehensibility of
   the learned program.

8. Louise can fold over-specialised programs to introduce recursion. In the
   following example, the learning problem is set up so as to force Louise to
   learn an over-specialised program that finds the last element of a list of
   length up to 3:

   ```prolog
   ?- learn(list_last/2).
   list_last(A,B):-tail(A,C),empty(C),head(A,B).
   list_last(A,B):-tail(A,C),tail(C,D),empty(D),head(C,B).
   list_last(A,B):-tail(A,C),tail(C,D),tail(D,E),empty(E),head(D,B).
   true.
   ```

   We can observe that some clauses in the program above include sequences of
   body literals that match the body literals in other clauses. The predicate
   `fold_recursive/2` can be used to replace body literals in a clause with an
   equivalent recursive call:

   ```prolog
   ?- learn(list_last/2, _Ps), fold_recursive(_Ps, _Fs), maplist(print_clauses,['%Learned:','\n%Folded:'], [_Ps,_Fs]).
   %Learned:
   list_last(A,B):-tail(A,C),empty(C),head(A,B).
   list_last(A,B):-tail(A,C),tail(C,D),empty(D),head(C,B).
   list_last(A,B):-tail(A,C),tail(C,D),tail(D,E),empty(E),head(D,B).
   
   %Folded:
   list_last(A,B):-tail(A,C),empty(C),head(A,B).
   list_last(A,B):-tail(A,C),list_last(C,B).
   true.
   ```

   Note the new clause `list_last(A,B):-tail(A,C),list_last(C,B).` replacing the
   second and third clause in the original program. The new, recursive
   hypothesis is now a correct solution for lists of arbitrary length. 

   The `list_last` example above was taken from _Inductive Logic Programming at
   30_ (Cropper et al., Machine Learning 2021, to appear). See
   `data/examples/recursive_folding.pl` for the complete example source code.

9. Louise can invent new examples. In the listing below a number of examples
   of `path/2` are invented. The background knowledge for this MIL problem
   consists of 6 `edge/2` ground facts that determine the structure of a graph
   and a few facts of `not_edge/2` that represent nodes not connected by edges.
   `path(a,f)` is the single example given by the user. The target theory (the
   program we wish the system to learn) for this problem is a recursive
   definition of `path/2` that includes a "base case" for which no example is
   given. Louise can invent examples of the base-case and so learn a correct
   hypothesis that represents the full path from node 'a' to node 'f', without
   crossing any non-edges.

   ```prolog
   % Try learning with the single given example:
   ?- learn(path/2).
   path(a,f).
   true.

   % List invented examples:
   ?- examples_invention(path/2).
   m(path,a,b).
   m(path,a,c).
   m(path,a,d).
   m(path,a,e).
   m(path,a,f).
   m(path,b,c).
   m(path,b,d).
   m(path,b,e).
   m(path,b,f).
   m(path,c,d).
   m(path,c,e).
   m(path,c,f).
   m(path,d,e).
   m(path,d,f).
   m(path,e,f).
   true.

   % Invent examples and try again:
   ?- learn_with_examples_invention(path/2).
   path(A,B):-edge(A,B).
   path(A,B):-edge(A,C),path(C,B).
   true.
   ```
   
   See `data/examples/example_invention.pl` for the `path/2` example.

11. Louise can learn large programs efficiently. Below, note the output of
    `length/2`, counting the clauses in the learned program:

    ```Prolog
    ?- time(learn(move/2,_Ps)), length(_Ps,N).
    % 15,952,615 inferences, 4.531 CPU in 4.596 seconds (99% CPU, 3520577 Lips)
    N = 2101.
    ```

    In the example above, we train Louise on a grid-world navigation task set up
    so that the size of the search space of hypotheses (Hypothesis Space) grows
    exponentially with the size of the target theory. The target theory in turn
    grows linearly with the number of training examples. Louise completes the
    learning task and learns a theory of more than 2000 clauses in under 5
    seconds (although running time will depend on the system; for the example
    above, we trained Louise on a six-year old laptop with an i7 processor
    clocked at 2.6 GHz and 16 GB of RAM).

    The target theory for the `move/2` learning problem is around 600 clauses so
    the hypothesis learned by Louise has much redundancy, although it is a
    correct hypothesis that is consistent with the examples. The reason for the
    redundancy is that there are multiple "versions" of the target theory and
    the Top Program learned by Louise includes each of them as a subset. Louise
    can learn this "super-theory" in only a few seconds thanks to the efficiency
    of its Top Program Construction algorithm (TPC) that avoids an expensive
    search of the Hypothesis Space and instead directly constructs a unique
    object. The TPC algorithm runs in polynomial time and can learn efficiently
    regardless of the size of the Hypothesis Space.

    See `data/examples/robots.pl` for the `move/2` example.

Louise comes with a number of libraries for tasks that are useful when learning
programs with MIL, e.g. metarule generation, program reduction, lifting of
ground predicates, etc. These will be discussed in detail in the upcoming Louise
manual.

Learning logic programs with Louise
-----------------------------------

In this section we give a few examples of learning simple logic programs with
Louise. The examples are chosen to demonstrate Louise's usage, not to convince
of Louise's strengths as a learner. All the examples in this section are in the
directory `louise/data/examples`. After going through the examples here, feel
free to load and run the examples in that directory to better familiarise
yourself with Louise's functionality.

### Running the examples in SWI-Prolog

SWI-Prolog is a popular, free and open-source Prolog interpreter and development
environment. Louise was written for SWI-Prolog. To run the examples in this
section you will need to install SWI-Prolog. You can download SWI-Prolog from
the following URL:

[https://www.swi-prolog.org/Download.html](https://www.swi-prolog.org/Download.html)

Louise runs with any of the latest stable or development releases listed on that
page. Choose the one you prefer to download.

It is recommended that you run the examples using the SWI-Prolog graphical IDE,
rather than in a system console. On operating systems with a graphical
environment the SWI-Prolog IDE should start automaticaly when you open a Prolog
file.

In this section, we assume you have cloned this project into a directory called
`louise`. Paths to various files will be given relative to the `louise` project
root directory and queries at the SWI-Prolog top-level will assume your current
working directory is `louise`.

### A simple example

Louise learns Prolog programs from examples, background knowledge and second
order logic clauses called _metarules_. Together, examples, background knowledge
and metarules form the elements of a _MIL problem_.

Louise expects the elements of a MIL problem to be in an _experiment file_
which is a Prolog module file with a standard format. The following is an
example showing how to use Louise to learn the "ancestor" relation from the
examples, background knowledge and metarules defined in the experiment file
`louise/data/examples/tiny_kinship.pl` using the learning predicate `learn/1`.

In summary, there are four steps to running an example: a) start Louise; b) edit
the configuration file to select an experiment file; c) load the experiment file
into memory; d) run a learning query. These four steps are described in detail
below.

 1. Consult the project's load file into SWI-Prolog to load necessary files into
    memory:

    In a graphical environment:
    
    ```prolog
    ?- [load_project].
    ```

    In a text-based environment:

    ```prolog
    ?- [load_headless].
    ```

    The first query will also start the SWI-Prolog IDE and documentation
    browser, which you probably don't want if you're in a text-based
    environment.

 2. Edit the project's configuration file to select an experiment file.

    Edit `louise/configuration.pl` in the SWI-Prolog editor (or your favourite
    text editor) and make sure the name of the current experiment file is set to
    `tiny_kinship.pl`:

    ```prolog
    experiment_file('data/examples/tiny_kinship.pl',tiny_kinship).
    ```

    The above line will already be in the configuration file when you first
    clone Louise from its github repository. There will be a few more clauses of
    `experiment_file/2`, each on a separate line and commented-out. These are
    there to quickly change between different experiment files without having to
    re-write their paths every time. Make sure that only a single
    `experiment_file/2` clause is loaded in memory (i.e.  don't uncomment any
    other `experiment_file/2` clause except for the one above).

 3. Reload the configuration file to pick up the new experiment file option.

    The easiest way to reload the configuration file is to use SWI-Prolog's `make/0`
    predicate to recompile the project (don't worry- this takes less than a
    second). To recompile the project with `make/0` enter the following query in
    the SWI-Prolog console:

    ```prolog
    ?- make.
    ```

    Note again: this is the SWI-Prolog predicate `make/0`. It's not the _make_
    build automation tool!

 4. Perform a learning attempt using the examples, background knowledge and
    metarules defined in `tiny_kinship.pl` for `ancestor/2`.

    Execute the following query in the SWI-Prolog console; you should see the
    listed output:
 
    ```prolog
    ?- learn(ancestor/2).
    ancestor(A,B):-parent(A,B).
    ancestor(A,B):-ancestor(A,C),ancestor(C,B).
    true.
    ```
 
    The learning predicate `learn/1` takes as argument the predicate symbol and
    arity of a _learning target_ defined in the currently loaded experiment
    file. `ancestor/2` is one of the learning targets defined in
    `tiny_kinship.pl`, the experiment file selected in step 2. The same
    experiment file defines a number of other learning targets from a typical
    kinship relations domain.

Learning with metarules
-----------------------

Metarules are used in MIL as second-order background knowledge. Metarules are
second-order definite clauses, similar to first-order definite clauses, but have
second-order variables existentially quantified over the set of predicate
symbols.

Below are some examples of metarules in the `H22` language of metarules, with at
most three literals of arity at most 2 (as output by Louise's metarule
pretty-printer):

```prolog
?- print_metarules([abduce,chain,identity,inverse,precon,postcon]).
(Abduce) ∃.P,X,Y: P(X,Y)←
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Identity) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
(Inverse) ∃.P,Q ∀.x,y: P(x,y)← Q(y,x)
(Precon) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)
(Postcon) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x,y),R(y)
true.
```

The words in parentheses preceding metarules are identifiers used by Louise to
find metarules declared in an experiment file.

MIL systems like Louise learn by instantiating the existentially quantified
variables in metarules to form the first-order clauses of a logic program. The
instantiation is performed during a refutation-proof of the positive examples,
by SLD-resolution with the metarules and the clauses in the background
knowledge. Positive examples are first unified with the head literals of
metarules, then the metarules' body literals are resolved with the background
knowledge. When resolution succeeds metarules become fully-ground. The
substitutions of metarules' universally quantified variables are discarded to
preserve the "wiring" between metarule literals, while _metasubstitutions_ of
their existentially quantified variables are kept to create first-order clauses
with ground predicate symbols.

In the example below, the metasubstitution `Theta =
{P/grandfather,Q/father,R/parent}` is applied to the _Chain_ metarule to produce
a first-order clause:

```Prolog
Chain = P(x,y)← Q(x,z),R(z,y)
Theta = {P/grandfather,Q/father,R/parent}
Chain.Theta = grandfather(x,y)← father(x,z),parent(z,y)
```

Metarules can also have existentially quantified _first-order_ variables. The
metasubstitutions of such variables are also kept, and become _constants_ in the
learned theories.

In the example below, the metasubstitution `Theta =
{P/father,X/kostas,Y/stassa}` is applied to the metarule _Abduce_ to produce a
ground first-order clause with constants:

```Prolog
Abduce = P(X,Y)←
Theta = {P/father,X/kostas,Y/stassa}
Abduce.Theta = father(kostas,stassa)←
```

Learning metarules with TOIL
----------------------------

Metarules are usually defined by hand, and are tailored to a possible solution
of a learning problem. Louise is capable of learning its own metarules.

Metarule learning in Louise is implemented by a sub-system called TOIL (an
acronym for _Third-Order Inductive Learner_). TOIL learns metarules from
examples, background knowledge and _generalised_ metarules. Unlike user-defined
metarules, the generalised metarules used by TOIL do not have to be closely
tailored to a problem.

For more information on TOIL see [(Patsantzis & Muggleton 2021b)].

### Metarule taxonomy

TOIL recognises three taxa of metarules. Listed by degrees of generality, these
are: sort, matrix and punch metarules. Their names are derived from typeset
printing where successive levels of molds for letters to be typed are carved in
metals of decreasing hardness.

#### Sort metarules

Sort metarules are the kind of metarules normally defined by a user and found
throughout the MIL literature. They are also widely used in ILP more generally
as well as in data mining and program synthesis.

Sort metarules are specialised, in the sense that their universally quantified
first-order variables are _shared_ between literals. For example, in the _Chain_
metarule, below, the variable `x` is shared between its head literal and its
first body literal, the variable `y` is shared between its head literal and last
body literal, and the variable `z` is shared between its two body literals:

```Prolog
P(x,y)← Q(x,z),R(z,y)
  `-|-----' `----' |
    `--------------'
```

The sharing of universally quantified variables (the "wiring" of the clause)
determines the metarule's semantincs. For example, _Chain_ denotes a
transitivity relation between the predicates `P,Q` and `R` (more precisely, the
predicates whose symbols are _subsituted_ for `P,Q` and `R` during learning).

Sort metarules may also have some, or all, of their existentially quantified
variables shared. For example, the existentially quantified second-order
variables in the metarule _Tailrec_ are shared between its head literal and its
last body literal, forcing _Tailrec_ to be instantiated so as to produce
tail-recursive clauses:

```Prolog
(Tailrec) ∃.P,Q ∀.x,y,z: P(x,y)← Q(x,z),P(z,y)
                          `-------------'
```

The "wiring" of metarules' variables, both first- and second-order, is preserved
in their first-order instances learned by MIL. In the example below, the
substitution `Theta = {P/ancestor,Q/parent}` is applied to _Tailrec_ to produce
a tail-recursive clause:

```Prolog
Tailrec = P(x,y)← Q(x,z),P(z,y)
Theta = {P/ancestor,Q/parent}
Tailrec.Theta = ancestor(x,y)← parent(x,z),ancestor(z,y)
```

Sort metarules are usually _fully-connected_, meaning that the variables in the
head are shared with variables in the body and every literal in the body is
"connected" through a sharing of variables to the head literal.

Thanks to the specialised "wiring" of their variables sort metarules are used to
fully-define the set of clauses that will be considered during a MIL-learning
attempt. At the same time, the wiring can be too-specific resulting in the
elimintation of clauses necessary to solve a problem.

#### Matrix metarules

Matrix metarules are a generalisation of the sort metarules. Matrix metarules
are also second-order clauses, like the sort metarules, but each variable
appears _exactly once_ in a matrix metarule. The following are examples of
matrix metarules:

```Prolog
?- print_metarules([meta_monadic,meta_dyadic,meta_precon,meta_postcon]).
(Meta-monadic) ∃.P,Q ∀.x,y,z,u: P(x,y)← Q(z,u)
(Meta-dyadic) ∃.P,Q,R ∀.x,y,z,u,v,w: P(x,y)← Q(z,u),R(v,w)
(Meta-precon) ∃.P,Q,R ∀.x,y,z,u,v: P(x,y)← Q(z),R(u,v)
(Meta-postcon) ∃.P,Q,R ∀.x,y,z,u,v: P(x,y)← Q(z,u),R(v)
true.
```

Note how the metarules above generalise multiple of the `H22` metarules in
previous sections. For example, _Meta-Monadic_ generalises _Identity_ and
_Inverse_ while _Meta-Dyadic_ generalises _Chain_, _Switch_ and _Swap_ (the
latter two are defined in `configuration.pl` but ommitted here for brevity).

In practice, for each set of sort metarules, `S`, there exists some minimal set
of matrix metarules `M`, such that each sort metarule in `S` has a
generalisation in `M`. Conversely, each sort metarule in `S` can be derived by
_specialisation_ of a metarule in `M`. This specialisation is performed by TOIL
to learn sort metarules from matrix metarules. We show examples of this in a
following section.

#### Punch metarules.

We can generalise sort metarules to matrix metarules by replacing each variable
in a sort metarule by a new, "free" variable. Matrix metarules can themselves be
generalised by replacing each of their literals by a variable. The resulting
metarules are the _punch_ metarules:

```Prolog
(TOM-1) ∃.P: P
(TOM-2) ∃.P,Q: P ← Q
(TOM-3) ∃.P,Q,R: P ← Q,R
```

Punch metarules have variables quantified over the set of _atoms_ and so they
are _third-order logic_ clauses. Just as matrix metarules can be specialised to
sort metarules, sets of punch metarules can be specialised to matrix metarules
with the same number of literals.

### Metarule specialisation with TOIL

TOIL learns metarules by exploiting the generality relation between the three
taxa of metarules described in the previous sections. TOIL takes as input the
examples and background knowledge in a MIL Problem and sets of either matrix, or
punch metarules. The matrix or punch metarules are specialised by SLD-resolution
to produce second-order sort metarules.

Specialisation of matrix and punch metarules in TOIL is performed by the Top
Program Construction algorithm, which is the same algorithm used to learn
first-order clauses from sort metarules in Louise's other learning predicates.
In other words, TOIL learns metarules for MIL by MIL.

TOIL defines two new families of learning predicates: `learn_metarules/[1,2,5]`
and `learn_meta/[1,2,5]`.

#### The `learn_metarules/[1,2,5]` family of learning predicates

The `learn_metarules/[1,2,5]` family of predicates is used to learn and output
sort metarules. 

Below is an example of specialising the matrix metarules _Meta-Monadic_ and
_Meta-Dyadic_ to learn a new set of sort metarules for the `ancestor` relation.
The necessary examples, background knowledge and matrix metarules are defined in
the experiment file `data/examples/tiny_kinship_toil.pl`:

```Prolog
% experiment_file('data/examples/tiny_kinship_toil.pl',tiny_kinship_toil).
?- learn_metarules(ancestor/2).
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
true.
```

Ouch! That's a lot of metarules! Indeed, the most severe limitation of TOIL's
metarule specialisation approach is that it can easily _over-generate_
metarules. The reason for that is that there are many possible specialisations
of a set of matrix (or punch) metarules into sort metarules that allow a correct
hypothesis to be learned. In practice, we usually want to keep only a few of
those metarules, for instance, in the example above, just the two metarules
_Meta-monadic-13_ and _Meta-dyadic-7_ suffice to learn a correct hypotesis for
`ancestor/2` (according to the MIL problem elements in the experiment file we're
using).

#### Controlling over-generation of metarules

To control over-generation, TOIL employs a number of different strategies that
can be chosen with the two configuration options
`generalise_learned_metarules/1` and `metarule_learning_limits/1`.

The option `metarule_learning_limits/1` recognises the following settings:
`none`, `coverset`, `sampling(S)` and `metasubstitutions(K)`. Briefly, option
`none` places no limit on metarule learning; `coverset` first removes the
examples "covered" by instances of the last learned metarule before attempting
to learn a new one; `sampling(S)` learns metarules from a sub-sample of examples
according to its single argument; and `metasubstitutions(K)` only attempts the
specified number of metasubstitutions while specialising each matrix or punch
metarule.

We show examples of each `metarule_learning_limits/1` option below. We ommit the
result for the setting `none` which is identical with the example of
`learn_metarules/1` given earlier in this section:

```Prolog
?- learn_metarules(ancestor/2), metarule_learning_limits(L).
(Meta-monadic-1) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
(Meta-dyadic-2) ∃.P,P,Q ∀.x,y,z: P(x,y)← P(z,y),Q(z,x)
(Meta-dyadic-3) ∃.P,P,P ∀.x,y,z: P(x,y)← P(z,y),P(x,z)
(Meta-dyadic-4) ∃.P,P,Q ∀.x,y,z: P(x,y)← P(x,z),Q(y,z)
L = coverset.

?- learn_metarules(ancestor/2), metarule_learning_limits(L).
(Meta-dyadic-1) ∃.P,P,Q ∀.x,y,z: P(x,y)← P(x,z),Q(y,z)
(Meta-dyadic-2) ∃.P,Q,P ∀.x,y,z: P(x,y)← Q(y,z),P(x,z)
(Meta-monadic-3) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
L = sampling(0.1).

?- learn_metarules(ancestor/2), metarule_learning_limits(L).
(Meta-dyadic-1) ∃.P,P,Q ∀.x,y,z: P(x,y)← P(x,z),Q(y,z)
(Meta-monadic-2) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
L = metasubstitutions(1).
```

Note that option `metasubstitutions(1)` allows one metasubstitution of _each_
matrix or punch metarule.

A different restriction to the set of metarules learned by TOIL can be applied
by setting the option `generalise_learned_metarules/1` to `true`. The example
below shows the effect of that setting on the `ancestor` example:

```Prolog
?- learn_metarules(ancestor/2), metarule_learning_limits(L), generalise_learned_metarules(G).
(Meta-dyadic-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(y,z)
(Meta-dyadic-2) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Meta-dyadic-3) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(y,z),R(x,z)
(Meta-dyadic-4) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,x),R(z,y)
(Meta-dyadic-5) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(x,z)
(Meta-dyadic-6) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(z,x)
(Meta-monadic-7) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
L = none,
G = true.
```

Note that in the sort metarules learned by TOIL this time around, none of the
second-order variables are shared between literals. By contrast, in the first
example of `learn_metarules/1` earlier in this section, where
`generalise_learned_metarules/1` was left to its default of `false`, TOIL
learned metarules with some second-order variables shared between literals, for
instance the two metarules below that only have one second-order variable each:

```Prolog
(Meta-dyadic-1) ∃.P,P,P ∀.x,y,z: P(x,y)← P(x,z),P(z,y)
(Meta-dyadic-2) ∃.P,P,P ∀.x,y,z: P(x,y)← P(z,y),P(x,z)
```

#### Specialisation of Punch metarules

Matrix metarules still require some intuition, from the part of the user, about
the structure of clauses in a target theory. In particular, while no variables
are shared between the literals of matrix metarules, the user still needs to
define those literals, with their correct arities.

TOIL can also specialise punch metarules, for which we only need to know the
number of literals, regardless of arity. The following is an example of using
punch metarules, again with the `ancestor` example from
`data/examples/tiny_kinship_toil.pl` (and with `metarule_learning_limits/1`
again set to "none"):

```Prolog
?- learn_metarules(ancestor/2), metarule_learning_limits(L), generalise_learned_metarules(G).
(Hom-1) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
(Hom-2) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(y,z)
(Hom-3) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Hom-4) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(y,z),R(x,z)
(Hom-5) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,x),R(z,y)
(Hom-6) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(x,z)
(Hom-7) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(z,x)
L = none,
G = true.
```

You can tell that the sort metarules above were learned from punch metarules
from their automatically generated identifiers- but you can also list the
learning problem for `ancestor/2`:

```Prolog
?- list_mil_problem(ancestor/2).
Positive examples
-----------------
ancestor(stathis,kostas).
ancestor(stefanos,dora).
ancestor(kostas,stassa).
ancestor(alexandra,kostas).
ancestor(paraskevi,dora).
ancestor(dora,stassa).
ancestor(stathis,stassa).
ancestor(stefanos,stassa).
ancestor(alexandra,stassa).
ancestor(paraskevi,stassa).

Negative examples
-----------------
:-ancestor(kostas,stathis).
:-ancestor(dora,stefanos).
:-ancestor(stassa,kostas).
:-ancestor(kostas,alexandra).
:-ancestor(dora,paraskevi).
:-ancestor(stassa,dora).
:-ancestor(stassa,stathis).
:-ancestor(stassa,stefanos).
:-ancestor(stassa,alexandra).
:-ancestor(stassa,paraskevi).

Background knowledge
--------------------
parent/2:
parent(A,B):-father(A,B).
parent(A,B):-mother(A,B).

Metarules
---------
(TOM-2) ∃.P,Q: P ← Q
(TOM-3) ∃.P,Q,R: P ← Q,R
true.
```

The `learn_metarules/[1,2,5]` family of predicates outputs metarules rather than
first-order clauses, and so it can be used to _suggest_ metarules to the user.
That is, the user can copy any of the metarules learned by TOIL into an
experiment file.

When you use TOIL to suggest metarules, remember to set the
`metarule_formatting/1` configuration option to `user_friendly`, so that TOIL
prints out its learned metarules in a format that you can directly copy/paste
into an experiment file:

```Prolog
?- learn_metarules(ancestor/2), metarule_formatting(F).configuration:hom_1 metarule 'P(x,y):- Q(x,y)'.
configuration:hom_2 metarule 'P(x,y):- Q(x,z),R(y,z)'.
configuration:hom_3 metarule 'P(x,y):- Q(x,z),R(z,y)'.
configuration:hom_4 metarule 'P(x,y):- Q(y,z),R(x,z)'.
configuration:hom_5 metarule 'P(x,y):- Q(z,x),R(z,y)'.
configuration:hom_6 metarule 'P(x,y):- Q(z,y),R(x,z)'.
configuration:hom_7 metarule 'P(x,y):- Q(z,y),R(z,x)'.
F = user_friendly.
```

#### The `learn_meta/[1,2,5]` family of learning predicates

To immediately pass the learned metarules to one of Louise's other learning
predicates, the `learn_meta/[1,2,5]` family of predicates can be used. We show
this below.

```Prolog
?- learn_meta(ancestor/2), metarule_learning_limits(L), generalise_learned_metarules(G).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(C,B),parent(C,A).
ancestor(A,B):-ancestor(C,B),ancestor(A,C).
ancestor(A,B):-ancestor(C,B),parent(A,C).
ancestor(A,B):-parent(C,B),ancestor(A,C).
ancestor(A,B):-parent(C,B),parent(A,C).
ancestor(A,B):-ancestor(A,C),parent(B,C).
L = coverset,
G = true.
```

The predicates in the `learn_meta/[1,2,5]` family pass the metarules learned by
TOIL to the learning predicate defined in the configuration option
`learning_predicate/1`. If this is set to one of the `learn_meta` predicates
itself, and to avoid an infinite recursion tearing a new one to the underlying
superstructure of the universe, the `learn/5` learning predicate is invoked
instead, which passes the metarules learned by TOIL to Louise's basic Top
Program Construction learning algorithm.

### Limitations of TOIL

The current implementation of TOIL is still a prototype, but we think it goes a
long way towards addressing an important limitation of MIL, the need to define
metarules by hand. In this section we discuss some limitations of TOIL itself.

We have already discussed the tendency of TOIL to over-generalise.
Counter-intuitively TOIL can also _over-specialise_ to its training examples.

TOIL is the first learning system that can learn metarules without first
generating every possible metarule pattern and testing it against the training
examples. Rather, TOIL uses the TPC algorithm to efficiently construct metarules
by SLD-resolution.

In the same way that sort metarules are fully-ground by TPC when learning
first-order clauses, matrix and punch metarules are fully-ground when learning
sort metarules. To ensure that the resulting sort metarules are fully-connected,
TOIL forces the first-order variables in matrix metarules (and in the
specialisations of punch metarules) to be replaced by constants in literals
earlier in the clause. This can cause a form of over-specialisation where the
metarules learned by TOIL overfit to the training examples.

This overfitting can be seen in the examples of `learn_metarules/1` and
`learn_meta/1` shown in the earlier sections. The following example calls
`learn_meta/1` with the options `metarule_learning_limits/1` and
`generalise_learned_metarules/1` chosen so as to force over-specialisation and
better illustrate overfitting:

```Prolog
?- learn_meta(ancestor/2), metarule_learning_limits(L), generalise_learned_metarules(G).
ancestor(stathis,stassa).
ancestor(stefanos,stassa).
ancestor(alexandra,stassa).
ancestor(paraskevi,stassa).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),parent(B,C).
L = metasubstitutions(1),
G = true.
```

A further limitation of TOIL is that its current implementation does not perform
predicate invention when learning new metarules. However, it is often possible
to learn metarules without predicate invention that suffice to perform predicate
invention. In the following example, we modify the `data/examples/anbn.pl`
experiment file to learn the two new metarules, one of which is identical to
_Chain_ and is then used to learn a correct hypothesis _with_ predicate
invention. We activate the `learned_metarules` debugging subject to show the
metarules learned by TOIL and passed to `learn_dynamic/5`:

```Prolog
?- debug(learned_metarules).
true.

?- learn_meta('S'/2).
% Learned metarules:
% (Hom-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
% (Hom-2) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(x,z)
'$1'(A,B):-'S'(A,C),'B'(C,B).
'S'(A,B):-'A'(A,C),'$1'(C,B).
'S'(A,B):-'A'(A,C),'B'(C,B).
true.
```

This is possible because of the generality of metarules, even sort metarules.

Pretty-printing metarules
-------------------------

In a previous section we have used Louise's pretty-printer for metarules,
`print_metarules/1` to print metarules in an easy-to-read format.

Louise can pretty-print metarules in three formats: quantified, user-friendly,
or expanded.

### Quantified metarules

The _quantified_ metarule format is identical to the formal notation of
metarules that can be found in the MIL literature. Quantified metarules are
preceded by an identifying name and have their variables quantified by
existential and universal quantifiers. 

We repeat the example from the previous section but this time we use the
two-arity `print_metarules/2` to specify the metarule printing format:

```Prolog
?- print_metarules(quantified, [abduce,chain,identity,inverse,precon,postcon]).
(Abduce) ∃.P,X,Y: P(X,Y)←
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Identity) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
(Inverse) ∃.P,Q ∀.x,y: P(x,y)← Q(y,x)
(Precon) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)
(Postcon) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x,y),R(y)
true.
```

The first argument of `print_metarules/2` is used to choose the printing format
and can be one of: `quantified, user_friendly`, or `expanded`. We describe the
latter two formats below.

### User-friendly metarules

The _user-friendly_ metarule format is used to manually define metarules for
learning problems. User-friendly metarules can be defined in experiment files,
or in the main configuration file, `configuration.pl`. In either case, metarules
always belong to the configuration module, so when they are defined outside
`configuration.pl`, they must be preceded by the module identifier
`configuration`. `print_metarules/2` automatically adds the configuration
identifier in front of metarules it prints in user-friendly format:

```Prolog
?- print_metarules(user_friendly, [abduce,chain,identity,inverse,precon,postcon]).
configuration:abduce metarule 'P(X,Y)'.
configuration:chain metarule 'P(x,y):- Q(x,z),R(z,y)'.
configuration:identity metarule 'P(x,y):- Q(x,y)'.
configuration:inverse metarule 'P(x,y):- Q(y,x)'.
configuration:precon metarule 'P(x,y):- Q(x),R(x,y)'.
configuration:postcon metarule 'P(x,y):- Q(x,y),R(y)'.
true.
```

This is particularly useful when learning new metarules, as described in a later
section.

### Expanded metarules

The _expanded_ metarule format is Louise's internal representation of metarules,
in a form that is more convenient for meta-interpretation. Expanded metarules
are _encapsulated_ and have an _encapsulation atom_ in their head, holding the
metarule identifier and the existentially quantified variables in the metarule.

The following is an example of pretty-printing the `H22` metarules from the
previous examples in Louise's internal, expanded representation:

```Prolog
?- print_metarules(expanded, [abduce,chain,identity,inverse,precon,postcon]).
m(abduce,P,X,Y):-m(P,X,Y)
m(chain,P,Q,R):-m(P,X,Y),m(Q,X,Z),m(R,Z,Y)
m(identity,P,Q):-m(P,X,Y),m(Q,X,Y)
m(inverse,P,Q):-m(P,X,Y),m(Q,Y,X)
m(precon,P,Q,R):-m(P,X,Y),m(Q,X),m(R,X,Y)
m(postcon,P,Q,R):-m(P,X,Y),m(Q,X,Y),m(R,Y)
true.
```

Metarules printed in expanded format are useful when debugging a learning
attempt, at which point it is more informative to inspect Louise's internal
representation of a metarule to make sure it matches the user's expectation.

### Debugging metarules

Louise can also pretty-print metarules to a debug stream. In SWI-Prolog, debug
streams are associated with debug _subjects_ and so the `debug_metarules` family
of predicates takes an additional argument to determine the debug subject.

In the following example we show how to debug metarules in quantified format to
a debug subject named `pretty`:

```Prolog
?- debug(pretty), debug_metarules(quantified, pretty, [abduce,chain,identity,inverse,precon,postcon]).
% (Abduce) ∃.P,X,Y: P(X,Y)←
% (Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
% (Identity) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
% (Inverse) ∃.P,Q ∀.x,y: P(x,y)← Q(y,x)
% (Precon) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)
% (Postcon) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x,y),R(y)
true.
```

In the output above, the lines preceded by Prolog's comment character, `%`, are
printed by SWI-Prolog's debugging predicates. Debugging output can be directed
to a file to keep a log of a learning attempt. This is described in a later
section.

### Configuring a metarule format

The preferred metarule pretty-printing format for both top-level output and
debugging output can be specified in the configuration, by setting the option
`metarule_formatting/1` to one of the three metarule formats recognised by
`print_metarules` and `debug_metarules`. We show how this works below:

```Prolog
?- print_metarules([abduce,chain,identity,inverse,precon,postcon]), metarule_formatting(F).
(Abduce) ∃.P,X,Y: P(X,Y)←
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Identity) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)
(Inverse) ∃.P,Q ∀.x,y: P(x,y)← Q(y,x)
(Precon) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(x,y)
(Postcon) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x,y),R(y)
F = quantified.

?- print_metarules([abduce,chain,identity,inverse,precon,postcon]), metarule_formatting(F).
configuration:abduce metarule 'P(X,Y)'.
configuration:chain metarule 'P(x,y):- Q(x,z),R(z,y)'.
configuration:identity metarule 'P(x,y):- Q(x,y)'.
configuration:inverse metarule 'P(x,y):- Q(y,x)'.
configuration:precon metarule 'P(x,y):- Q(x),R(x,y)'.
configuration:postcon metarule 'P(x,y):- Q(x,y),R(y)'.
F = user_friendly.

?- print_metarules([abduce,chain,identity,inverse,precon,postcon]), metarule_formatting(F).
m(abduce,P,X,Y):-m(P,X,Y)
m(chain,P,Q,R):-m(P,X,Y),m(Q,X,Z),m(R,Z,Y)
m(identity,P,Q):-m(P,X,Y),m(Q,X,Y)
m(inverse,P,Q):-m(P,X,Y),m(Q,Y,X)
m(precon,P,Q,R):-m(P,X,Y),m(Q,X),m(R,X,Y)
m(postcon,P,Q,R):-m(P,X,Y),m(Q,X,Y),m(R,Y)
F = expanded.
```

Controlling the Vanilla meta-interpreter
----------------------------------------

At the heart of Louise is a MIL meta-interpeter called "Vanilla", found in the
file `<louise root>/src/vanilla.pl`. This section describes Vanilla and the
configuration options used to control its behaviour in Louise.

### A vanilla Prolog meta-interpreter

The name Vanilla is a reference to the "vanilla" Prolog meta-interpreter. A
Prolog meta-interpreter is a Prolog interpreter written in Prolog and a
"vanilla" Prolog meta-interpreter is a Prolog meta-interpreter that does only
that, implement Prolog in Prolog. Prolog meta-interpreters that are not vanilla
include ones that change the depth-first search execution order to a
breadth-first search, that keep a trace of goal execution, impose a depth limit
to resolution, etc. A vanilla Prolog meta-interpreter looks like this:

```prolog
prove(true).
prove((L,Ls)):-
    prove(L)
    ,prove(ls).
prove(L):-
    clause(L,Bs)
    ,prove(Bs).
```

The first clause in the Vanilla Prolog meta-interpreter stops
meta-interpretation when the leaf of a proof tree is found, signalled by the
atom `true`. The second clause splits the proof in two branches, one for the top
literal, `L`, on the proof-stack, `(L,Ls)`, and one for the remaining literals,
`Ls`. The last clause calls the built-in `clause/2` to "fetch" clauses from the
Prolog program database whose heads match the last literal popped from the proof
stack, `L`. Then the body literals of that clause are placed on the stack and
the proof goes on its merry way, until there are no more literals to prove. If
the literal, `L`, in the third clause is a "fact" (a definite clause with a head
but no body, very Halloween) then `clause/2` returns the atom `true` as its
body, which will then stop the proof at the leaf-handling first clause. 

The use of a stack of goals makes the meta-interpretation into a depth-first
search of a tree where the nodes are goals (literals) and the edges are
resolution steps, with unification and backtracking handled by the Prolog
engine. Prolog execution by depth-first search ("top-down") may cause the proof
to go into an infinite recursion when trying to prove a left-recursive
predicate. If meta-interpretation terminates and the atom `true` is the last
atom that remains to be proved, the proof succeeds, refuting the initial goal
literal given at the start of the proof, the so-called "top goal". Backtracking
then returns each set of bindings of the variables in the top-goal that make the
proof succeed: these are called "answers" in logic programming parlance.

In short, a vanilla Prolog meta-interpreter is an implementation of Prolog's
SLD-Resolution in Prolog; and a successful proof by a vanilla Prolog
meta-interpreter is a proof by SLD-Refutation of the top-goal. 

SLD-Resolution is sound and complete; meaning that each answer found by
SLD-Resolution is correct, and that if any correct answer exists it is returned
by SLD-Resolution. The implementation of SLD-Resolution by depth-first search,
as in Prolog, however, is not complete: because it can get stuck into infinite
left-recursions, it can fail to find a proof when one exists. On the other hand,
depthf-rist search is computationally cheap.

### An Inductive Prolog meta-interpreter

Though it's named Vanilla, the meta-interpreter in Louise is not really a
vanilla Prolog meta-interpreter: it is capable of not only deduction, as
ordinary Prolog, but also induction and abduction. Here's what those arcane
terms mean in the context of the definite clauses used in Prolog:

Given a definite clause `C` with head literal `H` and body literals `B_1, ...,
B_n`:

1. Deduction is the derivation of an atom `A` with the same predicate symbol and
   arity as `H`, the head of `C`.

2. Induction is the derivation of a clause `D` with a head literal `H_1` with
   the same predicate symbol and arity as `H`.

3. Abduction is the derivation of an atom `A` with the same predicate symbol and
   arity as one of `B_1, ..., B_n`, the body literals of `C`

Seen another way, given a clause `C` representing an implication, deduction is
the derivation of new consequents of the implication, induction is the
derivation of new implications, and abduction is the derivation of new
antecendents (conditions) of the implication.

Ordinary Logic Programming is, of course, deductive, so a successful proof by a
vanilla Prolog meta-interpreter is a deductive proof. Inductive Logic
Programming studies the use of logic programming for induction and MIL, in
particular, is a form of ILP that performs induction by SLD-Resolution. This is
achieved by what is essentially an inductive Prolog meta-interpreter. In Louise,
this inductive Prolog meta-interpreter is Vanilla.

"Vanilla" is named so because its structure is the same as that of the vanilla
Prolog meta-interpreter. There are only two differences between Vanilla and a
vanilla Prolog meta-interpreter: 

1. the vanilla Prolog meta-interpreter only has one argument, for a stack of
   goals to be proved, whereas Vanilla has additional arguments for bookkeeping
   and depth limiting. 

2. Vanilla has a special implementation of a clause-fetching predicate, to
   replace `clause/2` in the vanilla Prolog meta-interpreter. This one fetches
   clauses not only from the Prolog program database but also a) from the set of
   second-order definite clauses we call "metarules" and b) from the clauses of
   a hypothesis learned so-far.

The inclusion of second-order definite clauses in the background knowledge is
what makes Vanilla an inductive meta-interpreter. With Vanilla, learning
begins with an SLD-Refutation proof of the positive examples. During the proof,
by unification, the second-order variables in the metarules, become bound to
predicate symbols thus producing the clauses of a first-order hypothesis.

It is worth noting that the proof only searches for substitutions of the
variables in the background knowledge, including the second-order variables in
the metarules. That means there is no need to search for a first-order program.
A first-order program naturally "falls off" the proof, when the second-order
variables in the metarules are substituted for predicate symbols.

### Controlling Vanilla's clause-fetching

We will not go through all the extra arguments in Vanilla, but it is useful to
understand the options to control its clause-fetching predicate. We will call
this predicate "the clause-fetch" for simplicity. 

The clause fetch is controlled with a configuration option, `fetch_clauses/1`.
This takes as argument either the atom `all`, or a list of clause-fetching
options, as follows:

```
fetch_clauses(all).
fetch_clauses([builtins,bk,hypothesis,metarules]).
```

Below we attempt to elucidate the use of the `fetch_clauses/1` options to
control inductive meta-interpretation in Vanilla.

1. Option "all".

   The option "all" is equivalent to [builtins,bk,hypothesis,metarules]. It
   means that clauses will be fetched during meta-interpretation from all
   possible sources: the Prolog progam database, including builtins and library
   predicates, and first-order background knowledge, the set of clauses in the
   hypothesis learned so-far, and the metarules in the second-order background
   knowledge.

2. Option "builtins".

   This option means that Prolog builtins and library predicates will be called
   during meta-interpretation. The call of such predicates is handed off to the
   Prolog engine. In general, you want to always set this option to avoid errors
   about missing predicates during meta-interpretation, but it may be useful in
   debugging and experimentation.

3. Option "bk".

   This option means that all the predicates declared as first-order background
   knowledge for a learning target will be used in resolution by
   meta-interpretation. Again, you probably want to always set this option.

4. Option "hypothesis".

   This option means that the set of clauses in the hypothesis so-far will be
   used in resolution by meta-interpretation. This setting allows learning
   arbitrary recursive hypotheses, with clauses that resolve with each other.
   Without this setting, only recursive clauses that resolve with the positive
   examples can be derived.

5. Option "metarules".

   This option means that the definite clauses in the second-order background
   knowledge, the "metarules", will be used in resolution. Resolution with the
   metarules is how Vanilla constructs new clauses in a hypothesis, so leaving
   this settnig out essentially turns Vanilla into a deductive meta-interpreter.

Below we discuss some subtleties of this set of options, and their combinations.

### Arbitrary recursion

Including "hypothesis" in the `fetch_clauses/1` option allows Vanilla to resolve
the clauses of a hypothesis derived so-far with each other (and each clause with
itself) in a recursive call. The depth of recursion between the clauses of the
hypothesis so-far is not limited in any way. This makes it possible to learn
recursive hypotheses of arbitrary structure (given the right first- and
second-order background knowledge) but it also makes it possible to enter an
infinite recursion.

This is particularly the case under two conditions:

1. A left-recursive clause is added to the current hypothesis.

2. A recursive hypothesis does not exist that entails all the positive examples,
   with respect to the background knowledge.

When the first condition, left-recursion, obtains, the depth-first search in
Vanilla will enter an infinite left-recursion. The result is an ugly error
informing the user that the Prolog engine's call-stack has been blown up.

When the second condition obtains, Church and Turing wag their finger at the
user and admonish: "didn't we tell you there are logical statements whose truth
cannot be decided in finite time?". Sadly, if a recursive program that covers
our training example does not exist, the only way to find that out is to try and
prove one anyway, resulting in an infinite recursion.

These two problems are addressed in Louise by, respectively: tabling (a.k.a.
SLG-Resolution); and constraints on the substituions of variables in metarules
during resolution (we call those "meta-substitutions").

### Controlling recursion with tabling

Tabling is an alternative model of execution for Prolog programs that a)
replaces depth-first search with breadth-first search, and b) stores interediate
goals of a proof in a table so they don't have to be re-derived (a process also
known as "memoization"). The combination of memoization and breadth-first search
makes it possible for left-recursive Prolog programs to terminate (unless they
can't termiante because of right-recursion, that is). 

[XSB Prolog](https://xsb.com/xsb-prolog/) is best known for its tabling
facilities, but SWI-Prolog (used to implement Louise) has included tabling for a
while now, thanks to the indefatigable work of Jan Wielemaker (one of my
personal all-time programming heroes). Vanilla can take advantage of
SWI-Prolog's tabling to avoid going into infintie left-recursions during
learning.

Tabiling Vanilla is managed by two configuration options, shown here with their
default settings:

```prolog
table_meta_interpreter(true).
untable_meta_interpreter(true).
```

The first option `table_meta_interpreter/1`, causes Vanilla to be tabled before
execution. The second option, `untable_meta_interpreter/1` removes Vanilla from
tabling after execution.

When `table_meta_interpreter/1` is set to `true`, the entire meta-interpreter is
tabled. This is a bit of a hack to allow tabling's defense against
left-recursion to be extended to the predicates in the background knowledge
(which would otherwise have to be tabled individually).

The option `untable_meta_interpreter/1` is useful because it is possible, when
an experiment file is updated, that the definition of examples and background
knowledge in it is not correctly updated in the Prolog database. This seems to
be a bit of a bug in the way Louise handles experiment files as Prolog modules.

Setting both options to `true` has the effect of making learning queries, after
the first, run considerably faster for some problems.

Tabling itself comes at a cost: breadth first search has exponential
(asymptotic) space complexity. In real terms this means that, for hard learning
problems, where the proof tree built by Vanilla during Resolution grows too
large, Prolog will run out of RAM for tabling. This can be addressed, partly, by
increasing the tabling space, e.g. as follows:

```prolog
:-set_prolog_flag(table_space, 34_359_738_368). % 32 GB
```

You will find a few of those directives in `load_project.pl` and
`load_headless.pl` at the root of Louise's installation directory.

Unfortunately, there are learning problems for which you'll never have enough
RAM.

### Controlling recursion with constraints

The aletarnative to tabling is imposing constraints on the set of clauses that
can be constructed during a proof with Vanilla; in other words, blocking some
clauses from being learned in the first place.

Such constraints are currently implemented in Louise using the predicate
`metarule_constraints/2`. This is declared as a multifile and dynamic predicate
in the configuration option, and so its clauses can be declared by each
individual experiment file that needs it. You can find examples of metarule
constraints in the configuration module, in `configuration.pl` (in Louise's
project root directory).

The perceptive reader will note that imposing constraints on the clauses that
can be constructed during an inductive proof makes the process _incomplete_
meaning that not all programs are possible to learn anymore. Still, there may be
an acceptable trade-off in being able to learn some programs, while ensuring
termination. Metarule constraints offer this trade-off.

The use of constraints to avoid constructing left-recursive clauses during
Resolution is explained in the following example:

```
<louise root>/data/examples/constraints.pl
```

Constraints can also be used to shape a hypothesis to make it shorter, easier to
learn and easier to read. An example of this can be found in:

```
<louise root>/data/examples/recipes.pl
```

### Depth-limited recursion

There is a third way in which Vanilla can be made to learn recursive predicates
of arbitrary structure, but without tabling or constraints. This way is to avoid
resolving the clauses in the hypothesis learned so-far with each other (and each
clause with itself). This is done by leaving `hypothesis` out of the list of
options in `fetch_clauses/1`:

```
% All sets of clauses, except "hypothesis"

fetch_clauses([builtins,bk,metarules]).
```

With the `hypothesis` option left out, Vanilla can still learn recursive
hypotheses, in two ways: 

1. By recursion with positive examples

2. By recursion with the metarules

#### Recursion with the positive examples

The first option only allows a limited form of recursive hypotheses to be
learned, specifically, hypotheses whose clauses resolve with the positive
examples, in a single step of recursion. This kind of recursion is enabled when
the setting `clause_limit(0)` is set in the configuration. This setting will
cause the positive examples to be added to the Prolog database, and restrict
resolution between metarules and examples to a single resolution step. 

Recursive clauses learned in this way can still be resolved, recursively, with
other clauses in the _completed_ hypothesis, so they are not as limited as it
may initially appear. For example, the experiment files `hello_world.pl` and
`tiny_kinship.pl` show how a recursive theory of the `ancestor` family relation
can be learned with `clause_limit(0)`.

#### Recursion with the metarules

Recursion with the metarules is enabled by adding the option `metarules` to the
list of options in `fetch_clauses/1`. Unlike recursion with the positive
examples, resolving the metarules with each other and the first-order background
knowledge allows for arbitrary recursion to be learned, but there is a catch: a
metarule can infinitely resolve with itself as long as it has any body literals
that have the same arity, and therefore, can unify, with its head literal.

To avoid this infinite recursion, louise imposes a limit on the number of times
that a metarule can resolve with itself. This limit is set in the configuration
option `clause_limit/1`.

When metarules resolve with each other and the first-order backrgound knowledge,
the instances of metarules derived become clauses in the hypothesis so-far.
Including the option `hypothesis` to the options for `fetch_clauses/1` allows
those clauses to resolve with each other, as discussed earlier. Each of those
clauses needs to be derived once, then can resolve an arbitrary number of times
with itself or other clauses.

Thus, as long as the option `hypothesis` is included in the options for
`fetch_clauses/1`, the value of `clause_limit/1` controls _the cardinality of
the set of program clauses participating in any refutation sequence of a
positive example_.

The flip side of that is that leaving `hypothesis` out of the options for
`fetch_clauses/1` will effectively restrict the number of times clauses in
the hypothesis so-far can resolve with themselves, _to 0_.

In turn, the value of `k` in `clause_limit(k)` will now effectively become _the
total number of program clauses included in any one refutation sequence_. If
some of those clauses are recursive, multiple instances of those clauses will
be included in a refutation sequence: one for each step of recursion.

What this means is that in order for a recursive program to be learned without
`hypothesis` included in the options for `fetch_clauses/1` the value of
`clause_limit/1` must be large enough for sufficient recursive steps to be
taken, to successfully complete a proof.

In a sense, keeping the clauses in the hypothesis so far from participating in
resolution turns the clause limit into a resolution depth limit.

This is best explained with an example. 

#### An example of clause limits as resolution depth limits

Consider the following learning
problem, defined in `data/examples/findlast.pl`:

```prolog
?- list_mil_problem(list_last/2).
Positive examples
-----------------
list_last([a,b,c,d,e,f,g,h,i],i).

Negative examples
-----------------

Background knowledge
--------------------
tail/2:
tail([A|B],B).

head/2:
head([A|B],A).

empty/1:
empty([]).

Metarules
---------
(Tailrec) ∃.P,Q ∀.x,y,z: P(x,y)← Q(x,z),P(z,y)
(Midcon) ∃.P,Q,R,S ∀.x,y,z: P(x,y)← Q(x,z),R(z),S(x,y)

true.
```

Louise can learn the target predicate for this learning problem with the
following configuration options:

```prolog
?- _Options = [clause_limit/1, fetch_clauses/1, table_meta_interpreter/1, untable_meta_interpreter/1], nl, list_options(_Options).

clause_limit(2)
fetch_clauses([builtins,bk,hypothesis,metarules])
table_meta_interpreter(true)
untable_meta_interpreter(true)
true.

?- learn(list_last/2).
list_last(A,B):-tail(A,C),list_last(C,B).
list_last(A,B):-tail(A,C),empty(C),head(A,B).
true.
```

The target theory for this problem consists of two clauses, one recursive and
one the base-case, so the clause limit must be set to 2. These clauses obviously
have to be able to resolve with each other so the option `hypothesis` is added
to the list of options in `fetch_clauses/1`. Unconstraint resolution between the
clauses of the hypothesis might well lead to an infinite recursion, especially
as left-recursive clauses are tried during the proof, so
`table_meta_interpreter/1` is set to `true`.

Now, suppose we remove tabling by setting `table_meta_interpreter(false)`. If
you try to run the learning query above, now, the proof will go into an infinite
recursion and eventually blow the call stack:

```
?- learn(list_last/2).
Failed to print resource exception due to lack of space
error(resource_error(stack),stack_overflow{choicepoints:4087498,depth:2725024,environments:2725012,globalused:958022,localused:1085743,non_terminating:[frame(2725008,vanilla:prove(m/3,2,[2],[1],[4],[2],_245253924),[]),frame(2725007,vanilla:prove((',')/2,2,[2],[1],[4],[2],_245253984),[])],stack_limit:2097152,trailused:53223})
ERROR: Stack limit (2.0Gb) exceeded
ERROR:   Stack sizes: local: 1.0Gb, global: 0.9Gb, trail: 52.0Mb
ERROR:   Stack depth: 2,725,024, last-call: 0%, Choice points: 4,087,498
ERROR:   Possible non-terminating recursion:
ERROR:     [2,725,008] vanilla:prove(<compound m/3>, 2, [length:2], [length:1], [length:4], [length:2], _245253924)
ERROR:     [2,725,007] vanilla:prove(<compound (',')/2>, 2, [length:2], [length:1], [length:4], [length:2], _245253984)
   Exception: (2,723,523) vanilla:prove((m(list_last, [], _245120092), m(list_last, _245120092, _245119912)), 2, [(m(tailrec, _3182, _3184):-m(_3182, _3196, _3198), m(_3184, _3196, _3212), m(_3182, _3212, _3198)), (m(midcon, _3110, _3112, _3114, _3116):-m(_3110, _3128, _3130), m(_3112, _3128, _3144), m(_3114, _3144), m(_3116, _3128, _3130))], [list_last], [bk, builtins, hypothesis, metarules], [m(tailrec, list_last, list_last), m(tailrec, list_last, tail)], _245119982) ? abort
% Execution Aborted
```

The infinite recursion is happening during resolution of the clauses in the
hypothesis with each other, and each clause with itself, so let's remove
`hypothesis` from the list of options in `fetch_clauses/1` and see what happens;
note that we keep not-tabling the meta-interpreter:

```
?- _Options = [clause_limit/1, fetch_clauses/1, table_meta_interpreter/1, untable_meta_interpreter/1], nl, list_options(_Options).

clause_limit(2)
fetch_clauses([builtins,bk,metarules])
table_meta_interpreter(false)
untable_meta_interpreter(true)
true.

?- learn(list_last/2).
[]
true.
```

This time learning terminates, but no clauses are learned (the empty list
printed at the end of the learning query is the empty hypothesis). That happens
because Vanilla cannot prove the target theory without resolving its recursive
clause with itself, and finally with the base-case clause.

More precisely, in order to compute the necessary recursion in the target
theory, Vanilla must resolve the recursive clause in it once for each element in
the list given as an example, the list [a,b,c,d,e,f,g,h,i]. This list has nine
elements, so the recursive clause in the target theory must resolve eight times
with itself, and one final time with the base-case clause, before it can
refute (and prove) the example.

Suppose then that we set clause-limit to 9:

```prolog

?- _Options = [clause_limit/1, fetch_clauses/1, table_meta_interpreter/1, untable_meta_interpreter/1], nl, list_options(_Options).

clause_limit(9)
fetch_clauses([builtins,bk,metarules])
table_meta_interpreter(false)
untable_meta_interpreter(true)
true.

?- learn(list_last/2).
list_last(A,B):-tail(A,C),list_last(C,B).
list_last(A,B):-tail(A,C),empty(C),head(A,B).
true.
```

Note we change nothing else- no `hypothesis` in the `fetch_clauses/1` options
and no tabling. And yet, the proof terminates and the target theory is learned.
Why is this?

That's because even without allowing the clauses in the hypothesis to resolve
with each other, and themselves, the same result can be obtained by allowing the
_metarules_ to resolve with each other. After all, the clauses in the hypothesis
are specialisations of the metarules with respect to the background knowledge.
More to the point, each of thos clauses is derived by resolution between the
metarules, and the first-order background knowledge, in the first place. Thus,
we don't need to store those clauses in the hypothesis so-far and reuse them, we
can just derive them "on the fly" from the metarules at each step of the proof
as needed.

Unfortunately, each such clause needs to be derived "on the fly" a sufficient
number of times to complete the proof. Hence the need for a sufficiently high
clause limit: the clause limit is now the total number of clauses that will be
constructed during resolution. Or, in other words, it is an absolute limit on
the number of resolution steps from the start to the end of a proof.

Here are some further learning queries to help clarify the semantics of
`clause_limit/1`, when `hypothesis` is excluded from the list of options in
`fetch_clauses/1`:

```prolog
?- _Options = [clause_limit/1, fetch_clauses/1, table_meta_interpreter/1, untable_meta_interpreter/1], nl, list_options(_Options).

clause_limit(8)
fetch_clauses([builtins,bk,metarules])
table_meta_interpreter(false)
untable_meta_interpreter(true)
true.

?- learn(list_last/2).
[]
true.
```

Above, the clause limit of 8 is not sufficient for the proof in Vanilla to "walk
over" all the nine elements in the list [a,b,c,d,e,f,g,h,i]. Suppose we were to
remove one element from the list:

```prolog
% First element removed from the list

?- findlast:positive_example(list_last/2, E).
E = list_last([b, c, d, e, f, g, h, i], i).

?- _Options = [clause_limit/1, fetch_clauses/1, table_meta_interpreter/1, untable_meta_interpreter/1], nl, list_options(_Options).

clause_limit(8)
fetch_clauses([builtins,bk,metarules])
table_meta_interpreter(false)
untable_meta_interpreter(true)
true.

?- learn(list_last/2).
list_last(A,B):-tail(A,C),list_last(C,B).
list_last(A,B):-tail(A,C),empty(C),head(A,B).
true.
```

With one element removed, only eight steps of resolution are needed to "walk
over" the entire list, so `clause_limit(8)` now suffices to learn the target
theory.

Suppose we put back the missing element and set the clause limit to 10. That is,
we will allow one more resolution step to be taken, beyond what is sufficient to
walk over the entire list. What will happen now?

```prolog
?- findlast:positive_example(list_last/2, E).E = list_last([a, b, c, d, e, f, g, h|...], i).

?- _Options = [clause_limit/1, fetch_clauses/1, table_meta_interpreter/1, untable_meta_interpreter/1], nl, list_options(_Options).

clause_limit(10)
fetch_clauses([builtins,bk,metarules])
table_meta_interpreter(false)
untable_meta_interpreter(true)
true.

?- learn(list_last/2).list_last(A,B):-tail(A,C),list_last(C,B).
list_last(A,B):-list_last(A,C),empty(C),head(A,B).
list_last(A,B):-tail(A,C),empty(C),head(A,B).
list_last(A,B):-tail(A,C),empty(C),list_last(A,B).
list_last(A,B):-tail(A,C),empty(C),tail(A,B).
true.
```

This time resolution goes on for too long and the result is a hypothesis with
over-general clauses.

#### Conclusions

As can be seen from the discussion and examples in the previous sections, there
are several trade-offs to be made between allowing arbitrary recursion, ensuring
termination, having enough RAM, and impose strict resolution depth limits.

These trade-offs are managed by setting appropriate values for the configuration
options `clause_limit/1` and `fetch_clauses/1`, that control the behaviour of
the Vanilla meta-intepreter (metarule constraints can also be used).

Debugging training data
-----------------------

In this section we discuss the facilities offered by Louise to debug learning
problems.

### Inspecting the current configuration options

Most of the functionality in Louise is controlled by the settings of
configuration options in a configuration file, `configuration.pl`, at the top
directory of the Louise installation.

The current settings of configuration options can be inspected with a call to
the predicate `list_config/0`. We show an example below:

```prolog
?- list_config.
clause_limit(0)
encapsulation_predicate(m)
example_clauses(call)
experiment_file(data/examples/tiny_kinship.pl,tiny_kinship)
fetch_clauses(all)
fold_recursive(false)
generalise_learned_metarules(false)
invented_symbol_prefix($)
learner(louise)
listing_limit(10)
max_error(0,0)
max_invented(0)
metarule_formatting(quantified)
metarule_learning_limits(none)
recursive_reduction(false)
reduce_learned_metarules(false)
reduction(plotkins)
resolutions(5000)
table_meta_interpreter(true)
theorem_prover(resolution)
unfold_invented(false)
untable_meta_interpreter(true)
true.
```

Consult the documentation of the `configuration.pl` file to learn about the
various configuration options listed above.

Various sub-systems and libraries in Louise also have their own configuration,
some of which are imported by `configuration.pl`. These options are not listed
by `list_config/0`. You can inspect those options with `print_or_debug/3` with
the last argument `all`:

```prolog
?- print_config(print,user_output,all).
configuration:clause_limit(0)
configuration:encapsulation_predicate(m)
configuration:example_clauses(call)
configuration:experiment_file(data/examples/tiny_kinship.pl,tiny_kinship)
configuration:fetch_clauses(all)
configuration:fold_recursive(false)
configuration:generalise_learned_metarules(false)
configuration:invented_symbol_prefix($)
configuration:learner(louise)
configuration:listing_limit(10)
configuration:max_error(0,0)
configuration:max_invented(0)
configuration:metarule_formatting(quantified)
configuration:metarule_learning_limits(none)
configuration:recursive_reduction(false)
configuration:reduce_learned_metarules(false)
configuration:reduction(plotkins)
configuration:resolutions(5000)
configuration:table_meta_interpreter(true)
configuration:theorem_prover(resolution)
configuration:unfold_invented(false)
configuration:untable_meta_interpreter(true)
evaluation_configuration:decimal_places(2)
evaluation_configuration:evaluate_atomic_residue(include)
evaluation_configuration:success_set_generation(sld)
reduction_configuration:call_limit(none)
reduction_configuration:depth_limit(10000)
reduction_configuration:derivation_depth(9)
reduction_configuration:inference_limit(10000)
reduction_configuration:meta_interpreter(solve_to_depth)
reduction_configuration:program_module(program)
reduction_configuration:recursion_depth(10)
reduction_configuration:time_limit(2)
sampling_configuration:k_random_sampling(randset)
thelma_cofiguration:default_ordering(lower)
thelma_cofiguration:depth_limits(2,1)
thelma_cofiguration:metarule_functor($metarule)
true.
```

The configuration options listed by `print_or_debug/3` are preceded by the
module identifier of the module in which they are defined.

Louise has many configuration options. When only a few of those need to be
inspected, the predicate `list_options/1` can be used instead of `list_config/0`
and `print_config/3`. `list_options/1` takes as argument a list of configuration
options, as predicate indicators (a predicate symbol and arity) and prints out
only those options' values:

```prolog
?- _Options = [experiment_file/2, clause_limit/1, fetch_clauses/1, max_invented/1, max_error/2, reduction/1, resolutions/1], nl, list_options(_Options).

experiment_file(data/examples/tiny_kinship.pl,tiny_kinship)
clause_limit(0)
fetch_clauses(all)
max_invented(0)
max_error(0,0)
reduction(plotkins)
resolutions(5000)
true.
```

### Inspecting the elements of a MIL Problem

The elements of a MIL problem defined in an experiment file can be listed to the
console with a call to the predicate `list_mil_problem/1`. This predicate takes
as argument the symbol and arity of a learning target defined in the currently
loaded experiment file and prints out the examples, background knowledge and
metarules declared for that target in that experiment file.

In the following example we call `list_mil_problem/1` to list the elements of
the MIL problem for the `grandmother/2` learning target, defined in the
`data/examples/tiny_kinship.pl` experiment file:

```prolog
?- list_mil_problem(grandmother/2).
Positive examples
-----------------
grandmother(alexandra,stassa).
grandmother(paraskevi,stassa).

Negative examples
-----------------
:-grandmother(stathis,stassa).
:-grandmother(stefanos,stassa).

Background knowledge
--------------------
mother/2:
mother(alexandra,kostas).
mother(paraskevi,dora).
mother(dora,stassa).

parent/2:
parent(A,B):-father(A,B).
parent(A,B):-mother(A,B).

Metarules
---------
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
true.
```

Listing a MIL problem is particularly helpful when multiple learning targets are
defined in an experiment file, at which point it can be confusing to look at the
experiment file itself and try to determine exactly what training data goes to
what learning target.

### Inspecting the elements of an encapsulated MIL problem

At the start of any learning attempt the elements of a MIL problem are
transformed by Louise to an internal representation by _encapsulation_.
Encapsulation "wraps" the predicate symbol and arguments of literals in a clause
into a new predicate, so that, for example, the atom `p(x,y)` becomes `m(p,x,y)`
and the clause `p(x,y):- q(x,y)` becomes `m(p,x,y):- m(q,x,y)`.  Encapsulation
facilitates resolution between metarules, that are second-order, and the
first-order background knowledge and examples. Encapsulation also makes
resolution between metarules and other clauses decidable by "lowering" them to
the first-order (even unification is undecidable in second order logic).

The encapsulated elements of a MIL Problem can be listed for inspection with a
call to the predicate `list_encapsulated_problem/1`. This predicate, too, takes
as argument the predicate symbol and arity of a learning target in the currently
loaded experiment file.

In the following example we show how to list the encapsulated elements of the
MIL problem for the `grandmother/2` target in `data/examples/tiny_kinship.pl`:

```prolog
?- list_encapsulated_problem(grandmother/2).
Positive examples
-----------------
m(grandmother,alexandra,stassa).
m(grandmother,paraskevi,stassa).

Negative examples
-----------------
:-m(grandmother,stathis,stassa).
:-m(grandmother,stefanos,stassa).

Background knowledge
--------------------
mother/2:
m(mother,alexandra,kostas).
m(mother,paraskevi,dora).
m(mother,dora,stassa).

parent/2:
m(parent,A,B):-father(A,B).
m(parent,A,B):-mother(A,B).
father(stathis,kostas).
father(stefanos,dora).
father(kostas,stassa).
mother(alexandra,kostas).
mother(paraskevi,dora).
mother(dora,stassa).


Metarules
---------
m(chain,P,Q,R):-m(P,X,Y),m(Q,X,Z),m(R,Z,Y)
true.
```

Note that only the of clauses in the `Background knowledge` section are
encapsulated, while their bodies are left alone. More precisely, the only
literals that are encapsulated are the literals of the predicates defined as
background knowledge for a learning target, in `background_knowledge/2`. That is
because those literals will be unified with the literals of metarules during
resolution. Literals of other predicates do not directly participate in
resolution with metarules and so do not need to be encapsulated. This also makes
it easier to use arbitrary Prolog predicates in the body of background
predicates, whereas the heads of background predicates' clauses must be strictly
_datalog_ (i.e. they cannot include functions, or what Prolog calls "compound
terms").

In the example above, the heads of the clauses of `mother/2` and `parent/2` are
encapsulated because they are declared as background knowledge for the learning
target, `grandmother/2`. `mother/2` is defined extensionally, as a set of ground
atoms and so it does not have a body to encapsulate.

Note that there is a second set of `mother/2` clauses that are _not_
encapsulated. These are found in the closure of `parent/2` along with the
extensional definition of `father/2`. During learning, clauses that are not
encapsulated will not be unified, and so resolved, with metarules' literals, but
instead will be interpreted only to complete the learning proof.

### Listing MIL problem statistics

When the elements of a MIL problem are too large (e.g. when there are many
examples or many clauses in the background knowledge) listing them is not very
helpful. The predicate `list_problem_statistics/1` lists only the numbers of
examples and the numbers and predicate symbols or identifiers of background
predicates and metarules declared for a learning target. 

Below we show an example of listing the problem statistics for the `ancstor/2`
target defined in `data/examples/tiny_kinship/pl`:

```prolog
?- list_problem_statistics(ancestor/2).
Positive examples:    10
Negative examples:    10
Background knowledge: 1 [parent/2]
Metarules:            2 [tailrec,identity] 
true.
```

### Listing the Top Program

The Top Program constructed by Louise's Top Program Construction algorithm can
be inspected by a call to the predicate `list_top_program/1`. This predicate
takes as argument the symbol and arity of a learning target and outputs the
results of the two steps of Top Program Construction, the generalisation step,
where all the clauses that entail positive examples with respect to background
knowledge are constructed; and the specialisation step, where all the clauses in
the previous step that entail negative examples are thrown out.

Below, we list the Top Program for the `grandfather/2` learning target defined
in `data/examples/tiny_kinship.pl`:

```prolog
?- list_top_program(grandfather/2).
Generalisation:
---------------
grandfather(A,B):-father(A,C),father(C,B).
grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).
grandfather(A,B):-parent(A,C),father(C,B).
grandfather(A,B):-parent(A,C),parent(C,B).
Length:5

Specialisation:
---------------
grandfather(A,B):-father(A,C),father(C,B).
grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).
Length:3
true.
```

Note how the `Specialisation` step has removed two over-general clauses, found
to entail negative examples with respect to background knowledge.

### Listing the Top Program with more detail

The implementation of the TPC algorithm in Louise constructs a Top Progam in
successive stages: first a set of ground _metasubstitutions_ is derived, which
are then applied to the corresponding metarules to derive a set of encapsulated
clauses. Finally, clause are excapsulated, turning them back to clauses of the
learning targets.

Similar to `list_top_program/1` the predicate `list_top_program/3` can list the
Top Program but this time in varying stages of its construction, particularly
with respect to application of metasubstitutions and encapsulation. The first
argument of `list_top_program/3` is the symbol and arity of a learning target,
as in `list_top_program/1`. The second argument determines whether to show the
application of derived metasubstitutions to their corresponding metarules. The
third argument determines whether to excapsulate the resulting clauses, or not.

In the example below we list the Top Program learned from the elements of the
MIL problem for `grandfather/2` in `data/examples/tiny_kinship.pl`. The second
and third argument are given as "false" to show the ground metasubstitutions
derived during learning before they are applied to their corresponding
metarules:

```prolog
?- list_top_program(grandfather/2,false,false).
Generalisation:
---------------
m(chain,grandfather,father,father)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E)).
m(chain,grandfather,father,parent)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E)).
m(chain,grandfather,husband,grandmother)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E)).
m(chain,grandfather,parent,father)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E)).
m(chain,grandfather,parent,parent)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E)).
Length:5

Specialisation:
---------------
m(chain,grandfather,father,father)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E)).
m(chain,grandfather,father,parent)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E)).
m(chain,grandfather,husband,grandmother)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E)).
Length:3
true.
```

In the output above, the ground metasubstitutions are the atoms
`m(chain,grandfather,father,father)` etc. The first argument in such a
metasubstitution atom is the identifier of a metarule and the remaining
arguments are predicate symbols and constants to be substituted for
existentially quantified variables in the metarule. The literals of the metarule
(in encapsulated form with a non-ground metasubstitution atom in its head) are
associated to the ground metasubstitution by the operator `-/2`.

In the following example we repeat the same call but with the second argument
set to "true" to apply the ground metasubstitutions to their corresponding
metarules:

```prolog
?- list_top_program(grandfather/2,true,false).
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

Setting the third argument of `list_top_program/3` to `true` will produce the
same result as calling `list_top_program/1`.

Inspecting the stages of Top Program construction with `list_top_program/1` and
`list_top_program/3` is useful to understand the inner workings of the Top
Program Construction algorithm.

Note however that both predicates must first build the Top Program for a
learning target, before they can list it. If the Top Program is very large, this
can take a while and the resulting listing will flood the SWI-Prolog console and
most of it will be lost.

Debugging learning attempts
---------------------------

You can log many steps of the learning algorithms in Louise to the console, or
redirect the logs to a file.

Much of the code in Louise includes logging statements ultimately calling on the
SWI-Prolog predicate `debug/3`. This predicate takes as argument the name of a
debug subject associated with a debug stream, which can be either the default
debug stream or a file on disk.

The debug messages available to the user (at least those available without
having to read the source code) are listed near the top of the main
configuration file, `configuration.pl`, as arguments of `debug/1` directives. We
copy the relevant lines of the configuration file below:

```prolog
:-nodebug(_). % Clear all debug topics.
%:-debug(learn). % Debug learning steps.
%:-debug(metasubstitution). % Debug metasubstitutions.
%:-debug(top_program). % Debug Top program construction.
%:-debug(reduction). % Debug Top program reduction.
%:-debug(dynamic). % Debug dynamic learning.
%:-debug(predicate_invention). % Debug predicate invention.
%:-debug(learn_metarules). % Debug metarule learning
%:-debug(learned_metarules). % Debug new metarules
%:-debug(metarule_grounding). % Debug metarule template specialisation
%:-debug(examples_invention). % Debug examples invention.
%:-debug(evaluation). % Debug learned program evaluation
```

By default, only the directive `:-nodebug(_)` is uncommented, which deactivates
all debugging messages. To activate a debug subject, uncomment the directive
that has that subject as an argument. For example, to activate the `learn`
subject, uncomment the following line:

```prolog
:-debug(learn). % Debug learning steps.
```

To activate a debug subject you can also call `debug/1` at the SWI-Prolog
console.

In the example below we show the logging outputs for the debug subjects `learn`,
`top_program` and `reduction` produced during an attempt of learning the
`grandfather/2` relation from the elements of the MIL problem defined in
`data/examples/tiny_kinship.pl`:

```prolog
?- debug(learn), debug(top_program), debug(reduction), learn(grandfather/2).
% Encapsulating problem...
% Constructing Top program...
% Constructing Top program...
% Generalised Top program
% m(chain,grandfather,father,father)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% m(chain,grandfather,father,parent)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% m(chain,grandfather,husband,grandmother)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% m(chain,grandfather,parent,father)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% m(chain,grandfather,parent,parent)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% Specialised Top program
% m(chain,grandfather,father,father)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% m(chain,grandfather,father,parent)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% m(chain,grandfather,husband,grandmother)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% Applied metarules
% m(grandfather,A,B):-m(father,A,C),m(father,C,B)
% m(grandfather,A,B):-m(father,A,C),m(parent,C,B)
% m(grandfather,A,B):-m(husband,A,C),m(grandmother,C,B)
% Reducing Top program...
% Reducing Top program by Plotkin's algorithm...
% Reduced Top program:
% m(father,stathis,kostas)
% m(father,stefanos,dora)
% m(father,kostas,stassa)
% m(parent,A,B):-m(father,A,B)
% m(parent,A,B):-mother(A,B)
% m(husband,A,B):-m(father,A,C),mother(B,C)
% m(grandmother,A,B):-mother(A,C),m(parent,C,B)
% mother(alexandra,kostas)
% mother(paraskevi,dora)
% mother(dora,stassa)
% m(grandfather,A,B):-m(father,A,C),m(parent,C,B)
% m(grandfather,A,B):-m(husband,A,C),m(grandmother,C,B)
% m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E)
% Excapsulating hypothesis...
grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).
true.
```

In the example above, the debug outputs are similar to the output of the
`list_top_program/[1,3]` predicates discussed in a previous section, however it
is possible to produce considerably more, and more diverse, output during
logging, than with those two predicates.

In fact, it is possible to produce _too much_ output while logging a learning
attempt, particularly when there are many clauses that can be constructed and
when many debug subjects are activated (the debug subject `metasubstitution` is
particularly prolific). The SWI-Prolog console has a limited-size buffer and
text that overflows this buffer is discarded. The result is that the early
logging output of very long learning attempts will be lost.

To retain the debugging output you can increase the size of the SWI-Prolog
console buffer (consult the SWI-Prolog documentation on how to do that).
Alternatively, you can redirect debugging output to a file on disk by appending
the name of the file to the debug subject with the operator `>/2`. You can do
that either in one of the `debug/1` directives in the configuration file, or in
the console. Below we repeat the above example showing how to redirect output to
a log file on disk from the console:

```prolog
?- nodebug(_), _F = 'log_file.log', debug(learn>_F), debug(top_program>_F), debug(reduction>_F), learn(grandfather/2).
grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).
true.
```

The `log_file.log` file will now have been written to disk in the directory
where you started the current SWI-Prolog process. If you've been starting Louise
by clicking the `load_project.pl` or `load_headless.pl` project load files, the
SWI-Prolog process will have started in the top-directory of the Louise
installation, so you should find your log file in that directory.

Below, we cat the contents of the `log_file.log` file that was written in the
previous example to disk at the top-directory of the Louise installation:

```PowerShell
PS C:\...\louise> cat .\log_file.log
% Encapsulating problem...
% Constructing Top program...
% Constructing Top program...
% Generalised Top program
% m(chain,grandfather,father,father)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% m(chain,grandfather,father,parent)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% m(chain,grandfather,husband,grandmother)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% m(chain,grandfather,parent,father)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% m(chain,grandfather,parent,parent)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% Specialised Top program
% m(chain,grandfather,father,father)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% m(chain,grandfather,father,parent)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% m(chain,grandfather,husband,grandmother)-(m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E))
% Applied metarules
% m(grandfather,A,B):-m(father,A,C),m(father,C,B)
% m(grandfather,A,B):-m(father,A,C),m(parent,C,B)
% m(grandfather,A,B):-m(husband,A,C),m(grandmother,C,B)
% Reducing Top program...
% Reducing Top program by Plotkin's algorithm...
% Reduced Top program:
% m(father,stathis,kostas)
% m(father,stefanos,dora)
% m(father,kostas,stassa)
% m(parent,A,B):-m(father,A,B)
% m(parent,A,B):-mother(A,B)
% m(husband,A,B):-m(father,A,C),mother(B,C)
% m(grandmother,A,B):-mother(A,C),m(parent,C,B)
% mother(alexandra,kostas)
% mother(paraskevi,dora)
% mother(dora,stassa)
% m(grandfather,A,B):-m(father,A,C),m(parent,C,B)
% m(grandfather,A,B):-m(husband,A,C),m(grandmother,C,B)
% m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E)
% Excapsulating hypothesis...
```

Note that in the examples above, the logging output for the specified debug
subjects was not output to the SWI-Prolog console anymore. This is because we
called `debug(_)` turning off all debug subjects, first, and SWI-Prolog takes
`debug(learn)` and `debug(learn>_F)` to be different bebug subjects. This means
that you can mirror the logging output to the SWI-Prolog console by activating
the two debug subjects separately. We show how to do this below for the `learn`
debug subject:

```prolog
?- nodebug(_), debug(learn), debug(learn>'log_file.log'), learn(grandfather/2).
% Encapsulating problem
% Constructing Top program...
% Reducing Top program...
% Excapsulating hypothesis
grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).
true.
```

Note that you have to call `debug(_)` at the start of the query above, to turn
off the debugging subject for `debug(learn>_F)` that was previously activated,
and that would otherwise suck up the logging output.

Now, if you cat the log file you should see the following log output:

```PowerShell
PS C:\...\louise> cat .\log_file.log
% Encapsulating problem
% Constructing Top program...
% Reducing Top program...
% Excapsulating hypothesis
```

You can also redirect debug output to a log file in a sub-directory of the
Louise installation. The default installation creates a directory `louise/logs`
for just this purpose. For example, you can redirect the logging output of the
debug subject `learn` to the file `louise/logs/logging_file_2.log` as follows:

```prolog
?- nodebug(_), debug(learn), debug(learn>'logs/log_file.log'), learn(grandfather/2).
% Encapsulating problem
% Constructing Top program...
% Reducing Top program...
% Excapsulating hypothesis
grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).
true.
```

Now we can inspect that file to find our output logs:

```PowerShell
PS C:\...\louise> cat .\logs\log_file.log
% Encapsulating problem
% Constructing Top program...
% Reducing Top program...
% Excapsulating hypothesis
```

Remember to leave computer memory tidy and close a log file when you're done
with it:

```prolog
?- close('logs/log_file.log').
true.
```

Experiment scripts
------------------

Louise stores experiment scripts in the directory `data/scripts`. An experiment
script is the code for an experiment that you want to repeat perhaps with
different configuration options and parameters. Louise comes with a "learning
curve" script that runs an experiment varying the number of examples (or
sampling rate) while measuring accuracy. The script generates a file with some R
data that can then be rendered into a learning curve plot by sourcing a plotting
script, also included in the scripts directory, with R.

The following are the steps to run a learning curve experiment with the data
from the `mtg_fragment.pl` example experiment file and produce a plot of the
results:

 1. Start the project:

    In a graphical environment:

    ```prolog
    ?- [load_project].
    ```

    In a text-based environment:

    ```prolog
    ?- [load_headless].
    ```

 2. Edit the project's configuration file to select the `mtg_fragment.pl`
    experiment file.

    ```prolog
    experiment_file('data/examples/mtg_fragment.pl',mtg_fragment).
    ```

 3. The learning curve script is not normally loaded with the rest of the
    project. Load it with the following query at the SWI-Prolog command line:

    ```prolog
    ?- use_module(data(scripts/learning_curve/learning_curve)).
    true.
    ```

 4. Edit the learning curve script's configuration file to select necessary
    options and output directories. The script is in the following file:

    ```
    <louise project root>/data/scripts/learning_curve/learning_curve_configuration.pl
    ```

    In that file, set the following options:

    ```prolog
    copy_plotting_scripts(scripts(learning_curve/plotting)).
    logging_directory('output/learning_curve/').
    plotting_directory('output/learning_curve/').
    r_data_file('learning_curve_data.r').
    learning_curve_time_limit(300).
    ```

    The option `copy_plotting_scripts/1` tells the experiment script whether to
    copy the R plotting script from the `scripts/learning_curve/plotting`
    directory, to an output directory, listed in the option's single argument.
    Setting this option to `false` means no plotting script is copied. You can
    specify a different directory for plotting scripts to be copied from if you
    want to write your own plotting scripts.

    The options `logging_directory/1` and `plotting_directory/1` determine the
    destination directory for output logs, R data files and plotting scripts.
    They can be separate directories if you want. Above, they are the same which
    is the most convenient.

    The option `r_data_file/1` determines the name of the R data file generated
    by the experiment script. Data files are clobbered each time the experiment
    re-runs (it's a bit of a hassle to point the plotting script to them
    otherwise) so you may want to output an experiment's R data script with a
    different name to preserve it. You'd have to manually rename the R data file
    so it can be used by the plotting script in that case.

    The option `learning_curve_time_limit/1` sets a time limit for each learning
    attempt in a learning curve experiment. If a hypothesis is not learned
    successfully until this limit has expired, the accurracy (or error etc) of
    the empty hypothesis is measured instead.

 5. Reload all configuration files to pick up the new options.

    ```prolog
    ?- make.
    ```

    Note that loading the main configuration file will turn off logging to the
    console. The next step directs you to turn it back on again so you can watch
    the experiment's progress.

 6. Enter the following queries to ensure logging to console is turned on.

    The  console output will log the steps of the experiment so that you can
    keep track of the experiment's progress (and know that it's running):

    ```prolog
    ?- debug(progress).
    true.

    ?- debug(learning_curve).
    true.
    ```

    Logging for the learning curve experiment script will have been turned off
    if you reloaded the main configuration file (because it includes the
    directive `nodebug(_)`). The two queries above turn it back on.

  7. Enter the following query to run the experiment script:

     ```prolog
     _T = ability/2, _M = acc, _K = 100, float_interval(1,9,1,_Ss), findall(S/0.0,member(S,_Ss),_Ss_Zs), learning_curve(_T,_M,_K,_Ss_Zs,_Ms,_SDs), writeln(_Ms), writeln(_SDs).
     ```

     `_M = acc` tells the experiment code to measure accuracy. You can also
     measure error, the reate of false positives, precision or recall, etc. `_K
     = 100` runs the experiment for 100 steps. `float_interval(1,9,1,_Ss)`
     generates a list of floating-point values used as sampling rates in each
     step of the experiment: `[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]`. 

     The`mtg_framework.pl` dataset has no negative examples so the list is
     zipped with a list of `0.0`'s in a `findall/3` call. The sampling library
     in `lib/sampling` that is used by the learning curve script can accept a
     sampling rate in two different formats: as a single number used as the
     sampling rate for both positive and negative examples, or as a pair, P/N,
     where P is the sampling rate for the positive examples and N the sampling
     rate for the negative examples. However, when there are no negative
     examples, passing a single number as a sampling rate will cause the
     sampling library code to raise an error. One way to avoid this error is to
     pass the sampling rate as a pair with the negative sampling rate set to 0,
     which is what the zipping with `0.0` in the query above does.

     The experiment code averages the accuracy of hypotheses learned with each
     sampling rate for all steps and also calculates the standard deviations.
     The query above will write the means (argument `_Ms` of `learning_curve/6`)
     and standard deviations (`_SDs`) in the console so you can have a quick
     look at the results. The same results are saved in a timestamped log file
     saved in the logging directory chosen in `loggign_directory/1`. Log files
     are _not_ clobbered (unlike R data files, that are) and they include a copy
     of the R data saved to the R data file. That way you always have a record
     of each experiment completed and you can reconstruct the plots if needed
     (by copying the R data from a log file to an R data script and sourcing the
     plotting script).

 8. Start R and source the plotting script in the R console to generate an
    image:

    ```R
    source("<path_to_louise>\\louise\\output\\learning_curve\\plot_learning_curve_results.r")
    ```

    You can save the plot from the R console: select the image and go to File >
    Savea As > Png... (or other file format). Then choose a location to save the
    file.
 
    The figure below is the result of sourcing the R plotting script for the
    learning curve experiment with the `mtg_fragment.pl` data.

    <img src="https://user-images.githubusercontent.com/1269718/72377073-ffd98b80-3706-11ea-924c-8c53384fe4c7.png" alt="Learning curve experiment plot." class="shrinkToFit" width="287" height="293">

Further documentation
---------------------

More information about Louise, how it works and how to use it is coming up in
the project's manual. For the time being, you can peruse the structured comments
in the project's source files. It's also possible to generate a `.pdf` file from
structured documentation, as follows:

 1. Load the project as usual.

 2. Load the `doc/latex/maket_tex.pl` module:

    ```prolog
    ?- use_module(doc/latex/make_tex).
    true.
    ```

 3. Run the following query to generate latex files from structure documentation
    in the project's source code:

    ```prolog
    ?- make_tex.
    true.
    ```

 4. Pass the main latex documentation file to pdflatex. By default the main
    documentation file is `louise/doc/latex/documentation.tex`

    ```
    cd /doc/latex/
    pdflatex -synctex=1 -interaction=nonstopmode .\documentation.tex
    pdflatex -synctex=1 -interaction=nonstopmode .\documentation.tex
    ```

    Run the pdflatex command twice to generate a ToC and bookmarks. You will
    probably see lots of errors but you will still get a .pdf file.
    _Probably_ (this hasn't been tested extensively).


Note that the structured documentation included in the Prolog source is a work
in progress. Most of the time it is mostly accurate, but there are parts of it
that are obsolete. Still- better than nothing.

Additionally to all this you may get some useful information from the current
draft of the manual stored in the file `MAN.md` in the directory `louise/doc`.
Keep in mind that `MAN.md` is a _draft_ and as such may contain incomplete or
inaccurate information. On the other hand, it will probably give a general idea
of how to use Louise and what it can do.

Citing Louise
-------------

If you decide to use Louise in your own work, you will probably want to cite
earlier work that gives more details about it. In that case, you are welcome to
use the reference to the original Top Program Construction paper, listed below
(in bib format) and which contains the most complete discussion of the Top
Program Construction algorithm:

```bib
@article{Patsantzis2021,
author = {Patsantzis, Stassa and Muggleton, Stephen},
title = {Top Program Construction and Reduction for Polynomial-Time Meta-Interpretive Learning},
journal = {Machine Learning},
year = {2021},
doi = {https://doi.org/10.1007/s10994-020-05945-w}
}
```

To clarify, you are not _required_ to cite our work to use Louise. You're also
not _required_ to wash your hands after going to the loo, it's just good form to
do so :P

Bibliography
------------

1. S.H. Muggleton, D. Lin, N. Pahlavi, and A. Tamaddoni-Nezhad. _Meta-interpretive learning: application to grammatical inference_. [Machine Learning, 94:25-49, 2014](https://link.springer.com/article/10.1007/s10994-013-5358-3)

2. S.H. Muggleton, D. Lin, and A. Tamaddoni-Nezhad. _Meta-interpretive learning of higher-order dyadic datalog: Predicate invention revisited_. [Machine Learning, 100(1):49-73, 2015](https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf)

3. S.H. Muggleton. _Inductive Logic Programming_. [New Generation Computing, 8(4):295-318, 1991](https://doi.org/10.1007/BF03037089)

4. S. Patsantzis and S. H. Muggleton. _Top Program Construction and Reduction for Polynomial-Time Meta-interpretive Learning_. [Machine Learning, 2021](https://link.springer.com/article/10.1007/s10994-020-05945-w)

4. S. Patsantzis and S. H. Muggleton. _Meta-Interpretive Learning as Metarule Specialisation_. [Machine Learning, 2021](https://arxiv.org/abs/2106.07464)

[(Muggleton et al. 2014)]: https://link.springer.com/article/10.1007/s10994-013-5358-3 "Meta-interpretive learning: application to grammatical inference"
[(Muggleton et al. 2015)]: https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf "Meta Interpretive Learning of higher-order dyadic datalog: predicate invention revisited"
[(Muggleton, 1991)]: https://doi.org/10.1007/BF03037089 "Inductive Logic Programming"
[DCG]:https://en.wikipedia.org/wiki/Definite_clause_grammar "Definite Clause Grammars"
[(Patsantzis & Muggleton 2021)]: https://link.springer.com/article/10.1007/s10994-020-05945-w "Top Program Construction and Reduction for Polynomial-Time Meta-interpretive Learning"
[(Patsantzis & Muggleton 2021b)]: https://arxiv.org/abs/2106.07464 "Meta-Interpretive Learning as Metarule Specialisation"
[Metagol]: https://github.com/metagol/metagol "Metagol"
