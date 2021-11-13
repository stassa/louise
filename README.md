Louise - polynomial-time Program Learning
=========================================

Getting help with Louise
------------------------

Louise's author can be reached by email at ep2216@ic.ac.uk. Please use this
email to ask for any help you might need with using Louise.

Louise is brand new and, should you choose to use it, you will most probably
encounter errors and bugs. The author has no way to know of what bugs and errors
you encounter unless you report them. Please use the author's email to contact
the author regarding bugs and errors. Alternatively, you are welcome to open a
github Issue or send a pull request. 

Table of contents
-----------------

[Overview](#overview)  
[Capabilities](#capabilities)  
[Learning logic programs with Louise](#learning-logic-programs-with-louise)  
[Running examples in Swi-Prolog](#running-examples-in-swi-prolog)  
[Learning the "ancestor" relation](#learning-the-ancestor-relation)  
[Dynamic learning and predicate invention](#dynamic-learning-and-predicate-invention)  
[Examples invention](#examples-invention)  
[Learning with metarules in MIL](#learning-with-metarules-in-mil)  
[Pretty-printing metarules in Louise](#pretty-printing-metarules-in-louise)  
[Learning metarules with TOIIL](#learning-metarules-with-toiil)  
[Experiment scripts](#experiment-scripts)  
[Further documentation](#further-documentation)  

Overview
--------

Louise [(Patsantzis & Muggleton 2021)] is a machine learning system that learns
Prolog programs.

Louise is based on a new program learning algorithm, called _Top Program
Construction_, that runs in polynomial time. Louise can learn recursive
programs, including left-recursive and mutually recursive programs and perform
multi-predicate learning, predicate invention and examples invention, among
other things. Louise includes the program TOIL that can learn metarules for MIL
systems, including Louise itself.

Louise is a Meta-Interpretive Learning (MIL) system. MIL [(Muggleton et al.
2014)], [(Muggleton et al. 2015)], is a new setting for Inductive Logic
Programming (ILP) [(Muggleton, 1991)]. ILP is the branch of machine learning
that studies algorithms learning logic programs from examples, background
knowledge and a language bias that determines the structure of learned programs.
In MIL, the language bias is defined by a set of second order logic clauses
called _metarules_. Examples, background knowledge and metarules must be
provided by the user, but Louise can perform predicate invention to extend its
background knowledge and metarules and so learn programs that are impossible to
learn only from its initial data. Louise can also learn new metarules from
examples of a learning target. Finally, Louise can perform examples invention to
extend its set of given examples.

In this manual we show simple examples where Louise is trained on small, "toy"
problems, designed to demonstrate its use. However, Louise's learning algorithm,
_Top Program Construction_, is efficient enough to learn very large programs. In
one of the example datasets included with Louise, a program of more than 2,500
clauses is learned in under 5 minutes.

In general, Louise's novelty means that it has so far primarily been applied to
artificial datasets designed to demonstrate its working principles rather than
realise its full potential. Work is underway to apply Louise on more challenging
problems, including more real-world applications. Keep in mind that Louise is
maintained by a single PhD student. New developments should be expected to come
at a leisurely pace.

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

2. Louise can simultaneously learn multiple dependent programs, including
   mutuallly recursive programs. This is called multi-predicate learning:


   ```prolog
   ?- learn([even/1,odd/1]).
   even(0).
   even(A):-predecessor(A,B),odd(B).
   odd(A):-predecessor(A,B),even(B).
   true.
   ```
   
   See `data/examples/multi_pred.pl` for the `odd/1` and `even/1`
   multi-predicate learning example. 

3. Louise can discover relevant background knowledge. In the `odd/1` and
   `even/1` example above, each predicate is only explicitly given
   `predecessor/2` as a background predicate. The following are the background
   knowledge declarations for `even/1` and `odd/1` in
   `data/examples/multi_pred.pl`:

   ```prolog
   background_knowledge(even/1, [predecessor/2]).
   background_knowledge(odd/1, [predecessor/2]).
   
   ```

   Louise figures out that `odd/1` is necessary to learn `even/1` and vice-versa
   on its own.

4. Louise can perform _predicate invention_ to incrase its background knowledge
   with new predicates that are necessary for learning. In the following example
   the predicate `'$'1/2` is an invented predicate:

   ```prolog
   ?- learn_dynamic('S'/2).
   '$1'(A,B):-'S'(A,C),'B'(C,B).
   'S'(A,B):-'A'(A,C),'$1'(C,B).
   'S'(A,B):-'A'(A,C),'B'(C,B).
   true.
   ```

   With predicate invention Louise can shift its inductive bias to learn
   programs that are not possible to learn from its initial set of background
   knowledge and metarules.

   See `data/examples/anbn.pl` for the `'S'/2` example.

   See the section [Dynamic learning and predicate
   invention](#dynamic-learning-and-predicate-invention) for more information on
   predicate invention in Louise. 

5. Louise can unfold programs to eliminate invented predicates. This is a
   version of the program in the previous example with the invented predicate
   `'$1'/2` eliminated by unfolding:
   
   ```prolog
   ?- learn_dynamic('S'/2).
   'S'(A,B):-'A'(A,C),'B'(C,B).
   'S'(A,B):-'A'(A,C),'S'(C,D),'B'(D,B).
   true.
   ```

   Eliminating invented predicates can sometimes improve comprehensibility of
   the learned program.

6. Louise can fold over-specialised programs to introduce recursion. In the
   following example, an over-specialised program is learned that finds the last
   element of a list of length up to 3:

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

7. Louise can invent new examples. In the following query a number of examples
   of `path/2` are invented. The background knowledge for this MIL problem
   consists of 6 `edge/2` ground facts that determine the structure of a graph
   and a few facts of `not_edge/2` that represent nodes not connected by edges.
   `path(a,f)` is the single given example. The target theory for this problem
   is a recursive definition of `path/2` that includes a "base case" for which
   no example is given. Louise can invent examples of the base-case and so learn
   a correct hypothesis that represents the full path from node 'a' to node 'f',
   without crossing any non-edges.

   ```prolog
   ?- learn(path/2).
   path(a,f).
   true.
   
   ?- examples_invention(path/2,_Es), print_clauses(_Es).
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
   
   ?- learn_with_examples_invention(path/2).
   path(A,B):-edge(A,B).
   path(A,B):-edge(A,C),path(C,B).
   true.
   ```
   
   See `data/examples/example_invention.pl` for the `path/2` example.
   
   See the section [Examples invention](#examples-invention) for more
   information on examples invention in Louise.

8. Louise can learn new metarules from examples of a target predicate. In the
   following example, Louise learns a new metarule from examples of the
   predicate `'S'/2` (as in item 4, above):

   ```Prolog
   ?- learn_metarules('S'/2).
   (Meta-dyadic-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
   true.
   ```

   The new metarule, Meta-dyadic-1 corresponds to the common _Chain_ metarule
   that is used in item 4 to learn a grammar of the a^nb^n language.

   Louise can learn new metarules by specialising the most-general metarule in
   each language class. In the example above, the language class is H(2,2), the
   language of metarules having exactly three literals of arity 2. The most
   general metarule in H(2,2) is Meta-dyadic:

   ```Prolog
   ?- print_quantified_metarules(meta_dyadic).
   (Meta-dyadic) ∃.P,Q,R ∀.x,y,z,u,v,w: P(x,y)← Q(z,u),R(v,w)
   true.   
   ```

   Louise can also learn new metarules given only an upper and lower bound on
   their numbers of literals. In the example of learning _Chain_ above, instead
   of specifying Meta-dyadic, we can instead give an upper and lower bound of 3,
   with a declaration of `higher_order(3,3)`.

9. Louise comes with a number of libraries for tasks that are useful when
   learning programs with MIL, e.g. metarule generation, program reduction,
   lifting of ground predicates, etc. These will be discussed in detail in the
   upcoming Louise manual.

Learning logic programs with Louise
-----------------------------------

In this section we give a few examples of learning simple logic programs with
Louise. The examples are chosen to demonstrate Louise's usage, not to convince
of Louise's strengths as a learner. All the examples in this section are in the
directory `louise/data/examples`. After going through the examples here, feel
free to load and run the examples in that directory to better familiarise
yourself with Louise's functionality.

### Running examples in Swi-Prolog

Swi-Prolog is a popular, free and open-source Prolog interpreter and development
environment. Louise was written for Swi-Prolog. To run the examples in this
section you will need to install Swi-Prolog. You can download Swi-Prolog from
the following URL:

[https://www.swi-prolog.org/Download.html](https://www.swi-prolog.org/Download.html)

Louise runs with any of the latest stable or development releases listed on that
page. Choose the one you prefer to download.

It is recommended that you run the examples using the Swi-Prolog graphical IDE,
rather than in a system console. On operating systems with a graphical
environment the Swi-Prolog IDE should start automaticaly when you open a Prolog
file.

In this section, we assume you have cloned this project into a directory called
`louise`. Paths to various files will be given relative to the `louise` project
root directory and queries at the Swi-Prolog top-level will assume your current
working directory is `louise`.

#### Learning the "ancestor" relation

Louise learns Prolog programs from examples, background knowledge and second
order logic clauses called _metarules_. Together, examples, background knowledge
and metarules form the elements of a _MIL problem_.

Louise expects the elements of a MIL problem to be in an _experiment file_ with
a standard format.  The following is an example showing how to use Louise to
learn the "ancestor" relation from the examples, background knowledge and
metarules defined in the experiment file `louise/data/examples/tiny_kinship.pl`
using the learning predicate `learn/1`.

In summary, there are four steps to running an example: a) start Louise; b) edit
the configuration file to select an experiment file; c) load the experiment file
into memory; d) run a learning query. These four steps are described in detail
below.

 1. Consult the project's load file into Swi-Prolog to load necessary files into
    memory:

    In a graphical environment:
    
    ```prolog
    ?- [load_project].
    ```

    In a text-based environment:

    ```prolog
    ?- [load_headless].
    ```

    The first query will also start the Swi-Prolog IDE and documentation
    browser, which you probably don't want if you're in a text-based
    environment.

 2. Edit the project's configuration file to select an experiment file.

    Edit `louise/configuration.pl` in the Swi-Prolog editor (or your favourite
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

    The easiest way to reload the configuration file is to use Swi-Prolog's `make/0`
    predicate to recompile the project (don't worry- this takes less than a
    second). To recompile the project with `make/0` enter the following query in
    the Swi-Prolog console:

    ```prolog
    ?- make.
    ```

    Note again: this is the Swi-Prolog predicate `make/0`. It's not the _make_
    build automation tool!

 4. Perform a learning attempt using the examples, background knowledge and
    metarules defined in `tiny_kinship.pl` for `ancestor/2`.

    Execute the following query in the Swi-Prolog console; you should see the
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

### Dynamic learning and predicate invention

The predicate `learn/1` implements Louise's default learning setting that learns
a program one-clause-at-a-time without memory of what was learned before. This
is limited in that clauses learned in an earlier step cannot be re-used and so
it's not possible to learn programs with mutliple clauses "calling" each other,
particularly recursive clauses that must resolve with each other.

Louise overcomes this limitation with Dynamic learning, a learning setting where
programs are learned incrementally: the program learned in each dynamic learning
episode is added to the background knowledge for the next episode. Dynamic
learning also permits predicate invention, by inventing, and then re-using,
definitions of new predicates that are necessary for learning but are not in the
background knowledge defined by the user.

The example of learning the `a^nb^n` language in section
[Capabilities](#capabilities) is an example of learning with dynamic learning.
The result is a grammar in Prolog's Definite Clause Grammars formalism (see:
[DCG]). Below, we list the steps to run this example yourself. The steps to run
this example are similar to the steps to run the `ancestor/2` example, only this
time the learning predicate is `learn_dynamic/1`:

 1. Start the project:

    In a graphical environment:
    
    ```prolog
    ?- [load_project].
    ```

    In a text-based environment:

    ```prolog
    ?- [load_headless].
    ```

 2. Edit the project's configuration file to select the `anbn.pl` experiment
    file.

    ```prolog
    experiment_file('data/examples/anbn.pl',anbn).
    ```

 3. Reload the configuration file to pick up the new experiment file option.

    ```prolog
    ?- make.
    ```

 4. Perform an initial learning attempt _without_ Dynamic learning:
    
    ```prolog
    ?- learn('S'/2).
    'S'([a,a,a,b,b,b],[]).
    'S'([a,a,b,b],[]).
    'S'(A,B):-'A'(A,C),'B'(C,B).
    true.
    ```

    The elements for the learning problem defined in the `anbn.pl` experiment
    file only include three example strings in the `a^nb^n` language, and the
    pre-terminals in the language, `'A' --> [a].` and `'B' --> [b].` as
    background knowledge (where `'A','B'` are nonterminal symbols and `[a],[b]`
    are terminals) and the _Chain_ metarule. Given this problem definition,
    Louise can only learn a single new clause of 'S/2' (the starting symbol in
    the grammar), that only covers its first example, the string 'ab'.

    The real limit in what can be learned in this case is the single metarule,
    _Chain_. The target theory for a DCG of the `a^nb^n` language includes a
    clause with three literals:

    ```prolog
   'S'(A,B):-'A'(A,C),'B'(C,B).
   'S'(A,B):-'A'(A,C),'S'(C,D),'B'(D,B).
    ```

    However, the _Chain_ metarule only allows clauses with three literals to be
    learned. To construct a complete definition of the target grammar, it is
    necessary to add a new symbol to the background knowledge by inventing a new
    predicate, to act as `glue' between clauses of _Chain_. We show how to do
    this in the next step of the experiment.

 5. Perform a second learning attemt with dynamic lerning:

    ```prolog
    ?- learn_dynamic('S'/2).
    '$1'(A,B):-'S'(A,C),'B'(C,B).
    'S'(A,B):-'A'(A,C),'$1'(C,B).
    'S'(A,B):-'A'(A,C),'B'(C,B).
    true.
    ```

    With dynamic learning, Louise can re-use the first clause it learns (the one
    covering 'ab', listed above) to invented a new nonterminal, `$1/2`, that it
    can then use to construct the full grammar.

 6. Note that the grammar learned in the previous step is equivalent to the
    target theory listed in step 4. This can be seen by unfolding the learned
    grammar to remove the invented predicate.

    First, set the configuration option `unfold_invented/1` to "true":

    ```prolog
    unfold_invented(true).
    ```

    Now, repeat the call to dynamic_learning/1 in the previous step:

    ```prolog
    ?- learn_dynamic('S'/2).
    'S'(A,B):-'A'(A,C),'B'(C,B).
    'S'(A,B):-'A'(A,C),'S'(C,D),'B'(D,B).
    true.
    ```

    Unfolding is automatically applied to the learned hypothesis eliminating the
    invented predicate.


### Examples invention

Louise can perform _examples invention_ which is just what it sounds like.
Examples invention works best when you have relevant background knowledge and
metarules but insufficient positive examples to learn a correct hypothesis. It
works even better when you have at least some negative examples. If your
background knowledge is irrelevant or you don't have "enough" negative examples
(where "enough" depends on the MIL problem) then examples invention can
over-generalise and produce spurious results.

The learning predicate for examples invention in Louise is
`learn_with_examples_invention/2`. Below is an example showing how to use it,
again following the structure of the examples shown previously.

 1. Start the project:

    In a graphical environment:
    
    ```prolog
    ?- [load_project].
    ```

    In a text-based environment:

    ```prolog
    ?- [load_headless].
    ```

 2. Edit the project's configuration file to select the `examples_invention.pl`
    experiment file.

    ```prolog
    experiment_file('data/examples/examples_invention.pl',path).
    ```

 3. Reload the configuration file to pick up the new experiment file option.

    ```prolog
    ?- make.
    ```

 4. Try a first learning attempt without examples invention:
    
    ```prolog
    ?- learn(path/2).
    path(a,f).
    true.
    ```

    The single positive example in `examples_invention.pl` are insufficient for
    Louise to learn a general theory of `path/2`. Louise simply returns the
    single positive example, un-generalised.

 5. Perform a second learning attempt with examples invention:

    ```prolog
    ?- learn_with_examples_invention(path/2).
    path(A,B):-edge(A,B).
    path(A,B):-edge(A,C),path(C,B).
    true.
    ```

    This time, Louise first tries to invent new examples of `path/2` by training
    in a semi-supervised manner, and then uses these new examples to learn a
    complete theory of `path/2`.

  6. You can list the examples invented by Louise with a call to the predicate
     `examples_invention/2`:

     ```prolog
     ?- examples_invention(path/2,_Ps), print_clauses(_Ps).
     m(path,a,b).
     m(path,a,c).
     m(path,a,f).
     m(path,b,c).
     m(path,b,d).
     m(path,c,d).
     m(path,c,e).
     m(path,d,e).
     m(path,d,f).
     m(path,e,f).
     true.
     ```

Learning with metarules in MIL
------------------------------

Metarules are used in MIL as inductive bias, to determine the structure of
clauses in learned hypotheses. Metarules are second-order logic clauses, that
resemble first-order Horn clauses, but have second-order variables existentially
quantified over the set of predicate symbols.

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

Pretty-printing metarules in Louise
-----------------------------------

In the previous section we have used Louise's pretty-printer for metarules,
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
to a file to keep a log of a learning attempt.

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

Learning metarules with TOIIL
-----------------------------

Metarules are usually defined by hand, and are tailored to a possible solution
of a learning problem. Louise is capable of learning its own metarules.

Metarule learning in Louise is implemented by a sub-system called TOIL (an
acronym for _Third-Order Inductive Learner_). TOIL learns metarules from
examples, background knowledge and _generalised_ metarules. Unlike user-defined
metarules, the generalised metarules used by TOIL do not have to be closely
tailored to a problem.

TOIL recognises three taxa of metarules. Listed by degrees of generality, these
are: sort, matrix and punch metarules. Their names are derived from typeset
printing where successive levels of molds for letters to be typed are carved in
metals of decreasing hardness.

### Sort metarules

Sort metarules are the kind of metarules normally defined by a user and found
throughout the MIL literature. They are also widely used in ILP more generally
as well as in data mining and program sythesis.

Sort metarules are specialised, in the sense that their universally quantified
first-order variables are _shared_ between literals. For example, in the _Chain_
metarule, below, the variable `x` is shared between its head literal and its
first body literal, the variable `y` is shared between its head literal and last
body literal, and the variable `z` is shared between its two body literals:

```Prolog
P(x,y)← Q(x,z),R(z,y)
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

### Matrix metarules

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

### Punch metarules.

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

### Metarule specialisation in TOIL

TOIL learns metarules by exploiting the generality relation between the three
taxa of metarules described in the previous sections. TOIL takes as in put the
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

### Controlling over-generation of metarules in TOIL

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

### Specialisation of Punch metarules

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

#### Limitations of TOIL

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

 3. Edit the learning curve script's configuration file to select necessary
    options and output directories:

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

 4. Reload all configuration files to pick up the new options.

    ```prolog
    ?- make.
    ```

    Note that loading the main configuration file will turn off logging to the
    console. The next step directs you to turn it back on again so you can watch
    the experiment's progress.

 5. Enter the following queries to ensure logging to console is turned on.

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

  6. Enter the following query to run the experiment script:

     ```prolog
     _T = ability/2, _M = acc, _K = 100, float_interval(1,9,1,_Ss), learning_curve(_T,_M,_K,_Ss,_Ms,_SDs), writeln(_Ms), writeln(_SDs).
     ```

     `_M = acc` tells the experiment code to measure accuracy. You can also
     measure error, the reate of false positives, precision or recall, etc. `_K
     = 100` runs the experiment for 100 steps. `float_interval(1,9,1,_Ss)`
     generates a list of floating-point values used as sampling rates in each
     step of the experiment: `[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]`.

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

 7. Source the plotting script in the console R to generate an image:

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

If you wish to use Louise, please cite our work with the reference below:

```bib
@article{Patsantzis2021,
author = {Patsantzis, Stassa and Muggleton, Stephen},
title = {Top Program Construction for Polynomial Meta-Interpretive Learning},
journal = {Machine Learning},
year = {2021},
doi = {https://doi.org/10.1007/s10994-020-05945-w}
}
```

Bibliography
------------

1. S.H. Muggleton, D. Lin, N. Pahlavi, and A. Tamaddoni-Nezhad. _Meta-interpretive learning: application to grammatical inference_. [Machine Learning, 94:25-49, 2014](https://link.springer.com/article/10.1007/s10994-013-5358-3)

2. S.H. Muggleton, D. Lin, and A. Tamaddoni-Nezhad. _Meta-interpretive learning of higher-order dyadic datalog: Predicate invention revisited_. [Machine Learning, 100(1):49-73, 2015](https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf)

3. S.H. Muggleton. _Inductive Logic Programming_. [New Generation Computing, 8(4):295-318, 1991](https://doi.org/10.1007/BF03037089)

4. S. Patsantzis and S. H. Muggleton. _Top Program Construction for Polynomial-Time Meta-interpretive Learning_. [Machine Learning, 2021](https://link.springer.com/article/10.1007/s10994-020-05945-w)

[(Muggleton et al. 2014)]: https://link.springer.com/article/10.1007/s10994-013-5358-3 "Meta-interpretive learning: application to grammatical inference"
[(Muggleton et al. 2015)]: https://link.springer.com/content/pdf/10.1007%2Fs10994-014-5471-y.pdf "Meta Interpretive Learning of higher-order dyadic datalog: predicate invention revisited"
[(Muggleton, 1991)]: https://doi.org/10.1007/BF03037089 "Inductive Logic Programming"
[DCG]:https://en.wikipedia.org/wiki/Definite_clause_grammar "Definite Clause Grammars"
[(Patsantzis & Muggleton 2021)]: https://link.springer.com/article/10.1007/s10994-020-05945-w "Top Program Construction for Polynomial-Time Meta-interpretive Learning"
