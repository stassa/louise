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
[Experiment scripts](#experiment-scripts)  
[Further documentation](#further-documentation)  

Overview
--------

Louise is a machine learning system that learns Prolog programs.

Louise is based on a new program learning algorithm that runs in polynomial
time. Louise can learn recursive programs, including left-recursive and mutually
recursive programs and perform multi-predicate learning, predicate invention and
examples invention, among other things.

Louise is a Meta-Interpretive Learning (MIL) system. MIL is a new setting for
Inductive Logic Programming (ILP). ILP is the branch of machine learning that
studies algorithms learning logic programs from examples, background knowledge
and a language bias that determines the structure of learned programs. In MIL,
the language bias is defined by a set of second-order clause templates called
_metarules_. Examples, background knowledge and metarules must be provided by
the user, but Louise can perform predicate invention to extend its background
knowledge and metarules and so learn programs that are impossible to learn only
from its initial data.

In this manual we show simple examples where Louise is trained on small, "toy"
problems, designed to demonstrate its use. However, Louise's learning algorithm,
_Top Program Construction_, is efficient enough to learn very large programs. In
one of the example datasets included with Louise, a program of more than 25,000
clauses is learned in under 5 minutes (and, yes, that is a correct program with
perfect accuracy). This is most likely larger than any program learned by an ILP
system, or indeed by any program learning system, to date. 

In general, Louise's novelty means that it has so far primarily been applied to
artificial datasets designed to demonstrate its working principles rather than
realise its full potential. Work is underway to apply Louise on more challenging
problems, including more real-world applications. Keep in mind however that
Louise is maintained by a single PhD student. New developments should be
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

6. Louise can invent some missing examples. In the following, a single example
   of the target predicate `path/2` is given, which is insufficient to learn
   without examples invention, as in the first query. In the second query
   sufficient examples are invented to learn a full definition of the target
   predicate. The third query learns with the examples invented with
   `examples_invention/2`:

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

7. Louise comes with a number of libraries for tasks that are useful when
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
order clause templates called _metarules_. Together, examples, background
knowledge and metarules form the elements of a _MIL problem_.

Louise expects the elements of a MIL problem to be in an _experiment file_ with
a standard format.  The following is an example showing how to use Louise to
learn the "ancestor" relation from the examples, background knowledge and
metarules defined in the experiment file `louise/data/examples/tiny_kinship.pl`
using the learning predicate `learn/1`.

In summary, there are four steps to running the example: a) start Louise; b)
edit the configuration file to select `tiny_kinship.pl` as the experiment file;
c) load the experiment file into memory; d) run a learning query. These four
steps are described in detail below.

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
    `experiment_file/2`, each on a seprate line and commented-out. These are
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
it's not possible to learn more complex recursive relations, or learn programs
with mutliple clauses "calling" each other.

Louise overcomes this limitation with Dynamic learning, a learning setting where
programs are learned incrementally: the program learned in each dynamic learning
episode is added to the background knowledge for the next episode. Dynamic
learning also permits predicate invention, by inventing, and then re-using,
definitions of new predicates that are necessary for learning but are not in the
background knowledge defined by the user.

The example of learning the `a^nb^n` language in section
[Capabilities](#capabilities) is an example of learning with dynamic learning.
Below, we list the steps to run this example yourself. The steps to run this
example are similar to the steps to run the `ancestor/2` example, only this time
the learning predicate is `learn_dynamic/1`:

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
    file only include three example strings in the `a^nb^n` language, the
    pre-terminals in the language, `'A' --> [a].` and `'B' --> [b].` (where
    `'A','B'` are nonterminal symbols and `[a],[b]` are terminals) and the
    _Chain_ metarule. Given this problem definition, Louise can only learn a
    single new clause of 'S/2' (the starting symbol in the grammar), that only
    covers its first example, the string 'ab'.

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

You can see the examples invented by Louise with examples invention by calling
the predicate `examples_invention/2`:

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
