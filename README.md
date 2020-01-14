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

What Louise is
--------------

Louise is a machine learning system that learns Prolog programs.

Louise is based on a new Meta-Interpretive Learning algorithm that runs in
polynomial time. Louise can learn recursive programs and perform predicate
invention and examples invention, among other things.

Meta-Interpretive Learning (MIL) is a new paradigm for Inductive Logic
Programming (ILP). ILP is the branch of machine learning that studies algorithms
learning logic programs from examples and background knowledge. In ILP, the
examples and background knowledge are also defined as logic programs. In MIL, in
addition to examples and background knowledge, a set of clause templates called
_metarules_ are also used. Examples, background knowledge and metarules are
discussed in depth in the upcoming Louise manual. 

What Louise does
----------------

Louise learns Prolog programs from examples, background knowledge and second
order clause templates called _metarules_. Together, examples, background
knowledge and metarules form the elements of a _MIL problem_. The following are
the elements of an example MIL problem for Louise:

```prolog
% Positive examples of strings in the context-free a^nb^n language
'S'([a,b],[])
'S'([a,a,b,b],[])
'S'([a,a,a,b,b,b],[])

% A notable absence of negative examples.

% Background knowledge: the two (pre-) terminals in the a^nb^n language given as
% Definite Clause Grammar productions.
'A' --> [a].
'B' --> [b].

% The metarule called "Chain"
chain metarule 'P(x,y):- Q(x,z), R(z,y)'.
```

Given the examples, background knowledge and metarules above, Louise will learn
a grammar for the a^nb^n language, as follows:

```prolog
?- learn_dynamic('S'/2).
'$1'(A,B):-'S'(A,C),'B'(C,B).
'S'(A,B):-'A'(A,C),'$1'(C,B).
'S'(A,B):-'A'(A,C),'B'(C,B).
true.
```

In the learned grammar above, the predicate `'$1'` is invented: it was not given
to Louise at the start of learning. This is called _predicate invention_ and is
a major feature of MIL-learners like Louise.

Learning logic prorgams with Louise
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

Louise expects its data to be in an _experiment file_ with a standard format.
The following is an example showing how to use Louise to learn the "ancestor"
relation from the examples, background knowledge and metarules defined in the
experiment file `louise/data/examples/tiny_kinship.pl` using the learning
predicate `learn/1`.

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

The example of learning the `a^nb^n` language at the start of this README is an
example of learning with dynamic learning. Below, we list the steps to run this
example yourself. The steps to run this example are similar to the steps to
run the `ancestor/2` example, only this time the learning predicate is
`learn_dynamic/1`:

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

Formalised experiments
----------------------

Louise includes experiment scripts for formalised experiments in the directory
`data/scripts`. Currently, the only experiment script is a "learning curve"
script that runs an experiment varying the number of examples (or sampling rate)
while measuring accuracy. The script generates a file with some R data that can
then be rendered into a learning curve plot by sourcing a plotting script, also
included in the scripts directory, with R.

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

Coming soon
-----------

More instructions to use Louise are coming up in the project's manual. For the
time being, you may get some useful information from the current draft of the
manual stored in the file `MAN.md` in the directory `louise/doc`. Keep in mind
that `MAN.md` is a _draft_ and as such may contain incomplete or inaccurate
information. On the other hand, it will probably give a general idea of how to
use Louise and what it can do.
