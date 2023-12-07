:-module(test_harness, [train_and_save/4
                       ,train_and_save/7
                       ,cleanup_after_training/1
                       ]).

:-use_module(src(auxiliaries)).

/** <module> Unit test harness for Extreme (Inductive Logic) Programming.

Predidcates in this module serve as helpers for unit tests with
SWI-Prolog's PLUnit.

The predicates train_and_save/4 and train_and_save/7 are used to learn a
program from a MIL problem, and write it to the dynamic database where
it can be queried by unit tests.

The predicate cleanup_after_training/1 is used to remove the clauses
added to the dynamic database by train_and_save/[4,7].

Those two predicates should be called at the directive beginnning a
unit, or a test, as setup and cleanup goals.

__ Training at the start of a test unit __

The example below shows how to invoke the two predicates as
cleanup and setup goals at the directive beginning a test unit
(together with other setup and cleanup operations):

==
:-begin_tests(hello_world, [setup((set_config
                                  ,writeln('Current setup:')
                                  ,auxiliaries:list_mil_problem(s/2)
                                  ,train_and_save(ancestor/2,program,long,Rs)
                                  ,debug(problem)
                                  ,debug(program)
                                  )
                                 )
                           ,cleanup((reset_config
                                    ,cleanup_after_training(Rs)
                                    ,nodebug(problem)
                                    ,nodebug(program)
                                    )
                                   )
                           ]
             ).
==

In the example above, note how the list of references output by the call
to train_and_save/[4,7] at setup is passed to cleanup_after_training/1
at cleanup.

In the example above, the program learned by the call to
train_and_save/4 will be accessible by all tests in the unit as a member
of the "program" module (passed as the second arguemt of
train_and_save/4).


__ Separately training for each unit test __


The example below shows how to invoke train_and_save/[4,7] and
cleanup_after_training/1 as cleanup and setup goals at the head of a
single unit test:

==
test(anbn_negative_aa, [setup((set_config
                              ,train_and_save([s([a,a,b,b],[])]
                                             ,[:-s([a,a],[])]
                                             ,[a/2,b/2]
                                             ,[chain]
                                             ,program
                                             ,short
                                             ,Rs)
                              ,table_learned
                              )
                             )
                       ,cleanup((reset_config
                                ,cleanup_after_training(Rs)
                                ,untable_learned
                                )
                               )
                       ]):-
        assertion( \+ program:s([a,a],[]) )
        ,assertion( program:s([b,b],[]) )
        ,assertion( program:s([a,b,b],[]) )
        ,assertion( program:s([a,a,b],[]) )
==

In the example above the program learned during the call to
train_and_save/7, from the elements of the MIL problem passed to it,
will be removed from the dynamic database (and from the "program"
module) at the cleanup step, during the call to
cleanup_after_training/1. That means the clause of the learned program
will only be available when running the test, but not to code in other
tests.

*/

%!      train_and_save(+Target,+Module,+Debug,-Refs) is det.
%
%       Learn a program for a Target and add its clauses to a module.
%
%       Target is a predicate indicator Symbol/Arity of a learning
%       target. Target is used to collect examples, background knowledge
%       and metarules to train on.
%
%       Module is the name of the module where the clauses of the
%       learned program will be added.
%
%       Debug is one of: [short, long], passed to debug_problem/5 to
%       determine the debugging format for the elements of the MIL
%       problem.
%
%       Refs is a list of clause references, pointing to the clauses of
%       the program learned for Target, written to the dynamic database
%       by this predicate. Use Refs to clean up afterwards.
%
%       Clauses of the learned program are saved in the dynamic
%       database, under the named Module.
%
train_and_save(T,M,D,Rs):-
        experiment_data(T,Pos,Neg,BK,MS)
        ,learning_query(Pos,Neg,BK,MS,Ps)
        ,debug_problem(D,Pos,Neg,BK,MS)
        ,debug_clauses(M,'Learned program:',Ps)
        ,assert_program(M,Ps,Rs)
        ,add_import_module(M,experiment_file,end).


%!      train_and_save(+Pos,+Neg,+BK,+MS,+Module,+Debug,-Refs) is det.
%
%       Learn a program from a MIL Problem and add it to a Module.
%
%       As train_and_save/4 but allows the elements of a MIL problem to
%       be passed in as arguments, rather than loaded from an
%       experiment file.
%
%       Pos, Neg, BK, MS are the positive and negative examples,
%       background knowledge and metaruels for the learning problem,
%       respectively.
%
%       Module is the name of the module where the clauses of the
%       learned program will be added.
%
%       Debug is one of: [short, long], passed to debug_problem/5 to
%       determine the debugging format for the elements of the MIL
%       problem:
%
%       Refs is a list of clause references, pointing to the clauses of
%       the program learned for Target, written to the dynamic database
%       by this predicate. Use Refs to clean up afterwards.
%
%       Clauses of the learned program are saved in the dynamic
%       database under the named Module.
%
train_and_save(Pos,Neg,BK,MS,M,D,Rs):-
        learning_query(Pos,Neg,BK,MS,Ps)
        ,debug_problem(D,Pos,Neg,BK,MS)
        ,debug_clauses(M,'Learned program:',Ps)
        ,assert_program(M,Ps,Rs)
        ,add_import_module(M,experiment_file,end).


%!      debug_problem(+Format,+Pos,+Neg,+BK,+MS) is det.
%
%       Log the elements of a learning problem.
%
%       Format is one of: [short, long], and determines the format in
%       which the elements of the MIL problem are to be printed.
%
%       If Format is "short", each set of elements (examples, BK,
%       metarules) is printed on one line, with e.g. all the positive
%       examples on the same line. Use this format if you have few
%       examples and want to preserve space.
%
%       If Format is "long" each set is printed with newlines in
%       between, e.g. each positive example is printed on its own line.
%       The effect is similar to list_mil_problem/1. Use this format
%       when you have more examples and want to read them more easily.
%
%       Pos, Neg, BK and MS are the elements of the MIL problem to be
%       printed to the current debug stream.
%
%       The debug subject for debug_problem/5 is 'problem'. To debug the
%       elements of a MIL problem the following atom should be added to
%       the unit tests file as a directive, or in the setup part of a
%       test or a unit, or called at the top-level as a goal:
%       ==
%       debug(problem)
%       ==
%
%       For example, to turn on debugging for 'program' during the
%       setup of a test unit, you can do this:
%       ==
%
%       :-begin_tests(anbn, [setup( debug(problem) )
%                           ,cleanup( nodebug(problem) )
%                           ]
%                    ).
%       ==
%
%       The "nodebug(problem)" goal in the cleanup step will then turn
%       debuggin off for 'problem' when the test unit has finished
%       running.
%
debug_problem(short,Pos,Neg,BK,MS):-
        debug(problem,'Positive examples: ~w',[Pos])
        ,debug(problem,'Negative examples: ~w',[Neg])
        ,debug(problem,'Background knowledge: ~w',[BK])
        ,debug(problem,'Metarules: ~w',[MS]).
debug_problem(long,Pos,Neg,BK,MS):-
        debug_clauses(problem,'Positive examples:',Pos)
        ,debug(problem,'',[])
        ,debug_clauses(problem,'Negative examples:',Neg)
        ,debug(problem,'',[])
        ,debug_clauses(problem,'Background knowledge:',BK)
        ,debug(problem,'',[])
        ,debug_clauses(problem,'Metarules:',MS)
        ,debug(problem,'',[]).


%!      cleanup_after_training(+Refs) is det.
%
%       Cleans the database of any remaining clauses.
%
%       Simple wrapper around erase_program_clauses/1 to remove clauses
%       added to the dynamic database by train_and_save/[4,7].
%
%       Use this in the test unit directive, as a cleanup goal, passing
%       it the list of references from train_and_save/[4,7], e.g.:
%       ==
%       :-begin_tests(anbn,[setup(train_and_save(s/2,program,long,Rs))
%                                  ,cleanup(cleanup_after_training(Rs))
%                                  ]
%                    ).
%
%       ==
%
cleanup_after_training(Rs):-
        erase_program_clauses(Rs).


