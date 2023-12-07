/*
Test unit for hello_world.pl.
*/

% Module with helper predicates used to train and save learned programs
% to the dynamic database, where they can be accessed by the tests in
% this unit.
:- use_module(scripts(unit_tests/test_harness)).
% Used to set configuration options.
:- use_module(src(auxiliaries)).
% Ensures debugging and other output are printed to the top-level;
% thsoe are suppressed by default when unit tests are running.
:- set_prolog_flag(plunit_output, always).

% load(normal): run tests only if not optimised
% run(make): run tests when recompiling this file.
% format(log): output results of all tests (rather than overwriting).
% show_blocked(true): output reasons for blocked tests.
:-set_test_options([load(normal)
                   ,run(make)
                   ,format(log)
                   ,show_blocked(true)
                   ]).

% Sets known good configuration options for hello_world.pl. Invoked at
% the setup of the test unit.
set_config:-
        auxiliaries:set_configuration_option(learning_predicate, [learn/1])
        ,auxiliaries:set_configuration_option(clause_limit, [0])
        ,auxiliaries:set_configuration_option(fetch_clauses, [all])
        ,auxiliaries:set_configuration_option(max_invented, [0])
        ,auxiliaries:set_configuration_option(max_error, [0,0])
        ,auxiliaries:set_configuration_option(reduction, [plotkins])
        ,auxiliaries:set_configuration_option(reduction, [5000]).

% Resets configuration to defaults.
reset_config:-
        auxiliaries:reset_defaults.

% Begins the test unit.
%
% The setup/1 goal performs necessary setup, including calling
% train_and_save/4 to learn a program from the elements of the MIL
% Problem in hello_world.pl and write its clauses to the dynamic
% database. The clauses of the learned program are added to the module
% "program" where they are then accessible by the tests in the unit.
%
% The cleanup/1 goal cleans up after the setup, including removing from
% the dynamic database all the clauses added to the "progam" module by
% the call to train_and_save/4 in the setup step.
%
:-begin_tests(hello_world, [setup((set_config
                                  ,writeln('Current setup:')
                                  ,auxiliaries:list_mil_problem(ancestor/2)
                                  ,debug(problem)
                                  ,debug(program)
                                  ,train_and_save(ancestor/2,program,long,Rs)
                                  ,table(program:ancestor/2)
                                  )
                                 )
                           ,cleanup((reset_config
                                    ,cleanup_after_training(Rs)
                                    ,nodebug(problem)
                                    ,nodebug(program)
                                    ,untable(program:ancestor/2)
                                    )
                                   )
                           ]
             ).

% Test a positive example.
test(ancestor_stathis_stassa, [ ]):-
        program:ancestor(stathis,stassa).

% Test a negative example.
test(ancestor_stassa_stathis, [ fail
                              ]):-
        program:ancestor(stassa,stathis).

% Test an example with incorrect types.
test(ancestor_bad_types, [ fail
                              ]):-
        program:ancestor(1,f(g(0))).

% Test an unknown example (i.e. one not in the background knowledge).
test(ancestor_unknown, [ fail
                              ]):-
        program:ancestor(rudolph,jengis).

% Test that a person can't be an ancestor of itself.
% This test can enter an infinite recursion unless ancestor/2 is tabled.
% That's because the program learned is left-recursive (this can be seen
% in the debugging output generated at the setup of the test unit. To
% avoid this, the program is tabled during the unit's setup.
test(ancestor_of_self, [ fail
                       ]):-
        program:ancestor(X,X).

% Test unit ends here.
:-end_tests(hello_world).
