/*
Test unit for anbn.pl.
*/

:- use_module(scripts(unit_tests/test_harness)).
:-use_module(src(auxiliaries)).

% Allows printing output to top-level.
% Development versions don't, by default:
% https://swi-prolog.discourse.group/t/how-to-print-to-screen-in-unit-tests/6347/5
:- set_prolog_flag(plunit_output, always).

:-set_test_options([load(normal)
                   ,run(make)
                   ,format(log)
                   ,show_blocked(true)
                   ]).

set_config:-
        auxiliaries:set_configuration_option(learning_predicate, [learn/1])
        ,auxiliaries:set_configuration_option(clause_limit, [3])
        ,auxiliaries:set_configuration_option(max_invented, [1])
        ,auxiliaries:set_configuration_option(fetch_clauses, [all])
        ,auxiliaries:set_configuration_option(table_meta_interpreter, [true])
        ,auxiliaries:set_configuration_option(untable_meta_interpreter, [true])
        % Goes faster than default '$'. Why?
        ,auxiliaries:set_configuration_option(invented_symbol_prefix,['inv_']).
reset_config:-
        reset_defaults.

% Table target and invented predicates to avoid infinite
% left-recursion when testing.
table_learned:-
        table(program:s/2)
        ,table(program:inv_1/2).
        %,table(program:'$1'/2).
untable_learned:-
        untable(program:s/2)
        ,untable(program:inv_1/2).
        %,untable(program:'$1'/2).

example(N,M,AsBs):-
        findall(a,between(1,N,_),As)
        ,findall(b,between(1,M,_),Bs)
        ,append(As,Bs,AsBs).


:-begin_tests(anbn, [setup((set_config
                           ,writeln('Current setup:')
                           ,auxiliaries:list_mil_problem(s/2)
                           ,debug(program)
                           ,debug(problem)
                           )
                          )
                    ,cleanup((reset_config
                             ,nodebug(program)
                             ,nodebug(problem)
                             )
                            )
                    ]
             ).

% Test generalisation up to n >> training n.
test(anbn_n2_n3_n9_n100, [setup((set_config
                       ,train_and_save([s([a,a,b,b],[])]
                                      ,[]
                                      ,[a/2,b/2]
                                      ,[chain]
                                      ,program
                                      ,short
                                      ,Rs)
                       ,table_learned
                       )
                      )
                ,cleanup((reset_config
                         ,untable_learned
                         ,cleanup_after_training(Rs)
                         )
                        )
                ]):-
        assertion( experiment_file:learning_predicate(learn/1) )
        ,assertion( program:s([a,a,b,b],[]) )
        ,example(3,3,A3B3)
        ,assertion( program:s(A3B3,[]) )
        ,example(9,9,A9B9)
        ,assertion( program:s(A9B9,[]) )
        ,example(100,100,ACBC)
        ,assertion( program:s(ACBC,[]) ).

% Test over-generalisation with negative example "aa".
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
        assertion( experiment_file:learning_predicate(learn/1) )
        ,assertion( \+ program:s([a,a],[]) )
        ,assertion( program:s([b,b],[]) )
        ,assertion( program:s([a,b,b],[]) )
        ,assertion( program:s([a,a,b],[]) ).

% Test overgeneralisation with negative example "bb".
test(anbn_negative_bb, [setup((set_config
                              ,train_and_save([s([a,a,b,b],[])]
                                             ,[:-s([b,b],[])]
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
        assertion( experiment_file:learning_predicate(learn/1) )
        ,assertion( program:s([a,a],[]) )
        ,assertion( \+ program:s([b,b],[]) )
        ,assertion( program:s([a,b,b],[]) )
        ,assertion( program:s([a,a,b],[]) ).

% Test overgeneralisation with negative examples "aa" and "bb".
test(anbn_negative_aa_bb, [setup((set_config
                                 ,train_and_save([s([a,a,b,b],[])]
                                                ,[:-s([a,a],[])
                                                 ,:-s([b,b],[])
                                                 ]
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
        assertion( experiment_file:learning_predicate(learn/1) )
        ,assertion( \+ program:s([a,a],[]) )
        ,assertion( \+ program:s([b,b],[]) )
        ,assertion( program:s([a,b,b],[]) )
        ,assertion( program:s([a,a,b],[]) ).

% Test overgeneralisation with negative examples "aa", "bb" and "abb".
test(anbn_negative_aa_bb_abb, [setup((set_config
                              ,train_and_save([s([a,a,b,b],[])]
                                             ,[:-s([a,a],[])
                                              ,:-s([b,b],[])
                                              ,:-s([a,b,b],[])
                                              ]
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
        assertion( experiment_file:learning_predicate(learn/1) )
        ,assertion( \+ program:s([a,a],[]) )
        ,assertion( \+ program:s([b,b],[]) )
        ,assertion( program:s([a,b,b],[]) )
        ,assertion( program:s([a,a,b],[]) ).

% Test overgeneralisation with negative examples "aa", "bb", "abb" and
% "aab".
test(anbn_negative_aa_bb_abb_aab, [setup((set_config
                                         ,train_and_save([s([a,a,b,b],[])]
                                                        ,[:-s([a,a],[])
                                                         ,:-s([b,b],[])
                                                         ,:-s([a,b,b],[])
                                                         ,:-s([a,a,b],[])
                                                         ]
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
        assertion( experiment_file:learning_predicate(learn/1) )
        ,assertion( \+ program:s([a,a],[]) )
        ,assertion( \+ program:s([b,b],[]) )
        ,assertion( program:s([a,b,b],[]) )
        ,assertion( program:s([a,a,b],[]) ).

% Test what happens when training with a more-specific example.
test(anbn_a7b7_specific_example, [setup((set_config
                                        ,example(7,7,ACBC)
                                        ,train_and_save([s(ACBC,[])]
                                                       ,[:-s([a,a],[])
                                                        ,:-s([b,b],[])
                                                        ,:-s([a,b,b],[])
                                                        ]
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
        assertion( experiment_file:learning_predicate(learn/1) )
        ,assertion( \+ program:s([a,a],[]) )
        ,assertion( \+ program:s([b,b],[]) )
        ,assertion( \+ program:s([a,b,b],[]) )
        ,assertion( \+ program:s([a,a,b],[]) ).

:- end_tests(anbn).
