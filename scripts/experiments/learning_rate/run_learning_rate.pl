:-module(run_learning_rate, [setup/0
                            ,run_all/0
                            ,run_kin/0
                            ,run_mtg_fragment/0
                            ,run_robots/0
                            ]).

% Must be run in the top directory of thelma or louise.
% Running in thelma/ will run the experiments with thelma.
% Running in louise/ will run the experiment with Aleph.
% Nah, just kidding. Louise.
:-[load_headless].


:-use_module(learning_rate).
:-use_module(learning_rate_configuration).
:-dynamic configuration:experiment_file/2.

% Make debug messages readable in default-coloured powershell.
user:message_property(debug(learning_rate), color( [ fg(cyan) ]) ).
user:message_property(debug(progress), color( [ fg(yellow) ]) ).


float_interval(I,K,J,Ss):-
        interval(I,K,J,Is)
        ,findall(S,(member(I_,Is)
                   ,S is I_ /10)
                ,Ss).

set_local_configuration_option(N, V):-
	atomic(V)
	,!
	,set_local_configuration_option(N,[V]).
set_local_configuration_option(N, Vs):-
	length(Vs, A)
	,functor(T,N,A)
	,T_ =.. [N|Vs]
	,retractall(learning_rate_configuration:T)
	,assert(learning_rate_configuration:T_).



setup:-
        set_local_configuration_option(copy_plotting_scripts
                                      ,[scripts(plotting/learning_rate)])
        ,robots:write_dataset.



run_all:-
        learning_rate_configuration:copy_plotting_scripts(Val)
        ,set_local_configuration_option(copy_plotting_scripts
                                      ,[scripts(plotting/learning_rate)])
        ,run_kin
        ,run_mtg_fragment
        ,run_robots
        ,set_local_configuration_option(copy_plotting_scripts,[Val])
        ,print_config(print,user_output,all).



run_kin:-
        experiment_file(P,Mod)
        ,learning_rate_configuration:logging_directory(D_L)
        ,learning_rate_configuration:plotting_directory(D_P)
        ,learning_rate_configuration:copy_plotting_scripts(Val)
        ,set_configuration_option(experiment_file
                                 ,['data/thelma_louise/kinship/kin/kin.pl',kin])
        ,set_local_configuration_option(logging_directory
                                       ,'output/testing/run_learning_rate/kin/')
        ,set_local_configuration_option(plotting_directory
                                       ,'output/testing/run_learning_rate/kin/')
        ,set_local_configuration_option(copy_plotting_scripts
                                      ,[scripts(plotting/learning_rate)])
        ,T = kin/2
        ,M = acc
        ,K = 2 %100
        ,float_interval(1,9,1,Ss)
        %,interval(1,9,1,Is)
        %,findall(S,(member(I,Is)
        %           ,S is I /10)
        %        ,Ss)
        ,learning_rate(T,M,K,Ss,_Ms,_SDs)
        ,set_configuration_option(experiment_file,[P,Mod])
        ,set_local_configuration_option(logging_directory,D_L)
        ,set_local_configuration_option(plotting_directory,D_P)
        ,set_local_configuration_option(copy_plotting_scripts,[Val]).


run_mtg_fragment:-
        experiment_file(P,Mod)
        ,learning_rate_configuration:logging_directory(D_L)
        ,learning_rate_configuration:plotting_directory(D_P)
        ,learning_rate_configuration:copy_plotting_scripts(Val)
        ,set_configuration_option(experiment_file
                                 ,['data/thelma_louise/mtg/mtg_fragment.pl',mtg_fragment])
        ,set_local_configuration_option(logging_directory
                                       ,'output/testing/run_learning_rate/mtg_fragment/')
        ,set_local_configuration_option(plotting_directory
                                       ,'output/testing/run_learning_rate/mtg_fragment/')
        ,set_local_configuration_option(copy_plotting_scripts
                                      ,[scripts(plotting/learning_rate)])
        ,T = ability/2
        ,M = acc
        ,K = 2 %100
        ,float_interval(1,9,1,Ss)
        ,learning_rate(T,M,K,Ss,_Ms,_SDs)
        ,set_configuration_option(experiment_file,[P,Mod])
        ,set_local_configuration_option(logging_directory,D_L)
        ,set_local_configuration_option(plotting_directory,D_P)
        ,set_local_configuration_option(copy_plotting_scripts,[Val]).


run_robots:-
        experiment_file(P,Mod)
        ,learning_rate_configuration:logging_directory(D_L)
        ,learning_rate_configuration:plotting_directory(D_P)
        ,learning_rate_configuration:copy_plotting_scripts(Val)
        ,set_configuration_option(experiment_file
                                 ,['data/thelma_louise/robots/robots.pl',robots])
        ,set_local_configuration_option(logging_directory
                                       ,'output/testing/run_learning_rate/robots/')
        ,set_local_configuration_option(plotting_directory
                                       ,'output/testing/run_learning_rate/robots/')
        ,set_local_configuration_option(copy_plotting_scripts
                                      ,[scripts(plotting/learning_rate)])
        ,use_module('data/thelma_louise/robots/move_generator.pl')
        ,move_generator:write_dataset
        ,T = move/2
        ,M = acc
        ,K = 2 %10
        ,float_interval(1,9,1,Ss)
        %,interval(1,10,1,Ss)
        ,learning_rate(T,M,K,Ss,_Ms,_SDs)
        ,set_configuration_option(experiment_file,[P,Mod])
        ,set_local_configuration_option(logging_directory,D_L)
        ,set_local_configuration_option(plotting_directory,D_P)
        ,set_local_configuration_option(copy_plotting_scripts,[Val]).
