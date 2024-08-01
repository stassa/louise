:-module(grid_solver, [background_knowledge/2
                      ,metarules/2
                      ,positive_example/2
                      ,negative_example/2
                       % Uncomment to hard-load primitives from file.
                       %,step_up/2
                       %,step_down/2
                       %,step_left/2
                       %,step_right/2
                      ,solver_test_instance/3
                      ,test_initialisation/4
                      ,list_with_primitives/1
                      ,list_encapsulated_with_primitives/1
                      ,learn_with_primitives/1
                      ,learn_with_primitives/2
                      ]).

:-ensure_loaded('lib/grid_master/load_headless.pl').
:-use_module(grid_master_src(action_generator)).
:-ensure_loaded(grid_master_src(map_display)).
:-ensure_loaded(grid_master_src(map)).
:-use_module(grid_master_data(environments/basic_environment/maze_observations),[]).
:-use_module(lib(controller_freak/controller_freak),[]).

/* Uncomment to hard-load primitives from file.
:-grid_master_configuration:primitives_file(P,_M)
        ,reexport(P).
*/

:- grid_master_configuration:action_representation(R)
   ,format('Loaded actions in ~w representation.~n',[R]).

% If maps are already loaded from primitives don't load them again!
:- load_map_files.

/** <module> Learn a grid navigation solver

Configs:
==
?- list_all(s/2).

Global stack limit 2,147,483,648
Table space 4,294,967,296

clause_limit(2)
max_invented(0)
fetch_clauses(all)
table_meta_interpreter(true)
untable_meta_interpreter(true)
reduction(none)

Positive examples:    1
Negative examples:    0
Background knowledge: 4 [step_down/2,step_left/2,step_right/2,step_up/2]
Metarules:            2 [identity,tailrec]

s([zero,A/B,C,[D,E],[F,G]],[zero,H/I,J,[],[]]).
true.
==

Full problem:

==
?- list_encapsulated_problem(s/2).
Positive examples
-----------------
m(s,[zero,A/B,C,D,[E,F],[G,H],[I,J],[K,L]],[zero,M/N,O,P,[],[],[],[]]).

Negative examples
-----------------

Background knowledge (First Order)
----------------------------------
step_down/2:
m(step_down,[zero,0/1,f,A,[A|B],[uppu|C],[down|D],[q2|E]],[zero,0/0,f,q2,B,C,D,E]).
m(step_down,[zero,1/1,f,A,[A|B],[uupp|C],[down|D],[q2|E]],[zero,1/0,f,q2,B,C,D,E]).

step_left/2:
m(step_left,[zero,1/0,f,A,[A|B],[puup|C],[left|D],[q3|E]],[zero,0/0,f,q3,B,C,D,E]).
m(step_left,[zero,1/1,f,A,[A|B],[uupp|C],[left|D],[q3|E]],[zero,0/1,f,q3,B,C,D,E]).

step_right/2:
m(step_right,[zero,0/0,f,A,[A|B],[ppuu|C],[right|D],[q1|E]],[zero,1/0,f,q1,B,C,D,E]).
m(step_right,[zero,0/1,f,A,[A|B],[uppu|C],[right|D],[q1|E]],[zero,1/1,f,q1,B,C,D,E]).

step_up/2:
m(step_up,[zero,0/0,f,A,[A|B],[ppuu|C],[up|D],[q0|E]],[zero,0/1,f,q0,B,C,D,E]).
m(step_up,[zero,1/0,f,A,[A|B],[puup|C],[up|D],[q0|E]],[zero,1/1,f,q0,B,C,D,E]).

Background knowledge (Second Order)
-----------------------------------
m(identity,P,Q):-m(P,X,Y),m(Q,X,Y)
m(tailrec,P,Q):-m(P,X,Y),m(Q,X,Z),m(P,Z,Y)
true.
==

Note:
==
?- grid_master_configuration:action_representation(R).
R = controller_sequences.
==


Learning:
==
?- time( learn(s/2,_Ps) ), print_clauses(_Ps), length(_Ps,N).
% 39,116 inferences, 0.000 CPU in 0.008 seconds (0% CPU, Infinite Lips)
s(A,B):-step_down(A,B).
s(A,B):-step_left(A,B).
s(A,B):-step_right(A,B).
s(A,B):-step_up(A,B).
s(A,B):-step_down(A,C),s(C,B).
s(A,B):-step_left(A,C),s(C,B).
s(A,B):-step_right(A,C),s(C,B).
s(A,B):-step_up(A,C),s(C,B).
N = 8.
==

Useful test queries:
==
% Test a few mazes:
?- _K = 10, _J = 100, member(_Id,[tessera_1,tessera_2,tessera_3,tessera_4,test_1]), _M = experiment_file, _M:test_initialisation(_Id,Q0,Q1,[XYs,XYe,Ts,Te,Q0,Q1]), once((_M:solver_test_instance(s/2,E,[_Id,_K,_J,XYs,XYe,Ts,Te,Q0,Q1]), call(_M:E))).

% Test zero maze (no initialisation):
% ?- _K = 1, _J = 2, _Id = zero, _M = experiment_file, _M:solver_test_instance(s/2,E,[_Id,_K,_J,XYs,XYe,Ts,Te,Q0,Q1]), call(maze_solver:E).
==

*/
:-nodebug(_).
%:-debug(learn).
%:-debug(top_program).
%:-debug(examples).
%:-debug(signature).
%:-debug(metasubstitutions).
%:-debug(prove_steps).
%:-debug(prove_metasubs).
%:-debug(fetch).
%:-debug(fetch_length).

/*
% Target theory, for testing.
:- table(s/2).
s(A,B):-step_down(A,B).
s(A,B):-step_left(A,B).
s(A,B):-step_right(A,B).
s(A,B):-step_up(A,B).
s(A,B):-step_down(A,C),s(C,B).
s(A,B):-step_left(A,C),s(C,B).
s(A,B):-step_right(A,C),s(C,B).
s(A,B):-step_up(A,C),s(C,B).
*/

:-auxiliaries:set_configuration_option(clause_limit,[2]).
:-auxiliaries:set_configuration_option(max_invented,[0]).
:-auxiliaries:set_configuration_option(reduction,[none]).
:-auxiliaries:set_configuration_option(fetch_clauses,[all]).
%:-auxiliaries:set_configuration_option(fetch_clauses,[[builtins,bk,metarules]]).
:-auxiliaries:set_configuration_option(table_meta_interpreter,[true]).
:-auxiliaries:set_configuration_option(untable_meta_interpreter,[true]).

% McCarthyite constraint for Identity and Tailrec
configuration:metarule_constraints(m(_,P,P),fail).

background_knowledge(s/2,[step_down/2
                         ,step_left/2
                         ,step_right/2
                         ,step_up/2
                         ]).

metarules(s/2,[identity,tailrec]).

% Trace with:
% _T = s/2, _Id = tessera, positive_example(_T,_E), trace_path(tiles,_Id,_E).
positive_example(s/2, E):-
        grid_master_configuration:action_representation(R)
        ,memberchk(R,[stack_based,lookaround])
        ,Id = zero
        ,K = 2
        ,maplist(length,[Os,As],[K,K])
        ,E = s([Id,_Xs/_Ys,_T1,Os,As],[Id,_Xe/_Ye,_T2,[],[]]).
positive_example(s/2, E):-
        grid_master_configuration:action_representation(controller_sequences)
        ,Id = zero
        ,K = 2
        ,maplist(length,[Qs,Os,As,Qs_],[K,K,K,K])
        ,E = s([Id,_Xs/_Ys,_T1,_Q0,Qs,Os,As,Qs_],[Id,_Xe/_Ye,_T2,_Q1,[],[],[],[]]).
positive_example(s/2, E):-
        grid_master_configuration:action_representation(stack_less)
        ,Id = zero
        ,E = s([Id,_Xs/_Ys,_T1],[Id,_Xe/_Ye,_T2]).

negative_example(_,_):- fail.


%!      solver_test_instance(+Target,-Instance,+Parameters) is det.
%
%       A solver Instance to be used for testing.
%
%       @tbd Needs dox.
%
%       Example:
%       ==
%       ?- _Id = tessera_1
%       ,_M = experiment_file
%       ,_M:test_initialisation(_Id,Q0,Q1,[XYs,XYe,Ts,Te,Q0,Q1])
%       ,_M:solver_test_instance(s/2,E,[_Id,10,10,XYs,XYe,Ts,Te,Q0,Q1]).
%
%       XYs = 0/6,
%       XYe = 6/0,
%       Ts = s,
%       Te = e,
%       E = s([tessera_1,0/6,s,Q0
%       ,[_,_,_,_,_,_,_,_,_,_]
%       ,[_,_,_,_,_,_,_,_,_,_]
%       ,[_,_,_,_,_,_,_,_,_,_],[_,_,_,_,_,_,_,_,_,_]]
%       ,[tessera_1,6/0,e,Q1,[],[],[],[]]) ;
%       false.
%       ==
%
solver_test_instance(s/2,E,[Id,N,M,Xs/Ys,Xe/Ye,Ts,Te,Q0,Q1]):-
        grid_master_configuration:action_representation(controller_sequences)
        ,between(N,M,K)
        ,debug(test_instance,'Sequence lengths ~w',[K])
        ,maplist(length,[Qs,Os,As,Qs_],[K,K,K,K])
        ,E = s([Id,Xs/Ys,Ts,Q0,Qs,Os,As,Qs_],[Id,Xe/Ye,Te,Q1,[],[],[],[]]).
solver_test_instance(s/2,E,[Id,N,M,Xs/Ys,Xe/Ye,Ts,Te,_Q0,_Q1]):-
        grid_master_configuration:action_representation(R)
        ,memberchk(R,[stack_based,lookaround])
        ,between(N,M,K)
        ,debug(test_instance,'Sequence lengths ~w',[K])
        ,maplist(length,[Os,As],[K,K])
        ,E = s([Id,Xs/Ys,Ts,Os,As],[Id,Xe/Ye,Te,[],[]]).
solver_test_instance(s/2,E,[Id,_N,_M,Xs/Ys,Xe/Ye,Ts,Te,_Q0,_Q1]):-
        grid_master_configuration:action_representation(stack_less)
        ,E = s([Id,Xs/Ys,Ts],[Id,Xe/Ye,Te]).

solver_test_instance(s/2,E,[Id,_N,_M,Xs/Ys,Xe/Ye,Ts,Te,_Q0,_Q1]):-
        grid_master_configuration:action_representation(list_based)
        ,E = s([Id,Xs/Ys,Ts,[]],[Id,Xe/Ye,Te,_Vs]).



%!      test_initialisation(+Id,+Q0,+Q1,+Inits) is det.
%
%       Initialise a solver test instance.
%
%       @tbd Needs dox.
%
%       Example:
%       ==
%       ?- experiment_file:test_initialisation(tessera_1,Q0,Q1,Is).
%       Is = [0/6,6/0,s,e,Q0,Q1] ;
%       false.
%       ==
%
test_initialisation(Id,Q0,Q1,[Xs/Ys,Xe/Ye,s,e,Q0_,Q1]):-
        grid_master_configuration:maps_module(Maps)
        ,state_init(Q0,q0,Q0_)
        ,Maps:map(Id,Dims,Ms)
        ,start_location(Ms,Dims,Xs/Ys)
        ,end_location(Ms,Dims,Xe/Ye).


%!      state_init(?State,?Label,?Init) is semidet.
%
%       Initialisse a State to a Label, if necessary.
%
%       Mainly used to initialise the starting state of a test run.
%
state_init(q0,q0,q0):- !.
state_init(Q,_,Q):- !.



%!      list_with_primitives(+Map) is det.
%
%       List a MIL problem using primitives generated from a Map.
%
list_with_primitives(Map):-
        G = list_mil_problem(s/2)
        ,with_primitives(Map,user,G).


%!      list_encapsulated_with_primitives(+Map) is det.
%
%       List a MIL problem using primitives generated from a Map.
%
%       As list_with_primitives/1 but lists the encapsulated MIL
%       problem.
%
list_encapsulated_with_primitives(Map):-
        G = list_encapsulated_problem(s/2)
        ,with_primitives(Map,user,G).


%!      learn_with_primitives(+Map) is nondet.
%
%       Learn a program from a set of primitives generated from a Map.
%
learn_with_primitives(Map):-
        G = (time( learn(s/2,Ps) )
            ,print_clauses(Ps)
            ,length(Ps,N)
            ,format('Clauses: ~w~n',[N])
            )
        ,with_primitives(Map,user,G).


%!      learn_with_primitives(+Map,-Program) is nondet.
%
%       Learn a Program from a set of primitives generated from a Map.
%
learn_with_primitives(Map,Ps):-
        G = (time( learn(s/2,Ps) )
            )
        ,with_primitives(Map,user,G).



% ==== Everything after this point is scrap and will be removed. ====


%!      reverse(?Action1,?Action2) is semidet.
%
%       Two actions that are the reverse of each other.
%
%       Action1 and Action2 are labels denoting controller actions.
%       These may or may not be the same as solver actions.
%
%       reverse/2 is used to execute a controller with the possibility
%       of reversing a sequence of actions to backtrack into a
%       previously visited location, without magickally jumping as in a
%       backtracking search of a graph.
%
reverse(up,down).
reverse(down,up).
reverse(left,right).
reverse(right,left).
