:- ensure_loaded(lib(grid_master/load_headless))
   ,ensure_loaded(lib(controller_freak/controller_freak))
   ,ensure_loaded(lib(grid_master/data/environments/basic_environment/maze_generator))
   ,ensure_loaded(lib(grid_master/src/map_display))
   ,ensure_loaded(lib(grid_master/src/action_generator))
   ,ensure_loaded(lib(grid_master/src/map))
   ,ensure_loaded(lib(controller_freak/executors))
   ,ensure_loaded(lib(grid_master/data/environments/basic_environment/basic_environment)).

% Only if not already loaded, e.g. by Solver experiment file.
%:- load_map_files.

/*
:-edit(test_scripts).
:-edit(controller_freak_configuration).
:-edit(grid_master_configuration).
:-edit(basic_environment).
:-edit(executors).
:-edit(action_generator).
*/

/*
Scripts to run executors in different types of mission:

* Seek: search a map for an exit tile.
* Search: search a map exhaustively.
* Survey: search, then seek a map to register both start and exit tiles.
* Patrol: Tavel between landmarks in order.

*/


/* ================================================================================
 * Seek a target already marked on a map.
 * Display with blessed.
================================================================================ */

seek_bigroom:-
        seek_map_terminal(id(big_room),blessed,_,tiles).

seek_room:-
        seek_map_terminal(id(room),blessed,_,tiles).

seek_usual_suspects:-
        member(Id,[tessera_1
                  ,tessera_2
                  ,tessera_3
                  ,tessera_4
                  ,test_1
                  ,room
                  ,big_room
                  ])
        ,seek_map_terminal(id(Id),blessed,_,tiles).

seek_mage_10:-
        seek_map_terminal(id(mageid),blessed,10-10,tiles).

seek_mage_15:-
        seek_map_terminal(id(mageid),blessed,15-15,tiles).

seek_mage_20:-
        seek_map_terminal(id(mageid),blessed,20-20,tiles).

seek_mage_30:-
        seek_map_terminal(id(mageid),blessed,30-30,tiles).

seek_dungeon_1:-
        seek_map_terminal(dungeon(dungeon_1),blessed,_,tiles).

seek_10_dungeons:-
        findall(Id
               ,(between(1,10,K)
                ,atom_concat(dungeon_,K,Id)
                )
               ,Ids)
        ,member(Id,Ids)
        ,seek_map_terminal(dungeon(Id),blessed,_,tiles).


/* ================================================================================
 * Seek a target already marked on a map.
 * Display by printing in the SWI-IDE.
================================================================================ */

print_seek_bigroom:-
        seek_map_terminal(id(big_room),print,_,tiles).

print_seek_room:-
        seek_map_terminal(id(room),print,_,tiles).

print_seek_usual_suspects:-
        member(Id,[tessera_1
                  ,tessera_2
                  ,tessera_3
                  ,tessera_4
                  ,test_1
                  ,room
                  ,big_room
                  ])
        ,seek_map_terminal(id(Id),print,_,tiles).

print_seek_mage_10:-
        seek_map_terminal(id(mageid),print,10-10,tiles).

print_seek_mage_15:-
        seek_map_terminal(id(mageid),print,15-15,tiles).

print_seek_mage_20:-
        seek_map_terminal(id(mageid),print,20-20,tiles).


/* ================================================================================
 * Seek a target on an empty map.
 * Start at a random location.
 * Display with blessed.
================================================================================ */

% Places an exit tile at the far end of the emtpy grid.
seek_empty_10:-
        search_map_terminal(diag,blessed,10-10,s,e,tiles).

seek_empty_15:-
        search_map_terminal(diag,blessed,15-15,s,e,tiles).

seek_empty_20:-
        search_map_terminal(diag,blessed,20-20,s,e,tiles).


/* ================================================================================
 * Seek a target on an empty map.
 * Start at a random location.
 * Display by printing in SWI-IDE
================================================================================ */

% Places an exit tile at the far end of the emtpy grid.
print_seek_empty_10:-
        search_map_terminal(diag,print,10-10,s,e,tiles).

print_seek_empty_15:-
        search_map_terminal(diag,print,15-15,s,e,tiles).

print_seek_empty_20:-
        search_map_terminal(diag,print,20-20,s,e,tiles).


/* ================================================================================
 * Search a map exhaustively.
 * Display with blessed.
================================================================================ */

search_bigroom:-
        search_map_terminal(id(big_room),blessed,_,s,nil,tiles).

search_room:-
        search_map_terminal(id(room),blessed,_,s,nil,tiles).

search_usual_suspects:-
        member(Id,[tessera_1
                  ,tessera_2
                  ,tessera_3
                  ,tessera_4
                  ,test_1
                  ,room
                  ,big_room
                  ])
        ,search_map_terminal(id(Id),blessed,_,s,nil,tiles).

search_mage_10:-
        search_map_terminal(id(mageid),blessed,10-10,s,nil,tiles).

search_mage_15:-
        search_map_terminal(id(mageid),blessed,15-15,s,nil,tiles).

search_mage_20:-
        search_map_terminal(id(mageid),blessed,20-20,s,nil,tiles).


/* ================================================================================
 * Search a map exhaustively.
 * Display with print.
================================================================================ */

print_search_bigroom:-
        search_map_terminal(id(big_room),print,_,s,nil,tiles).

print_search_room:-
        search_map_terminal(id(room),print,_,s,nil,tiles).

print_search_usual_suspects:-
        member(Id,[tessera_1
                  ,tessera_2
                  ,tessera_3
                  ,tessera_4
                  ,test_1
                  ,room
                  ,big_room
                  ])
        ,search_map_terminal(id(Id),print,_,s,nil,tiles).

print_search_mage_10:-
        search_map_terminal(id(mageid),print,10-10,s,nil,tiles).

print_search_mage_15:-
        search_map_terminal(id(mageid),print,15-15,s,nil,tiles).

print_search_mage_20:-
        search_map_terminal(id(mageid),print,20-20,s,nil,tiles).


/* ================================================================================
 * Sarch an empty map exhaustively.
 * Start at a random location.
 * Display with blessed.
================================================================================ */

search_empty_10:-
% Starts in a random location
        starting_location(random_nondet,9-9,Xs/Ys)
        ,!
        ,search_map_terminal(id(Xs/Ys),blessed,10-10,s,nil,tiles).

search_empty_15:-
        starting_location(random_nondet,14-14,Xs/Ys)
        ,!
        ,search_map_terminal(id(Xs/Ys),blessed,15-15,s,nil,tiles).

search_empty_20:-
        starting_location(random_nondet,19-19,Xs/Ys)
        ,!
        ,search_map_terminal(id(Xs/Ys),blessed,20-20,s,nil,tiles).


/* ================================================================================
 * Sarch an empty map exhaustively.
 * Start at a random location.
 * Display by printing in SWI-IDE.
================================================================================ */

print_search_empty_10:-
% Starts in a random location
        starting_location(random_nondet,9-9,Xs/Ys)
        ,!
        ,search_map_terminal(id(Xs/Ys),print,10-10,s,nil,tiles).

print_search_empty_15:-
        starting_location(random_nondet,14-14,Xs/Ys)
        ,!
        ,search_map_terminal(id(Xs/Ys),print,15-15,s,nil,tiles).

print_search_empty_20:-
        starting_location(random_nondet,19-19,Xs/Ys)
        ,!
        ,search_map_terminal(id(Xs/Ys),print,20-20,s,nil,tiles).


/* ================================================================================
 * Carry out a survey mission, mapping out an environment.
 * Display with blessed.
================================================================================ */

survey_usual_suspects:-
        member(Id,[tessera_1
                  ,tessera_2
                  ,tessera_3
                  ,tessera_4
                  ,test_1
                  ,room
                  ,big_room
                  ])
        ,survey_mission(id(Id),blessed,_,s,e,tiles).

survey_mage_10:-
        survey_mission(mage(dyn1),blessed,10-10,s,e,tiles).

survey_mage_15:-
        survey_mission(mage(dyn1),blessed,15-15,s,e,tiles).

survey_mage_20:-
        survey_mission(mage(dyn1),blessed,20-20,s,e,tiles).

survey_dungeon_1:-
        survey_mission(dungeon(dungeon_1),blessed,20-20,s,e,tiles).

survey_dungeon_3:-
        survey_mission(dungeon(dungeon_3),blessed,20-20,s,e,tiles).


/* ================================================================================
 * Carry out a patrol mission, circling between landmarks.
 * Display with blessed.
================================================================================ */

patrol_big_room:-
% Doesn't quite work because start is always marked with S
% So when the map is restarted at last exit, that tile becomes S.
% I think.
        patrol_mission(id(big_room),blessed,_,s,[e,s],tiles).

patrol_hall_a_small:-
        patrol_mission(id(hall_a_small),blessed,_,a,[b,c,g,a],tiles).

patrol_hall_a:-
        patrol_mission(id(hall_a),blessed,_,a,[b,c,g,a],tiles).

print_patrol_big_room:-
        patrol_mission(id(big_room),print,_,s,[e,s],tiles).

print_patrol_hall_a_small:-
        patrol_mission(id(hall_a_small),print,_,a,[b,c,g,a],tiles).

print_patrol_hall_a:-
        patrol_mission(id(hall_a),print,_,a,[b,c,g,a],tiles).



/* ================================================================================
 * Explore a map with a controller to create a SLAMming map.
 * Solve the map with a solver using the SLAMming map as a model.
 * Display with blessed.
 * Doesn't look that great - prefer print_* versions.
================================================================================ */

solve_exec_usual_suspects:-
        member(Id,[tessera_1
                  ])
        ,solve_exec_map(id(Id),blessed,_).

solve_exec_mage_10:-
        solve_exec_map(mage(dyn1),blessed,10-10).

solve_exec_mage_15:-
        solve_exec_map(mage(dyn1),blessed,15-15).

solve_exec_mage_20:-
        solve_exec_map(mage(dyn1),blessed,20-20).


/* ================================================================================
 * Explore a map with a controller to create a SLAMming map.
 * Solve the map with a solver using the SLAMming map as a model.
 * Display in SWI IDE.
================================================================================ */

print_solve_exec_usual_suspects:-
        member(Id,[tessera_1
                  ,tessera_2
                  ,tessera_3
                  ,tessera_4
                  ,test_1
                  ,room
                  ,big_room
                  ])
        ,solve_exec_map(id(Id),print,_).

print_solve_exec_mage_10:-
        solve_exec_map(mage(dyn1),print,10-10).

print_solve_exec_mage_15:-
        solve_exec_map(mage(dyn1),print,15-15).

print_solve_exec_mage_20:-
        solve_exec_map(mage(dyn1),print,20-20).


/* ================================================================================
 * Compare a solver and a controller on the same map
 * Display in SWI IDE.
================================================================================ */

print_compare_usual_suspects:-
        member(Id,[tessera_1
                  ,tessera_2
                  ,tessera_3
                  ,tessera_4
                  ,test_1
                  ,room
                  ,big_room
                  ])
        ,solver_vs_controller(id(Id),_,print).

print_compare_mage_10:-
        solver_vs_controller(mage(dyn1),10-10,print).

print_compare_mage_15:-
        solver_vs_controller(mage(dyn1),15-15,print).

print_compare_mage_20:-
        solver_vs_controller(mage(dyn1),20-20,print).

print_compare_10_dungeons:-
        between(1,10,K)
        ,atom_concat(dungeon_,K,Id)
        ,solver_vs_controller(dungeon(Id),_,print).

print_compare_10_by_2_dungeons:-
        findall(Id
               ,(between(1,10,K)
                ,between(1,2,_)
                ,atom_concat(dungeon_,K,Id)
                )
               ,Ids)
        ,member(Id,Ids)
        ,writeln('Dungeon':Id)
        ,solver_vs_controller(dungeon(Id),_,print).

print_compare_dungeon_3:-
        solver_vs_controller(dungeon(dungeon_3),_,print).

print_compare_dungeon_small:-
        solver_vs_controller(dungeon(dungeon_small),_,print).


/* ================================================================================
 * Compare a solver and a controller on the same map
 * Display with blessed.
================================================================================ */

% This doesn't look too good- you don't have time to see the solver.
% Needs more work on the display I guess.
compare_usual_suspects:-
        member(Id,[tessera_1
                  ,tessera_2
                  ,tessera_3
                  ,tessera_4
                  ,test_1
                  ,room
                  ,big_room
                  ])
        ,solver_vs_controller(id(Id),_,blessed).

compare_dungeons:-
        solver_vs_controller(dungeon(dungeon_1),_,blessed).


/* ================================================================================
 * Solve a map with a solver.
 * Display in SWI IDE.
================================================================================ */

solve_usual_suspects:-
        member(Id,[tessera_1
                  ,tessera_2
                  ,tessera_3
                  ,tessera_4
                  ,test_1
                  ,room
                  ,big_room
                  ])
        ,solve_map(id(Id),_Ds,print).

solve_mage_10:-
        solve_map(mage(dyn1),10-10,print).

solve_mage_15:-
        solve_map(mage(dyn1),15-15,print).

solve_mage_20:-
        solve_map(mage(dyn1),20-20,print).

solve_mage_100:-
        solve_map(mage(dyn1),100-100,print).

solve_dungeon_3:-
        solve_map(dungeon(dungeon_3),_,print).


/* ================================================================================
 * Learn controllers from a solver.
 * Display in SWI IDE, only. No animation needed.
================================================================================ */

solver_controller_usual_suspects:-
        member(Id,[tessera_1
                  ,tessera_2
                  ,tessera_3
                  ,tessera_4
                  ,test_1
                  ,room
                  ,big_room
                  ])
        ,print_map(tiles,Id)
        ,learn_solver_controller(Id).


/* ================================================================================
* Script auxiliaries.
================================================================================ */

%!      solve(?Id,?Clauses) is semidet.
%
%       A Solver's Clauses.
%
%       Id is the identifier of a solver. Currently only the grid
%       navigation solver learend by Louise is known.
%
%       Clauses is the list of clauses of the solver named in Id.
%
%       Convenience predicate to avoid re-learning a solver for each
%       experiment.
%
solver(s/2,[(s(A1,B1):-step_down(A1,B1))
           ,(s(A2,B2):-step_left(A2,B2))
           ,(s(A3,B3):-step_right(A3,B3))
           ,(s(A4,B4):-step_up(A4,B4))
           ,(s(A5,B5):-step_down(A5,C5),s(C5,B5))
           ,(s(A6,B6):-step_left(A6,C6),s(C6,B6))
           ,(s(A7,B7):-step_right(A7,C7),s(C7,B7))
           ,(s(A8,B8):-step_up(A8,C8),s(C8,B8))
           ]).


%!      solve_map(+Tpe,+Display,+Dimensions) is det.
%
%       Solve an environment using a Solver.
%
%       Calls solve_environment/5.
%
solve_map(Map,Ds,Disp):-
        solver(s/2,Ss)
        ,(   Map = id(Id)
         ->  map_term(Id,Ds,_,M)
         ;   Map = mage(Id)
         ->  mage(Ds,Mz)
            ,map_term(Id,Ds,Mz,M)
         ;   Map = dungeon(Id)
         ->  map_term(Id,Ds,Mz,M)
            ,maze_generator:write_start_exit(random_nondet,Ds,Mz)
         )
        ,Ds = W-H
        ,N is W * H
        ,solve_environment(Ss,M,0,N,As_s)
        ,environment_init(M,Disp,Ds,Fs-s,_Q0,_O0,_Gs-e)
        ,executors:debug_environment(solve,'\nSolver:',Disp,As_s,Fs)
        ,!.



%!      solver_vs_controller(+Map,+Dimensions,+Display) is det.
%
%       Compare a solver and a controller on a Map.
%
%       Map may be a map in the database, including a dungeon map in
%       grid_master/data/maps/dungeons, or a mage map.
%
%       To log number of actions:
%       ==
%       debug(seek).
%       debug(solve).
%       ==
%
%       @tbd Can be basd on solve_map/3 (copied from this one).
%
solver_vs_controller(Map,Ds,Disp):-
        solver(s/2,Ss)
        ,(   Map = id(Id)
         ->  map_term(Id,Ds,_,M)
         ;   Map = mage(Id)
         ->  mage(Ds,Mz)
            ,map_term(Id,Ds,Mz,M)
         ;   Map = dungeon(Id)
         ->  map_term(Id,Ds,Mz,M)
            ,maze_generator:write_start_exit(random_nondet,Ds,Mz)
         )
        ,Ds = W-H
        ,N is W * H
        ,solve_environment(Ss,M,0,N,As_s)
        ,environment_init(M,Disp,Ds,Fs-s,Q0,O0,Gs-e)
        ,executors:debug_environment(solve,'\nSolver:',Disp,As_s,Fs)
        ,executor(Fs,Q0,O0,Gs,As,Ms)
        ,executors:clear_marks(Ms)
        ,executors:debug_environment(seek,'\nController:',Disp,As,Fs)
        ,!.



%!      solve_exec_map(+Tpe,+Display,+Dimensions) is det.
%
%       Solve a map with a solver on a SLAM model derived by an FSC.
%
%       Calls solver_executor/4.
%
solve_exec_map(Type,Disp,Ds):-
        solver(s/2,Ps)
        ,(   Type = id(Id)
         ->  map_term(Id,Ds,_,Map)
         ;   Type = mage(Id)
         ->  mage(Ds,Mz)
            ,map_term(Id,Ds,Mz,Map)
         )
        ,writeln('Real map:')
        ,print_map(tiles,Map)
        ,solver_executor(Ps,Map,Disp,_M).



%!      patrol_mission(+Type,+Disp,+Dims,+Start,+Landmarks,-What) is
%!      det.
%
%       Run a patrol mission to visit a series of Landmarks in order.
%
%       Calls executor/7 in a loop with a new target tile, taken from
%       the list of Landmarks, and passing in the SLAMming map returned
%       by the previous call, in each step.
%
%       Remember to set the following overlay_precedence/2 options:
%       ==
%       % If you want to mark landmarks as "E":
%       overlay_precedence(grid_slam,[s>_,'@'>f,'@'>x,'@'>e,e>x,e>f,w>x,f>x]).
%       overlay_precedence(mark,[s>_,e>_]).
%       ==
%
%       @tbd Doesn't seem to work with the backtracking_slam executor,
%       and with maps that have only a start and exit tile.
%
patrol_mission(Type,Disp,Ds,S,Ls,W):-
        patrol_mission(Type,Disp,Ds,S,Ls,[],[],[XY0,M])
        ,!
        ,executors:clear_marks([XY0,M])
        ,print_map(W,M).

%!      patrol_mission(+Typ,+Disp,+Ds,+S,+Ls,+As,+Map,-New) is nondet.
%
%       Business end of patrol_mission/6,
%
patrol_mission(_Type,_Disp,_Ds,_S,[],_As,M,M):-
                !.
patrol_mission(Type,Disp,Ds,S,[L|Ls],Acc,M0,M):-
        init_environment(Type,Disp,Ds,Fs-S,Q0,O0,Gs_0-L)
        ,executor(Fs,Q0,O0,Gs_0,As,M0,M1)
        ,patrol_mission(Type,Disp,Ds,L,Ls,[As|Acc],M1,M).



%!      survey_mission(+Type,+Display,+Dims,+Start,+End,+What) is det.
%
%       Run a survey mission to fully map an environment with SLAM.
%
%       Calls executor/6 twice, once with a non-existing target ('nil')
%       and once with the exit tile as a target. In the first call the
%       controller will traverse the entire map and return to the start,
%       thus mapping out the environment completely. In the second call,
%       the controller witll go from start to end and mark their
%       location. The second map is overlayed on the first thereby
%       completing the first map with the start and missing exit
%       location.
%
%       This is a bit wasteful, I guess, but it allows a controller to
%       carry out survey missions without being explicitly designe for
%       them, i.e. without having observation labels that relate to
%       start and exit tiles. executor/6 only marks the start and end
%       of a controller's trajectory, but a generic grid navigation
%       controller won't actually observe the start and exit tiles and
%       so cannot add them to the SLAMming map, except for this hack.
%
%       Remember to set overlay_precedence/2 options:
%       ==
%       overlay_precedence(mark,[s>_,e>_]).
%       overlay_precedence(survey_mission,[s>_,e>_,'@'>f,'@'>x,w>x,f>x]).
%       ==
%
survey_mission(Type,Disp,Ds,S,T,W):-
        (   Type = id(_Id)
        ->  Typ = Type
        ;   Type = mage(Id)
        ->  mage(Ds,Map)
           ,Typ = map(Id,Ds,Map)
        ;   Type = dungeon(Id)
        ->  map_term(Id,Ds,Mz,M)
           ,maze_generator:write_start_exit(random_nondet,Ds,Mz)
           ,Typ = M
        )
        % search exhaustively
        ,environment_init(Typ,Disp,Ds,Fs-S,Q0,O0,Gs_0-nil)
        ,executor(Fs,Q0,O0,Gs_0,As,[X0/Y0,M0])
        ,executors:clear_marks([X0/Y0,M0])
        % seek target
        ,environment_init(Typ,Disp,Ds,Fs_1-S,Q0,O0,Gs_1-T)
        ,executor(Fs_1,Q0,O0,Gs_1,As_1,[X0/Y0,M0],[X1/Y1,M1])
        ,executors:clear_marks([X1/Y1,M1])
        ,nl
        ,writeln('Unified map:')
        ,print_map(W,M1)
        ,write('\nSearching actions:')
        ,print_up_to(10,As)
        ,write('\nSeeking actions:')
        ,print_up_to(10,As_1)
        ,!.


%!      seek_map_terminal(+Type,+Display,+Dimensions,+What) is det.
%
%       Seek a target already marked on a map.
%
seek_map_terminal(Type,Disp,Ds,W) :-
% Search for standard exit tile.
        search_map_terminal(Type,Disp,Ds,s,e,W).


%!      search_map_terminal(+Type,+Display,+Dimensions,+What) is det.
%
%       Search for a target that may or may not be on a map.
%
search_map_terminal(Type,blessed,Ds,S,T,W) :-
        init_environment(Type,blessed,Ds,Fs-S,Q0,O0,Gs-T)
        ,executor(Fs,Q0,O0,Gs,As,Ms)
        ,(   Ms = [_XY,M]
         ->  nl
            ,writeln('Slamming map:')
            ,print_map(W,M)
         ;   Ms = []
         ->  true
         )
        ,print_up_to(10,As)
        ,!.
search_map_terminal(Type,print,Ds,S,T,W) :-
        init_environment(Type,print,Ds,Fs-S,Q0,O0,Gs-T)
        ,executor(Fs,Q0,O0,Gs,As,Ms)
        ,executors:debug_environment(_,'\nProgress:',print,As,Fs)
        ,(   Ms = [_XY,M]
         ->  nl
            ,writeln('Slamming map:')
            ,print_map(W,M)
         ;   Ms = []
         ->  true
         )
        ,print_up_to(10,As).


%!      init_environment(+Type,+Disp,+Dims,+Fs,+Q0,+O0,+Goals) is det.
%
%       Initialise a maze environment.
%
init_environment(diag,Disp,Ds,Fs-S,q0,O,Gs-E):-
        map:new_map(Ds,f,M)
        ,Xs/Ys = 0/0
        % Assuming a square grid.
        ,Ds = W-H
        ,(   0 is H mod 2
         ->  Xe = 0
         ;   Xe is W - 1
         )
        ,Ye is H - 1
        ,write_to_location(Xs/Ys,S,M,Ds,true)
        ,write_to_location(Xe/Ye,E,M,Ds,true)
        ,!
        ,environment_init(map(egrid,Ds,M),Disp,Ds,Fs-S,q0,O,Gs-E).
init_environment(id(Xs/Ys),Disp,Ds,Fs-S,q0,O,Gs-E):-
        map:new_map(Ds,f,M)
        ,write_to_location(Xs/Ys,S,M,Ds,true)
        ,!
        ,environment_init(map(egrid,Ds,M),Disp,Ds,Fs-S,q0,O,Gs-E).
init_environment(id(Id),Disp,Ds,Fs-S,q0,O,Gs-E):-
        atomic(Id)
        ,environment_init(id(Id),Disp,Ds,Fs-S,q0,O,Gs-E).
init_environment(dungeon(Id),Disp,Ds,Fs-S,q0,O,Gs-E):-
        map_term(Id,Ds,M,Map)
        ,maze_generator:write_start_exit(random_nondet,Ds,M)
        ,environment_init(Map,Disp,Ds,Fs-S,q0,O,Gs-E).


%!      print_up_to(+K,+List) is det.
%
%       Print up to K action labels from a List.
%
print_up_to(K,Ls):-
        length(Ls,K)
        ,!
        ,format('~w Actions: ~w~n',[K,Ls]).
print_up_to(K,Ls):-
        length(Ls,N)
        ,findall(L
               ,(between(1,K,I)
                ,nth1(I,Ls,L)
                )
               ,Ls_)
        % Make list into atom for display
        ,atomic_list_concat(Ls_,',',Ls_A)
        ,format('~n~w of ~w Actions: ~w,... ~n',[K,N,Ls_A]).



%!      print_generated_actions(-N) is det.
%
%       Print a list of actions for a map.
%
%       N is the number of actions generated by the action generator
%       from the map.
%
print_generated_actions(Id,N):-
         G = generate_actions(Id,Vs)
         ,with_primitives(Id,user,G)
         ,maplist(writeln,Vs)
         ,length(Vs,N).


%!      learn_solver_controller(+Id) is det.
%
%       Learn a controller from a solver.
%
%       Id is the identifier of a map to be solved with a learned solver
%       to label examples to learn a controller. The solver is currently
%       taken from solver/2.
%
%       This is a thin shell around learn_solver_controller/4 to print
%       out results easily.
%
learn_solver_controller(Id):-
        learn_solver_controller(Id,Cs,As,Ts)
        ,maplist(print_clauses,['% Controller:','% Actions:','% Tuples:'],[Cs,As,Ts]).


%!      learn_solver_controller(Id,Cs,As,Ts) is det.
%
%       Learn a controller from a solver.
%
%       Id is the identifier of a map.
%
%       Cs, As, Ts are the lists of clauses of the controller, its
%       action predicates and tuples, respectively.
%
%
learn_solver_controller(Id,Cs,As,Ts):-
        map_term(Id,W-H,_,Map)
        ,K is W * H
        ,solver(s/2,Ps)
        ,executors:solved_example(Ps,Map,0,K,E)
        ,solver_controller(c,[E],Cs,As,Ts)
        ,!.


%!      generate_observation_matrices is det.
%
%       Print the set of 15 observation matrices for grid FSCs.
%
generate_observation_matrices:-
         Ds = 3-3
         ,observation_matrices(Ds,Is,As)
         ,nth1(I,As,Map)
         ,write_to_location(1/1,@,Map,Ds,true)
         ,nth1(I,Is,O)
         ,format('~w~n',[O])
         ,map_display:print_map(tiles,Ds,Map)
         ,nl
         , fail
         ; true.


observation_matrices(Ds,Is,As):-
        maze_observations:observations(Os)
        ,findall(O-Ls
                ,(member(Os_i,Os)
                 ,maze_observations:observations_grid(Os_i,Ds,Ls)
                 ,atomic_list_concat(Os_i,'',O)
                 )
                ,I_Ls)
        ,pairs_keys_values(I_Ls,Is,Ls)
        ,maze_observations:lists_to_arrays(Ls,As).
