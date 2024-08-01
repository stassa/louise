:-module(map_display, [write_map_file/2
                      ,print_map/2
                      ,print_map/3
                      ,display_map/3
                      ,display_map/4
                      ,display_tile/5
                      ,display_tile/6
                      ,trace_path/2
                      ,trace_path/3
                      ,trace_path/4
                      ,execute_plan/5
                      ,print_path/3
                      ,print_path/4
                      ,display_path/4
                      ,display_path/5
                      ]).

:-use_module(grid_master_root(grid_master_configuration)).
:-use_module(grid_master_src(action_generator)).

/** <module> Pretty-print grid world maps in glorious ASCII and other formats.

__ Map Printing __

print_map/2 can be used to print a map at the Prolog console. The first
argument of print_map/3 determines what is printed: 'tiles',
'coordinates', or 'both'. Those do what they sound like.

Example of using print_map/2:

==
?- print_map(tiles,short_2).
s f w
w f w
w e w
true.

?- print_map(coordinates,short_2).
0/2 1/2 2/2
0/1 1/1 2/1
0/0 1/0 2/0
true.

?- print_map(both,short_2).
0/2.s 1/2.f 2/2.w
0/1.w 1/1.f 2/1.w
0/0.w 1/0.e 2/0.w
true.
==


__ Path printing __

TODO: needs new representation.

Pretty-print a path through a map:
==
?- _T = lfa/2, _Id = 2, lfa:testing_instance(_T,_Id,_E), trace_path(tiles,_Id,_E).
► ► ▼ ■ □ □ □
■ ■ ▼ ■ □ ■ □
□ ■ ▼ ■ □ ■ □
□ ■ ▼ ■ ■ ■ □
□ ■ ▼ ■ ► ► ▼
□ ■ ▼ ■ ▲ ■ ▼
□ □ ► ► ▲ ■ █
true.
==

Print all paths; useful for testing:
==
?- findall(Id,action_generator:maze(Id,_,_),_Ids), member(Id,_Ids), Id \= tessera, writeln('Id':Id), trace_path(Id), nl, fail ; true.
==

__ Themes __

TODO.


*/


%!      write_map_file(+File,+Map) is det.
%
%       Write a Map to a File.
%
write_map_file(F,map(_Id,W-H,Map)):-
        maplist(dec,[W,H],[W_,H_])
        ,S = (expand_file_search_path(F,E),
             open(E,write,Strm,[alias(output_file)
                               ,close_on_abort(true)
                               ])
            )
        ,G = (forall(between(0,H_,Y)
                    ,(forall(between(0,W_,X)
                             % Whatever, really.
                            ,(action_generator:rotation(X/Y,W-H,Xt/Yt)
                             ,map_location(Xt/Yt,T,Map,W-H,true)
                             ,write_tile(Strm,Xt/Yt,T)
                             )
                            )
                     ,nl(Strm)
                     )
                    )
             )
        ,C = close(Strm)
        ,setup_call_cleanup(S,G,C).


%!      write_tile(+Stream,+Coords,+Tile) is det.
%
%       Write a Tile to a Stream at the given Coords.
%
write_tile(Strm,X/Y,T):-
        theme(Th)
        ,mapping(tiles,F)
        ,grid_master_configuration:tile(T,Th,_As,S)
        ,format(Strm,F,[X/Y,S]).



%!      print_map(+How,+Map) is det.
%
%       Print a Map of a grid world to the top-level.
%
%       As print_map/3 but can take the map to print from the dynamic
%       database where it must be loaded with load_map_file/2.
%
%       Map should be the Id of the map to print. Alternatively, Map can
%       be a map/3 term.
%
print_map(W,map(_Id,Dims,Ms)):-
        !
       ,print_map(W,Dims,Ms).
print_map(W,Id):-
        grid_master_configuration:maps_module(Maps)
        ,Maps:map(Id,Dims,Ms)
        ,print_map(W,Dims,Ms).



%!      print_map(+How,+Dimensions,+Map) is det.
%
%       Print a Map of a grid world to the top-level.
%
%       How is one of:
%       * tiles: prints the map as a list of terrain type symbols.
%       * coordinates: prints the coordinate of each Map location.
%       * both: prints both tiles and coordinates.
%
%       Examples:
%
%       ==
%       ?- _F = data('test_map.map'), map_file_map(_F,map_1,map(Id,_Ds,_M))
%       ,print_map(tiles,_Ds,_M).
%
%       S □ □ ■ □ □ □
%       ■ ■ □ ■ □ ■ □
%       □ ■ □ ■ □ ■ □
%       □ ■ □ ■ ■ ■ □
%       □ ■ □ ■ □ □ □
%       □ ■ □ ■ □ ■ □
%       □ □ □ □ □ ■ E
%       Id = map_1.
%
%       ?- _F = data('test_map.map'), map_file_map(_F,map_1,map(Id,_Ds,_M))
%       ,print_map(coordinates,_Ds,_M).
%       0/6 1/6 2/6 3/6 4/6 5/6 6/6
%       0/5 1/5 2/5 3/5 4/5 5/5 6/5
%       0/4 1/4 2/4 3/4 4/4 5/4 6/4
%       0/3 1/3 2/3 3/3 4/3 5/3 6/3
%       0/2 1/2 2/2 3/2 4/2 5/2 6/2
%       0/1 1/1 2/1 3/1 4/1 5/1 6/1
%       0/0 1/0 2/0 3/0 4/0 5/0 6/0
%       Id = map_1.
%
%       ?- _F = data('test_map.map'), map_file_map(_F,map_1,map(Id,_Ds,_M))
%       ,print_map(both,_Ds,_M).
%       0/6.S 1/6.□ 2/6.□ 3/6.■ 4/6.□ 5/6.□ 6/6.□
%       0/5.■ 1/5.■ 2/5.□ 3/5.■ 4/5.□ 5/5.■ 6/5.□
%       0/4.□ 1/4.■ 2/4.□ 3/4.■ 4/4.□ 5/4.■ 6/4.□
%       0/3.□ 1/3.■ 2/3.□ 3/3.■ 4/3.■ 5/3.■ 6/3.□
%       0/2.□ 1/2.■ 2/2.□ 3/2.■ 4/2.□ 5/2.□ 6/2.□
%       0/1.□ 1/1.■ 2/1.□ 3/1.■ 4/1.□ 5/1.■ 6/1.□
%       0/0.□ 1/0.□ 2/0.□ 3/0.□ 4/0.□ 5/0.■ 6/0.E
%       Id = map_1.
%       ==
%
print_map(W,Max_x-Max_y,Ms):-
        maplist(dec,[Max_x,Max_y],[Max_x_,Max_y_])
        ,forall(between(0,Max_y_,Y)
               ,(forall(between(0,Max_x_,X)
                        % Whatever, really.
                       ,(action_generator:rotation(X/Y,Max_x-Max_y,Xt/Yt)
                        ,map_location(Xt/Yt,T,Ms,Max_x-Max_y,true)
                        ,print_tile(W,Xt/Yt,T)
                        )
                       )
                ,nl
                )
               ).

%!      dec(+X,-Y) is det.
%
%       Decrement an integer X.
%
%       Helper arithmetic predicate to be passed to maplist/2 to
%       decrement coordinates.
%
dec(X,X_):- X_ is X - 1.



%!      display_map(+How,+Map,+Options) is det.
%
%       Print a Map of a grid world to the top-level.
%
%       As display_map/4 but retrieves the map to be displayed from
%       the database, using its Id given in Map.
%
%       Alternatively, Map can be a map/3 term.
%
display_map(W,map(_Id,Ds,M),[Term]):-
        !
        ,display_map(W,Ds,M,[Term]).
display_map(W,Id,[Term]):-
        grid_master_configuration:maps_module(Maps)
        ,Maps:map(Id,Ds,M)
        ,display_map(W,Ds,M,[Term]).



%!      display_map(+How,+Dimensions,+Map,+Options) is det.
%
%       Print a Map of a grid world to the top-level.
%
%       As print_map/3 but Options allow displaying a map in ways other
%       than printing, by passing a suitable display substrate, e.g. a
%       reference to a terminal object etc.
%
%       Currently, display_map/4 displays a map on a tty using the
%       Python library blessed ("more than just a wrapper around
%       curses").
%
display_map(W,Max_x-Max_y,Ms,[Term]):-
        maplist(dec,[Max_x,Max_y],[Max_x_,Max_y_])
        ,display_setup([Term])
        ,forall(between(0,Max_y_,Y)
               ,(forall(between(0,Max_x_,X)
                        % Whatever, really.
                       ,(action_generator:rotation(X/Y,Max_x-Max_y,Xt/Yt)
                        ,map_location(Xt/Yt,T,Ms,Max_x-Max_y,true)
                        ,print_tile(W,Xt/Yt,T)
                        )
                       )
                ,nl
                )
               )
        %,py_call(Term:inkey())
        .



%!      display_tile(+What,+Coords,+Tile,+Dims,+Map) is det.
%
%       Display a Tile at the given Coords.
%
%       Similar to display_map/4 but allows a tile to be displayed at
%       the given coordinates instead of the contents of the map in
%       those coordiantes. The map is not modified.
%
%       Coords is the coordinates of the cell in the map on which the
%       given Tile is to be displayed.
%
%       Tile is the tile to display.
%
%       This predicate makes it possible to animate a map by repeatedly
%       calling it with a different pair of Coords and Tile each time.
%       This will cause each tile in the map to be re-written and that
%       can possibly cause some distortion or flickering, depending on
%       the terminal. On Linux it seems to work OK.
%
%       This version uses only SWI-Prolog printing machinery and does
%       not require an extra argument, but is otherwise copy/pasta from
%       display_tile/6. Mmmm... pasta!
%
display_tile(W,X0/Y0,D,Max_x-Max_y,Ms):-
        maplist(dec,[Max_x,Max_y],[Max_x_,Max_y_])
        ,forall(between(0,Max_y_,Y)
                ,(forall(between(0,Max_x_,X)
                         % Whatever, really.
                        ,(action_generator:rotation(X/Y,Max_x-Max_y,Xt/Yt)
                         ,map_location(Xt/Yt,T,Ms,Max_x-Max_y,true)
                         ,(   X0/Y0 == Xt/Yt
                          ->  print_tile(W,Xt/Yt,D)
                          ;   print_tile(W,Xt/Yt,T)
                          )
                         )
                        )
                 ,nl
                 )
               ).



%!      display_tile(+What,+Coords,+Tile,+Dims,+Map,+Options) is det.
%
%       Display a Tile at the given Coords.
%
%       Similar to display_map/4 but allows a tile to be displayed at
%       the given coordinates instead of the contents of the map in
%       those coordiantes. The map is not modified.
%
%       Coords is the coordinates of the cell in the map on which the
%       given Tile is to be displayed.
%
%       Tile is the tile to display.
%
%       Options are the options to pass to the display device. Currently
%       this can only be a blessed Terminal() object.
%
%       This predicate makes it possible to animate a map by repeatedly
%       calling it with a different pair of Coords and Tile each time.
%       This will cause each tile in the map to be re-written and that
%       can possibly cause some distortion or flickering, depending on
%       the terminal. On Linux it seems to work OK.
%
display_tile(W,X0/Y0,D,Max_x-Max_y,Ms,[Term]):-
        maplist(dec,[Max_x,Max_y],[Max_x_,Max_y_])
        ,display_setup([Term])
        ,forall(between(0,Max_y_,Y)
                ,(forall(between(0,Max_x_,X)
                         % Whatever, really.
                        ,(action_generator:rotation(X/Y,Max_x-Max_y,Xt/Yt)
                         ,map_location(Xt/Yt,T,Ms,Max_x-Max_y,true)
                         ,py_print(Term,move_xy(Xt,Yt),'')
                         ,(   X0/Y0 == Xt/Yt
                          ->  print_tile(W,Xt/Yt,D)
                          ;   print_tile(W,Xt/Yt,T)
                          )
                         )
                        )
                 ,nl
                 %,py_print(Term,move_y(Y))
                 )

               ).


%!      display_setup(+Options) is det.
%
%       Setup a display stream.
%
%       Currently only works for displaying maps on the terminal with
%       the Python library blessed.
%
%       @tbd The reason taht this does not cler the screen is to avoid
%       flickering where a map is printed multiple times.
%
display_setup([Term]):-
        py_call(Term:fullscreen())
        ,py_call(Term:hidden_cursor())
        ,py_print(Term,home,'\n').


%!      py_print(+Terminal,+Command) is det.
%!      py_print(+Terminal,+Command,+End) is det.
%!      py_print(+Terminal,+Command,+End,+Flush) is det.
%
%       Print the escape sequence of a blessed Command.
%
%       Used to clear the screen, move to home, move to x/y locations
%       and so on.
%
%       Command must be a blessed command that returns an escape
%       sequence that can be printed to a terminal to have some effect.
%
%       End is the character to print after the Command output.
%
%       Flush is True or False, whether to flush the output before
%       printing a newline. Doesn't seem to work.
%
py_print(Term,C):-
        py_call(Term:C,R)
        ,py_call(print(R,end='')).

py_print(Term,C,E):-
        py_call(Term:C,R)
        ,py_call(print(R,end=E)).

py_print(Term,C,E,F):-
% Flush doesn't seem to be respected at all.
        py_call(Term:C,R)
        ,py_call(print(R,end=E,flush=F)).



%!      trace_path(+Id,+Instance) is det.
%
%       Pretty-print a path through a maze.
%
%       As path(tiles,Id,Instance).
%
trace_path(Id,E):-
        trace_path(tiles,Id,E).



%!      trace_path(+What,+Id,+Instance) is det.
%
%       Pretty-print a path through a maze.
%
%       What is one of [tiles, coordinates, both] and denotes what to
%       print.
%
%       Id is the id of a maze map.
%
%       Instance is a testing instance through which a path is to be
%       traced.
%
%       The path taken must be defined in the experiment file
%       module so that it is accessible to this module.
%
trace_path(W,Id,E):-
%        debug(trace_path)
        definition_module(E,M)
        ,debug(trace_path,'Finding all paths...',[])
        ,G = findall(Cs
                ,(execute_plan(M,E,[],Cs)
                 )
                ,CS)
        ,time(G)
        %,call(G)
        ,length(CS,N)
        ,debug(trace_path,'Found ~w paths.',[N])
        ,nth1(I,CS,Ps)
        ,debug(trace_path,'Tracing path ~w...',[I])
        ,time( print_path(W,Id,Ps) ).



%!      trace_path(+What,+Id,+Instance,-Steps) is det.
%
%       Pretty-print a path through a maze.
%
%       What is one of [tiles, coordinates, both] and denotes what to
%       print.
%
%       Id is the id of a maze map.
%
%       Instance is a testing instance through which a path is to be
%       traced.
%
%       Steps is an integer, the number of steps taken to solve the
%       maze. This includes steps taken during backtracking.
%
%       The path taken must be defined in the experiment file
%       module so that it is accessible to this module.
%
trace_path(W,Id,E,C):-
%        debug(trace_path)
        definition_module(E,M)
        ,C = c(0)
        ,debug(trace_path,'Finding all paths...',[])
        ,G = findall(Cs
                ,(execute_plan(M,C,E,[],Cs)
                 )
                ,CS)
        ,time(G)
        ,length(CS,N)
        ,debug(trace_path,'Found ~w paths.',[N])
        ,nth1(I,CS,Ps)
        ,debug(trace_path,'Tracing path ~w...',[I])
        ,time( print_path(W,Id,Ps) ).


%!      definition_module(+Instance,-Module) is det.
%
%       Definition Module of a testing Instance.
%
%       Instance is a testing instance for a learned predicate, as
%       returned by testing_instance/3, defined in the current
%       experiment file.
%
%       Module is the definition module of the program defining the
%       predicate of the testing instance. This is used to locate the
%       definition of the predicate so that it ca be tested.
%
%       The experiment file setup seems to change the module where
%       testing_instance/3 is defined everytime the experiment file is
%       loaded.
%
definition_module(E,experiment_file):-
        functor(E,F,A)
        ,current_predicate(experiment_file:F/A)
        ,!.
definition_module(E,M):-
        configuration:experiment_file(_P,M)
        ,functor(E,F,A)
        ,current_predicate(M:F/A).



%!      execute_plan(?Goal,+Steps,+Acc,-Coordinats) is det.
%
%       Follow a path through a map and collect its Coordinates.
%
%       Meta-interpreter for a plan generating paths through a maze.
%
%       Goal is the initial goal of the plan to be executed. This is the
%       same name as a target predicate in an experiment learning a
%       maze-solver.
%
%       Steps is a term c(I) where I is the count of steps taken through
%       the maze while trying to solve it, updated destructively by
%       nb_setarg/3, so that it persists through backtracking. This
%       count includes steps taken during backtracking, i.e. it counts
%       every step taken during the search for a path through a maze,
%       not just the steps of the path returned at the end.
%
%       Acc is an empty list, used as an accumulator of Coordinates.
%
%       Coordinates is a list of pairs X/Y, the coordinates of each
%       floor traversed by a path generated by the learned plan.
%
execute_plan(_M,_C,true,Ms,Ms):-
        !.
execute_plan(M,C,(L,Ls),Acc,Ms):-
        execute_plan(M,C,L,Acc,Acc_)
        ,execute_plan(M,C,Ls,Acc_,Ms).
execute_plan(M,C,(L),Acc,Ms):-
        L \== true
        ,L \= (_,_)
        ,safe_clause(M,L,B)
        ,extract_coords(L,Acc,Acc_)
        ,avoid_oscillation(Acc_)
        ,update_count(C)
        ,execute_plan(M,C,B,Acc_,Ms).


%!      update_count(+Count) is det.
%
%       Update the step counter for execute_plan/5.
%
update_count(C):-
        arg(1,C,I)
        ,succ(I,J)
        ,nb_setarg(1,C,J).


%!      execute_plan(?Goal,+Acc,-Coordinats) is det.
%
%       Follow a path through a map and collect its Coordinates.
%
%       Meta-interpreter for a plan generating paths through a maze.
%
%       Goal is the initial goal of the plan to be executed. This is the
%       same name as a target predicate in an experiment learning a
%       maze-solver.
%
%       Acc is an empty list, used as an accumulator of Coordinates.
%
%       Coordinates is a list of pairs X/Y, the coordinates of each
%       floor traversed by a path generated by the learned plan.
%
execute_plan(_M,true,Ms,Ms):-
        !.
execute_plan(M,(L,Ls),Acc,Ms):-
        execute_plan(M,L,Acc,Acc_)
        ,execute_plan(M,Ls,Acc_,Ms).
execute_plan(M,(L),Acc,Ms):-
        L \== true
        ,L \= (_,_)
        ,safe_clause(M,L,B)
        ,extract_coords(L,Acc,Acc_)
        ,avoid_oscillation(Acc_)
        ,execute_plan(M,B,Acc_,Ms).


%!      safe_clause(?Head,-Body) is nondet.
%
%       Variant of clause/2 avoiding calling built-ins.
%
%       Head is the head of a clause in the program database.
%
%       Body is the body of the clause, or the atom 'true' if Head is
%       the head of a built-in or library predicate.
%
%       Accessing built-ins by a call to clause/2, as in a
%       meta-interpreter like the one in execute_plan/3 raises
%       permission errors. This variant of clause/2 ensures that
%       built-ins (and predicates defined in libraries) are handed off
%       to Prolog instead. Other predicates, particularly learned plans,
%       are beheaded by clause/2 and their body literals returned.
%
safe_clause(M,L,true):-
        built_in_or_library_predicate(L)
        ,!
        ,call(M:L).
safe_clause(M,L,B):-
        clause(M:L,B).


%!	built_in_or_library_predicate(+Predicate) is det.
%
%	True for a built-in or autoloaded Predicate.
%
%	Thin wrapper around predicate_property/2. Used to decide what
%	programs to collect with closure/3 and what programs to
%	encapsulate.
%
%	@tbd Copied from louise/src/auxiliaries.pl
%
built_in_or_library_predicate(H):-
	predicate_property(H, built_in)
	,!.
built_in_or_library_predicate(H):-
	predicate_property(H, autoload(_)).


%!      extract_coords(+Literal,+Acc,-Coords) is det.
%
%       Extract a coordinate pair from a move Literal.
%
%       Literal is a literal of a move predicate, a dyadic predicate
%       where the first argument is a list representing the world-state
%       before the move, and the second argument is a list representing
%       the world-state after the move.
%
%       Acc is the accumulator of coordinates passed-in by
%       execute_plan/3. Each element of this list is a triple X/Y:D
%       where X/Y are the actual coordinates and D is a symbol denoting
%       the direction of the move (according to the move action): 'u'
%       for up, 'd' for down, 'l' for left and 'r' for right.
%
%       Coords is the list of coordinates in Acc, updated with the
%       coordinates of the move represented by Literal.
%
/*
extract_coords(L,Acc,Acc_2):-
        L =.. [Mv,S1,S2]
        ,move_direction(Mv,D)
        ,S1 = [Id,X1/Y1|_]
        ,S2 = [Id,X2/Y2|_]
        ,include_coords(X1/Y1:D,Acc,Acc_1)
        ,include_coords(X2/Y2:D,Acc_1,Acc_2)
        ,!.
extract_coords(_L,Acc,Acc).
*/
extract_coords(L,Acc,Acc_1):-
        L =.. [Mv,_S1,S2]
        ,move_direction(Mv,D)
        ,S2 = [_Id,X2/Y2|_]
        ,include_coords(X2/Y2:D,Acc,Acc_1)
        ,!.
extract_coords(_L,Acc,Acc).


%!      move_direction(?Move,?Direction) is semidet.
%
%       Mapping between Move predicates and Direction symbols.
%
%       Used by extract_coords/3 to construct a coordinate term.
%
%       @tbd These should not be hard-coded. We might want to change
%       them alter on.
%
move_direction(move_down, d).
move_direction(move_up, u).
move_direction(move_left, l).
move_direction(move_right, r).
move_direction(step_down, d).
move_direction(step_up, u).
move_direction(step_left, l).
move_direction(step_right, r).


%!      include_coords(+Coordinates,+Acc,-New) is det.
%
%       Decide whether to add a pair of Coordinates to an accumulator.
%
%       Coordinates is a triple X/Y:D, where X/Y a pair of coordinates
%       and D a direction of movement, as created by extract_coords/3.
%
%       Acc is the accumulator of coordinates:move triples accumulated
%       during execution of a plan, by execute_plan/4.
%
%       New is the accumulator in Acc updated with the given
%       Coordinates, if Coordinaes is ground and it is not already in
%       Acc.
%
%       The motivation for this predicate is to avoid adding
%       already-added coordinates to the accumulator, as a small
%       optimisation of path tracing.
%
include_coords(X/Y:D,Acc,[X/Y:D|Acc]):-
% Start and end are already ground so this excludes them
%        \+ ground(X/Y:D)
        ground(X/Y:D)
        ,\+ memberchk(X/Y,Acc)
        ,debug(include_coords,'Moves: ~w',[[X/Y:D|Acc]])
        ,!.
include_coords(_X/_Y:_D,Acc,Acc).


%!      avoid_oscillation(+Path) is det.
%
%       Check a Path for oscillatory conditions.
%
%       Path is a list of coordinate triples X/Y:D, as returned by
%       extract_coords/3. This is checked against the configuration
%       predicate opposite_direction/2 to make sure that the last two
%       coordinates added to Path are not conducive to an oscillation,
%       such as a military oscillation (l,r,l,r,l,r, ...) or a Batman
%       oscillation (d,u,d,u,d,u,,d,u, ....).
%
%       @tbd This predicate only checks an oscillatory pattern of up to
%       two moves. We can go further than that!
%
avoid_oscillation([]):-
        !.
avoid_oscillation([_]):-
        !.
avoid_oscillation([_:D1,_:D2|_]):-
        debug(avoid_oscillation,'Checking oscillation between ~w, ~w',[D1,D2])
        ,\+ opposite_direction(D1,D2).


%!      opposite_direction(?Direction,?Opposite) is semidet.
%
%       A Direction of movement and its Opposite.
%
%       Used to implement anti-oscillation constraints, Markovian or
%       not.
%
opposite_direction(d,u).
opposite_direction(u,d).
opposite_direction(l,r).
opposite_direction(r,l).



%!      print_path(+What,+Id,+Directions) is det.
%
%       Pretty-print a path through a maze.
%
%       Variant of print_map/2, printing move symbols along with map
%       tiles.
%
%       Directions is a list of triples X/Y:D, as generated by
%       extract_coords/3, where X/Y are the coordinates of the current
%       tile, iff the tile was traversed by a path, and D is the
%       direction in which the tile was traversed. Otherwise D is a tile
%       symbol. Whatever it is, it is to be printed.
%
%       @tbd This is very copy-pasta of print_map/2. Couldn't we
%       abstract the functionality a bit so we don't have two different
%       predicates to print a maze with and without moves?
%
print_path(W,Id,CS):-
        grid_master_configuration:maps_module(Maps)
        ,Maps:map(Id,Ds,M)
        ,print_path(W,Ds,M,CS).



%!      print_path(+What,+Dimensions,+Map,+Directions) is det.
%
%       Pretty-print a path through a maze.
%
%       As print_path/3 but allows the Dimensions and Map to be passed
%       in rather than retrieved by Id.
%
print_path(W,Max_x-Max_y,Ms,CS):-
        maplist(dec,[Max_x,Max_y],[Max_x_,Max_y_])
        ,forall(between(0,Max_y_,Y)
               ,(forall(between(0,Max_x_,X)
                        % Whatever, really.
                       ,(action_generator:rotation(X/Y,Max_x-Max_y,Xt/Yt)
                        ,map_location(Xt/Yt,T,Ms,Max_x-Max_y,true)
                        % Decide if at X/Y is a direction symbol
                        ,(   T == s
                         ->  S = s
                         ;   memberchk(Xt/Yt:D,CS)
                         ->  S = D
                         ;   S = T
                         )
                        ,print_tile(W,Xt/Yt,S)
                        )
                       )
                ,nl
                )
               ).



%!      display_path(+What,+Id,+Directions,+Options) is det.
%
%       Display a path through a maze.
%
%       As print_paht/3, but Options allow display to a stream other
%       than the SWI Prolog console.
%
display_path(W,Id,CS,[Term]):-
        grid_master_configuration:maps_module(Maps)
        ,Maps:map(Id,Ds,M)
        ,display_path(W,Ds,M,CS,[Term]).



%!      display_path(+What,+Dimensions,+Map,+Directions,+Options)
%!      is det.
%
%       Display a path through a maze.
%
%       As display_path/4 but allows the Dimensions and Map to be passed
%       in rather than retrieved by Id.
%
display_path(W,Max_x-Max_y,Ms,CS,[Term]):-
        maplist(dec,[Max_x,Max_y],[Max_x_,Max_y_])
        ,py_call(Term:clear,C)
        ,py_call(print(C))
        ,display_setup([Term])
        ,forall(between(0,Max_y_,Y)
               ,(forall(between(0,Max_x_,X)
                        % Whatever, really.
                       ,(action_generator:rotation(X/Y,Max_x-Max_y,Xt/Yt)
                        ,map_location(Xt/Yt,T,Ms,Max_x-Max_y,true)
                        % Decide if at X/Y is a direction symbol
                        ,(   memberchk(Xt/Yt:D,CS)
                         ->  S = D
                         ;   S = T
                         )
                        ,print_tile(W,Xt/Yt,S)
                        )
                       )
                ,nl
                )
               ).



%!      print_tile(+What,+Coordinates,+Tile) is det.
%
%       Print a map tile which may be a terrain or move symbol.
%
%       What is one of [tiles, coordinates, both], denoting what is to
%       be printed.
%
%       Coordinates is a pair X/Y, the coordinates of the map tile
%       to print.
%
%       Tile is the symbol of the tile at the given coordinates.
%
%       This predicate prints tile symbols with a call to ansi_format/3.
%       The attributes and format string used in the call are determined
%       by tile/5 (attributes) and mapping/2 (format).
%
print_tile(W,X/Y,T):-
        theme(Th)
        ,mapping(W,F)
        ,grid_master_configuration:tile(T,Th,As,S)
        ,ansi_format(As,F,[X/Y,S]).


%!      mapping(?What,?Format) is semidet.
%
%       Mapping between tile printing style and formatting.
%
%       How is one of: [tiles, coordinates, both], which prints what it
%       says.
%
%       Format is a format atom to be used when printing the thing said
%       in What.
%
mapping(tiles,'~i~w ').
mapping(coordinates,'~w~i ').
mapping(both,'~w.~w ').
