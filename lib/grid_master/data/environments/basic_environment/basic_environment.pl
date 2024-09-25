:-module(basic_environment, [environment_init/7
                            ,display_environment/5
                            ,new_observation/2
                            ,environment/4
                            ,action/8
                            ]).

:-use_module(grid_master_src(action_generator)).
:-use_module(grid_master_src(actions)).
:-use_module(grid_master_src(map_display)).
%:-use_module(maze_generator).

/** <module> Maze environment.

Predicates to initialise an environment, interact with it, and display
the results.

Needs direction arrows to be declared passable (I think):

==
passable(u).
passable(r).
passable(d).
passable(l).
==


Example calls:
==
% Generating a maze with mage/2.
?- _Type = id(dyn1), _Ds = 8-8, maze_environment:environment_init(_Type,print,_Ds,_Fs-s,_,_,_Gs-e), executor(_Fs,_Q0,_,_Gs,As,_), length(As,K), maze_environment:display_environment(tiles,_Type,print,As,_Fs).
□ S □ □ ■ ■ ■ □
■ ▼ ■ □ □ ■ □ □
■ ▼ ■ ■ □ □ □ ■
► ◄ ▼ ■ ■ □ ■ ■
■ ■ ▼ ■ ■ □ ■ □
E ■ ► ▼ ■ □ □ □
▲ ■ ■ ▼ ■ ■ ■ ■
▲ ◄ ◄ ◄ □ □ □ □
As = [down,down,down,left,right,right,down,down,right,down,down,left,left,left,up,up],
K = 16 ;
false.

% Loading a maze map from memory.
?- _Type = id(tessera_1),  maze_environment:environment_init(_Type,print,_Ds,_Fs-s,_,_,_Gs-e), executor(_Fs,_Q0,_,_Gs,As,_), length(As,K), maze_environment:display_environment(tiles,_Type,print,As,_Fs).
S ► ▼ ■ □ □ □
■ ■ ▼ ■ □ ■ □
▼ ■ ▼ ■ □ ■ □
▲ ■ ▼ ■ ■ ■ □
▲ ■ ▼ ■ ► ► ▼
▲ ■ ▼ ■ ▲ ■ ▼
▲ ◄ ◄ ► ▲ ■ E
As = [right,right,down,down,down,down,down,down,left,left,up,up,up,up,down,down,down,down,right|...],
K = 28 ;
false.
==

*/

%!      environment_init(+Type,+Display,+Dims,-Fluents,-Q0,-O0,-Goal) is
%!      det.
%
%       Initialise a maze environment.
%
%       Typedenotes where the maze map comes from and it can be either a map/3
%       term, or a term id(Id), where Id is an identifier for the map.
%       If Type is a term id(Id) and Id is the identifier of a map/3
%       term in the maps module, then that map is used, otherwise a new
%       maze map is generated using mage/2, and with Id as its
%       identifier.
%
%       Display is one of: [none, print, blessed], denoting how the
%       environment is to be displayed.
%       * none: no display. Not currently implemented.
%       * print: the environment will be displayed at the end of
%         execution with a call to print_path/4.
%       * blassed: the environment will be animated in real time using
%         the blessed Python library to control the terminal. Cannot run
%         in the SWI-Prolog IDE (on windows).
%
%       Dims is a pair W-H, the dimensions of a maze to be generated
%       with mage/2. Has no effect if Type is 'mage'.
%
%       Fluents is a pair Fs-S where Fs is a set of fluents that
%       describe the state of the environment and S is a starting tile
%       matched to a corresponding S variable in Fs. The form of Fs
%       depends on the value of Display:
%       * If Display is 'print', Fs is of the form: [M,X/Y,S], where M
%         is a map/3 term, X/Y the initial location of the agent in the
%         maze and S the symbol at that location.
%       * If Display is 'blessed' an additional element Term is added to
%         the end of Fs, that holds a blessed Terminal() object to be
%         used for display.
%
%       Q0 is the state label for the initial state of a controller
%       exploring the maze.
%
%       O0 is an observation label for the initial observation of a
%       controller exploring the maze.
%
%       Goal is a pair G-E, where G is a list of fluents similar to
%       Fluents with the difference that E is a matched to a variable E,
%       replacing S in Fluents. E can be used to direct a controller to
%       a target tile, e.g. a maze's exit tile.
%
environment_init(id(Id),Disp,Ds,Fs-S,q0,O,Gs-E):-
        grid_master_configuration:maps_module(Maps)
        ,(   Maps:map(Id,Ds,M)
         ->  true
         ;   mage(Ds,M)
         )
        ,!
        ,environment_init(map(Id,Ds,M),Disp,Ds,Fs-S,q0,O,Gs-E).
environment_init(map(Id,Ds,M),Disp,Ds,Fs-S,q0,O,Gs-E):-
        (   start_location(M,Ds,Xs/Ys)
           ,map_location(Xs/Ys,S,M,Ds,true)
        ->  true
        ;   map_location(Xs/Ys,S,M,Ds,true)
        )
        ,look_around(Xs/Ys,M,Ds,O)
        ,(   Disp == blessed
        ->   py_call(blessed:'Terminal'(),Term,[python_object(true)])
         ;   Disp = print
         ->  Term = nil
         )
        ,fluents(id,Disp,map(Id,Ds,M),Xs/Ys,S,Term,Fs)
        ,fluents(id,Disp,map(Id,Ds,M),_XYe,E,Term,Gs).


%!      fluents(?Type,?Disp,?Map,?XY,?Tile,?Term,?Fluents) is det.
%
%       A list of Fluents for a Type of maze and its components.
%
%       Used to compose and decompose lists of fluents for different
%       combinations of Type and Disp(lay) method.
%
%       @tbd The cuts are to avoid unnecessary nondeterminism that e.g.
%       makes backtracking executors get stuck forever in a dead end.
%       True story.
%
%       @tbd This needs some cleaning up after changes to
%       environment_init/7.
%
fluents(mage,print,Map,Xs/Ys,S,nil,[Map,Xs/Ys,S]):- !.
fluents(mage(_),print,Map,Xs/Ys,S,nil,[Map,Xs/Ys,S]):- !.
fluents(id,print,Map,Xs/Ys,S,nil,[Map,Xs/Ys,S]):- !.
fluents(id(_),print,Map,Xs/Ys,S,nil,[Map,Xs/Ys,S]):- !.
fluents(mage,blessed,Map,Xs/Ys,S,Term,[Map,Xs/Ys,S,Term]):- !.
fluents(id,blessed,Map,Xs/Ys,S,Term,[Map,Xs/Ys,S,Term]):- !.
fluents(id(_),blessed,Map,Xs/Ys,S,Term,[Map,Xs/Ys,S,Term]):- !.
fluents(mage(_),blessed,Map,Xs/Ys,S,Term,[Map,Xs/Ys,S,Term]):- !.



%!      display_environment(+How,+Type,+Display,+Actions,+Fluents) is
%!      det.
%
%       Display a maze environment.
%
%       How is one of: [tiles, coordinates, both], denoting how the
%       environment is to be displayed, passed to print_path/4.
%
%       Type is one of [mage, id], denoting where the maze map comes
%       from. See environment_init/7 for details.
%
%       Display is one of: [none,print ,blessed], denoting where the
%       environment is to be displayed. See environment_init/7 for
%       details.
%
%       Actions is a list of action labels representing a path through
%       the maze found by an agent solving the maze.
%
%       Fluens is a set of fluents corresponding to the initial state of
%       the environment, as returned by environment_init/7.
%
display_environment(How,Type,Disp,As,Fs):-
        fluents(Type,Disp,map(_Id,Ds,M),_XY,_T,_Term,Fs)
        ,actions_path(Type,Disp,Fs,As,Ps)
        ,print_path(How,Ds,M,Ps).


%!      actions_path(+Type,+Display,+Fluents,+Actions,-Directions) is
%!      det.
%
%       Convert a list of Actions to a list of Directions
%
%       Type is the type of maze in which the given actions are taken,
%       one of: [mage, id]. See environment_init/7 for details.
%
%       Display is the way to print the maze and path, one of: [none,
%       print, blessed]. See environment_init/7 for details.
%
%       Fluents is a list of fluents for the given Type and Display, as
%       returned by environment_init/7.
%
%       Actions is a list of action labels returned by a controller
%       operating in a maze environment. The list of Actions is one path
%       through the maze, staring at the location in Fluents and
%       possibly ending at the exit of the maze.
%
%       Directions is a list of triples X/Y:D, where each X/Y are the
%       coordinates of a location visited by the controller with one of
%       the actions in Actions and D is the direction of the move
%       represented by that action.
%
%       Directions can be passed to print_path/3 in map_display.pl to
%       print out the path represented by Actions.
%
actions_path(Type,Disp,Fs,As,Ps):-
        actions_path(As,Type,Disp,Fs,Ps,[]).

%!      actions_path(+As,+Tp,+Disp,+Fs,+Acc,-Dirs) is det.
%
%       Business end of actions_path/5.
%
actions_path([],_Type,_Disp,_Fs,Ps,Ps):-
        !.
actions_path([A|As],Type,Disp,Fs,[X/Y:D|Acc],Ps):-
        grid_master_configuration:action_symbol(_,_,A,D)
        ,fluents(Type,Disp,_M,X/Y,_S,_Term,Fs)
        ,environment(Fs,A,_O,Fs_)
        ,actions_path(As,Type,Disp,Fs_,Acc,Ps).



%!      new_observation(+Fluents,-Observation) is det.
%
%       Make a new Observation in the current environment.
%
%       Fluents is a set of fluents, representing an environment.
%
%       Observation is an observation label that corresponds to the
%       current state of the environment as seen in Fluents.
%
%       This predicate is used to make an Observation without having to
%       take an action. For example, it can be used to make an initial
%       observation at the start of a controller's execution.
%
%       Clauses are selected according to the contents of Fluents, as
%       returned by environment_init/7. See that predicate for details.
%
new_observation(Fs,O):-
% Desplays the environment by printing in the Prolog terminal.
        fluents(_,print,map(_Id,Ds,M),X/Y,_T,nil,Fs)
        ,!
        ,actions:look_around(X/Y,M,Ds,O).
new_observation(Fs,O):-
% Displays the environment by printing with a blessed Terminal() object.
        fluents(_,blessed,map(_Id,Ds,M),X/Y,_T,Term,Fs)
        ,actions:look_around(X/Y,M,Ds,O)
        ,py_call(Term:clear,C)
        % New line needed to collocate map with print_map/3.
        % Otherwise, leave end='' and no flush= at all.
        ,py_call(print(C,end='\n',flush='True'))
        ,print_map(tiles,Ds,M).



%!      environment(+Fluents,+Action,-Observation,-New) is det.
%
%       Apply an Action to an environment and observe its New state.
%
%       Fluents is a set of fluents representing an environment.
%
%       Action is an action label.
%
%       Observation is an observation label, the result of taking Action
%       in the environment state reprersented by Fluents.
%
%       New is the environment represented by Fluents changed after
%       Action is taken.
%
%       Clauses of environment/4 are selected depending on the form of
%       Fluents, as returned by environment_init/7. See that predicate
%       for details.
%
environment(Fs,A,O,Fs_):-
% Displays the environment by printing in the Prolog terminal.
        fluents(_,print,map(Id,Ds,M),XY,T,nil,Fs)
        ,(   action(A,M,Ds,XY,T,XY_,T_,O)
         ->  true
         ;   writeln(Id-action(A,M,Ds,XY,T,XY_,T_,O))
            ,throw('You are here')
         )
        ,fluents(_,print,map(Id,Ds,M),XY_,T_,nil,Fs_)
        ,!.
environment(Fs,A,O,Fs_):-
% Displays the environment by printing with a blessed Terminal() object.
        fluents(_,blessed,map(Id,Ds,M),XY,T,Term,Fs)
        % T0 may already be replaced by a direction symbol; or not.
        ,action(A,M,Ds,XY,_T0,XY_,T_,O)
        ,fluents(_,blessed,map(Id,Ds,M),XY_,T_,Term,Fs_)
        ,action_display(Ds,M,XY,XY_,T,A,Term).


%!      action(+A,+Map,+Dims,+XY1,+T1,XY2,T2,-Observation) is nondet.
%
%       Perform action A and return a new Observation label.
%
action(step_up,Ms,Dims,X/Y,T,X_/Y_,T_,O):-
        action_generator:step(X/Y,+,0/1,Ms,Dims,X_/Y_)
        ,action_generator:map_location(X/Y,T,Ms,Dims,true)
        ,action_generator:map_location(X_/Y_,T_,Ms,Dims,true)
        ,actions:look_around(X_/Y_,Ms,Dims,O).

action(step_down,Ms,Dims,X/Y,T,X_/Y_,T_,O):-
        action_generator:step(X/Y,-,0/1,Ms,Dims,X_/Y_)
        ,action_generator:map_location(X/Y,T,Ms,Dims,true)
        ,action_generator:map_location(X_/Y_,T_,Ms,Dims,true)
        ,actions:look_around(X_/Y_,Ms,Dims,O).

action(step_left,Ms,Dims,X/Y,T,X_/Y_,T_,O):-
        action_generator:step(X/Y,-,1/0,Ms,Dims,X_/Y_)
        ,action_generator:map_location(X/Y,T,Ms,Dims,true)
        ,action_generator:map_location(X_/Y_,T_,Ms,Dims,true)
        ,actions:look_around(X_/Y_,Ms,Dims,O).

action(step_right,Ms,Dims,X/Y,T,X_/Y_,T_,O):-
        action_generator:step(X/Y,+,1/0,Ms,Dims,X_/Y_)
        ,action_generator:map_location(X/Y,T,Ms,Dims,true)
        ,action_generator:map_location(X_/Y_,T_,Ms,Dims,true)
        ,actions:look_around(X_/Y_,Ms,Dims,O).


%!      action_display(+Dims,+Map,+XY0,+XY1,+S,+A,+Term) is det.
%
%       Display the result of an action A in the environment.
%
%       S is a symbol to be written to the last location visited by the
%       controller.
%
%       A is the label of the current action, to be translated to a
%       suitable display symbol.
%
action_display(Ds,M,_XY0,X1/Y1,_T,A,Term):-
        action_symbol(_,_,A,S)
        ,map_display:display_tile(tiles,X1/Y1,S,Ds,M,[Term])
        %,sleep(0.3)
        %,py_call(Term:inkey())
        .


%!      bread_crumb(+Map,+Dimensions,+Coords,+Crumb) is det.
%
%       Drop a breadcrumb on a map.
%
%       Map and Dimensions are the array of cells and dimensions of a
%       map.
%
%       Coords is an X/Y pair of coordinates on Map.
%
%       Crumb is symbol to be written at the location on the Map defined
%       by Coordinates.
%
%       The point of this is to mark a position on the Map as visited,
%       including some information about the nature of the visitation
%       (e.g. the direction of travel etc), so that an agent (a solver
%       or a controller) can later incorporate this information into
%       its decision-making.
%
%       @tbd Not currently used.
%
bread_crumb(M,Ds,X/Y,D):-
        write_to_location(X/Y,D,M,Ds,true).
