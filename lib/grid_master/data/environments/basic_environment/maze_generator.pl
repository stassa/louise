:-module(maze_generator, [mage/2
                         ,starting_location/3
                         ,filled_maze/3
                         ,generator_loop/6
                         ,inverted_maze/2
                         ]).

:-use_module(observation_matrices).
:-use_module(mage_controller,[]).
:-use_module(grid_master_src(action_generator)).
:-use_module(grid_master_src(actions)).
:-use_module(maze_generator_configuration).

/** <module> Use a maze-solving controller to generate a maze.
*/


%!      mage(+Dimensions,-Maze) is det.
%
%       MAze GEnerator.
%
%       Dimensions is a pair Width-Height, denoting the dimensions of a
%       grid on which a maze will be generated.
%
%       Maze is the map of the generated maze as a row-major order
%       array.
%
%       Collects determinism/1 and targets/1 options from the maze
%       generator configuration, to be passed to mage/4.
%
%       Mazes created by this predicate conform to the following
%       specification, as it were:
%       * Every cell in the maze is either a floor tile, or a wall tile.
%       * Each floor tile is reachable from another floor tile by moving
%         only up, right, down or left (i.e. no diagonal moves are
%         needed).
%       * The maze contains no floor tile "plazas" (regions of four
%       floor tiles forming a square).
%       * The maze contains no floor tile loops (where a path of floor
%         tiles ends at a floor tile earlier in the path).
%       * Each wall tile is reachable from another floor tile, or from
%         the edge of the maze by moving up, right, down, or left, or
%         also diagonally up, right, down or left, depending on the
%         maze_generation/1 option in maze_generator_configuration.pl.
%
%       Other than that, floor and wall tiles are placed at random. The
%       effect is of a maze with some inaccessible areas (covered by
%       wall tile clumps) and with "bendy" walls and corridors, and such
%       that the walls are connected to the edges of the maze and all
%       the corridors are connected to each otther. That way an agent
%       can visit every floor tile in the maze and so the maze can
%       always be "solved".
%
%       @tbd This predicate sets grid_master_configuration option
%       observation_matrices/1 to "four_way" to control the generation
%       of observation matrices, used to build a maze. This is clearly a
%       bit of a hack and should instead be replaced with new code in
%       observation_matrices module to allow parameterised generation of
%       observation matrices.
%
mage(Ds,M):-
        maze_generator_configuration:directions(Det)
        ,maze_generator_configuration:targets(T)
        ,maze_generator_configuration:path_origins(O)
        ,grid_master_configuration:observation_matrices(OM)
        ,filled_maze(Ds,w,M)
        ,once( starting_location(O,Ds,X0/Y0) )
        ,S = (retract(grid_master_configuration:observation_matrices(OM))
             ,assert(grid_master_configuration:observation_matrices(four_way))
             )
        ,G = mage_loop(Det,[],[Ds,M,X0/Y0,_T0])
        ,C = (retract(grid_master_configuration:observation_matrices(four_way))
             ,assert(grid_master_configuration:observation_matrices(OM))
             )
        ,setup_call_cleanup(S,G,C)
        ,write_start_exit(T,Ds,M).


%!      filled_maze(+Dimensions,+Tile,-Maze) is det.
%
%       Generate a filled Maze of the given Width and Height.
%
%       Dimensions is a pair Width-Height where Width and Height are
%       integers, from 1 upwards.
%
%       Tile is a constant, representing a maze tile, i.e. the terrain
%       type at a cell in a maze.
%
%       Maze is a list of Height elements each of which is a lists of
%       Width elements, each element filled in with the given Tile.
%
filled_maze(W-H,T,M):-
        findall(Rs
               ,(between(1,H,_K)
                ,filled_path(W,T,Rs)
                )
               ,Ms_)
        ,lists_to_arrays([Ms_],[M]).


%!      filled_path(+Length,+Tile,-Path) is det.
%
%       Generate one filled maze Path of the given Length.
%
%       Length is an integer, which should be 1 or more.
%
%       Tile is a constant, representing a maze tile, i.e. the terrain
%       type at a cell in a maze.
%
%       Path is a list of the given Length where each element is the
%       given Tile.
%
%       Use this to generate the rows and columns of an initial maze map
%       that is filled with wall tiles. Or, dunno, floor tiles also
%       work. Or whale tiles. Have you tried whale tiles?
%
filled_path(L,T,Ps):-
        length(Ps,L)
        ,findall(T
                ,member(T,Ps)
                ,Ps).



%!      starting_location(+Determinism,+Dimensions,-Location) is nondet.
%
%       Generate a starting Location for maze generation.
%
%       Determinism is one of: [random, nondet], denoting how Location
%       is to be generated: either at random and once at a time (i.e.
%       determinstically), or nondeterministically, starting at 0/0 and
%       going alll the way to W/H as defined by Dimensions in
%       successive backtracking.
%
%       Dimensions is a pair W-H, the dimensions of a maze map.
%
%       Location is a pair X/Y the generated coordinates of a location
%       in the maze map.
%
%       Known Determinism options:
%       * random: X/Y are generated by backtracking at random between 0
%         and W-H in Dimensions.
%       * nondet: X/Y are generated by backtracking over values between
%         0 and W-H.
%       * random_nondet: X/Y are generated by backtracking over values
%         between 0 and W-H in random order.
%       * edge_random: X/Y are generated by backtracking at random from
%         the coordinates at the four edges of the maze.
%       * left_edge: X/Y are generated by backtracking over the set of
%         cells at the left edge of the maze.
%       * right_edge: as left_edge but for the right edge.
%       * bottom_edge: as right_edge but for the bottom edge.
%       * top_edge: as right_edge but for the top edge.
%
starting_location(random,W-H,X/Y):-
        maplist(random_between(0),[W,H],[X,Y]).
starting_location(random_nondet,W-H,X/Y):-
        findall(Xi/Yi
               ,maplist(between(0),[W,H],[Xi,Yi])
               ,Cs)
        ,random_permutation(Cs,Rs)
        ,member(X/Y,Rs).
starting_location(nondet,W-H,X/Y):-
        maplist(between(0),[W,H],[X,Y]).
starting_location(edge_random,W-H,X/Y):-
        findall(Xi/Yi
               ,starting_location(edge_nondet,W-H,Xi/Yi)
               ,Es)
        ,random_permutation(Es,Es_r)
        ,member(X/Y,Es_r).
starting_location(edge_nondet,W-H,X/Y):-
        setof(Xi/Yi
             ,W^H^(   starting_location(left_edge,W-H,Xi/Yi)
                  ;   starting_location(bottom_edge,W-H,Xi/Yi)
                  ;   starting_location(right_edge,W-H,Xi/Yi)
                  ;   starting_location(top_edge,W-H,Xi/Yi)
                  )
             ,Es)
        ,member(X/Y,Es).
starting_location(left_edge,_W-H,0/Y):-
        H_ is H - 1
        ,between(0,H_,Y).
starting_location(bottom_edge,W-_H,X/0):-
        W_ is W - 1
        ,between(0,W_,X).
starting_location(right_edge,W-H,W_/Y):-
        H_ is H - 1
        ,W_ is W - 1
        ,between(0,H_,Y).
starting_location(top_edge,W-H,X/H_):-
        H_ is H - 1
        ,W_ is W - 1
        ,between(0,W_,X).



%!      mage_loop(+Determinism,+Passable,+Fluents) is det.
%
%       Business end of mage/3.
%
%       Determinism is as in mage/3.
%
%       Passable is the set of passable tiles in the maze, used to
%       choose new cells to restart the generation with a call to
%       generator_loop/6.
%
%       Fluents is a list [Dims,Map,X/Y,Tile] passed around between
%       generation steps with generator_loop/6.
%
%       As you can probably tell this predicate calls generator_loop/6.
%       It does so nondeterministically in a forall/2 loop, ignoring the
%       returned coordinates at the end. After each such exhaustive
%       generation, the set of passable tiles is collected and one of
%       them chosen (by member/2) and then generator_loop/6 called again
%       starting at that new tile. When the set of passable tiles
%       doesn't change between iterations, the process ends and the maze
%       is returned. This works because generator_loop/6 modifies the
%       maze destructively, with the state persisting through
%       backtracking. See generator_loop/6 for details.
%
mage_loop(_Det,Ps,[Ds,M,_XY,_T]):-
        Ps \== []
        ,passable_tiles(M,Ds,Ps)
        ,!.
mage_loop(Det,Ps,[Ds,M,X0/Y0,T0]):-
        forall(generator_loop(Det,[Ds,M,X0/Y0,T0],_Q0,_O,[Ds,M,_XY1,_T1],_As)
              ,true
              )
        ,passable_tiles(M,Ds,Ts)
        ,Ts \== Ps
        ,member(X2/Y2,Ts)
        ,X2/Y2 \== X0/Y0
        % Cut choicepoints in member/2.
        ,!
        ,mage_loop(Det,Ts,[Ds,M,X2/Y2,_T2]).



%!      generator_loop(+Det,+Fluents,+Q0,+O0,?Goal,-Actions) is nondet.
%
%       Call a controller to generate a single maze path through a grid.
%
%       Det is one of: [random,nondet], denoting the way that new
%       locations are chosen to generate the target path: either at
%       random, or nondeterminstically, on successive backtracking.
%
%       Fluents is a list of fluents of the form [Dims,Map,Loc,Tile]
%       where:
%       * Dims is a pair W-H, the width and height of the grid.
%       * Map is the map of the grid as a row-major array, initially
%         blocked-out with wall tiles.
%       * Loc is a pair X/Y, the coordinates of the starting location of
%         the path through the grid.
%       * Tile is the tile type in the starting location in Loc.
%
%       Q0 and O0 are the starting state and initial observation,
%       respectively, of the controller called to generate a path
%       through the grid.
%
%       Goal is a list of fluents with the same structure as Fluents,
%       used as a goal for the target path, i.e. the target path must
%       connect the starting location in Fluents to the location in
%       Goals. Goal can be left unbound, in which case the path through
%       the maze should be restricted some other way, e.g. the length of
%       Actions.
%
%       Actions is the list of actions taken by the controller through
%       the grid as it generates the target path.
%
generator_loop(D,Fs,Q0,O,Gs,As):-
        first_cell(Fs)
        ,generate_observation(D,O)
        ,debug(generator_loop,'Observation ~w',[O])
        ,path_generator(D,Fs,Q0,O,As,Gs,[]).


%!      first_cell(+Fluents) is det.
%
%       Write a floor tile to the initial cell in a new path.
%
first_cell([Ds,M,XY,_T]):-
        map_display:nb_write_to_location(XY,f,M,Ds,true).


%!      path_generator(+Det,+Fluents,+Q0,O0,+Acc,+Goal,-Actions) is
%!      nondet.
%
%       Business end of generator_loop/6.
%
path_generator(D,Fs,Q0,O,[A_|Acc],Gs,As):-
        controller_tuple(Q0,O,A_,Q1)
        ,new_cell(Fs,A_,Fs_)
        ,generate_observation(D,O_)
        ,debug(environment,'~w -> ~w',[Fs, Fs_])
        ,debug(generator,'~w -> ~w',[(Q0,O,A_,Q1),O_])
        %,Fs_ = [Ds,M,_,_]
        %,print_map(coordinates,Ds,M)
        ,path_generator(D,Fs_,Q1,O_,Acc,Gs,As).
path_generator(_D,Fs,_Q0,_O,As,Fs,As).


%!      controller_tuple(+Q0,+O,-A,-Q1) is nondet.
%
%       Mapping between states, observations and actions.
%
controller_tuple(Q0,O,A,Q1):-
        T =.. [t,Q0,O,A,Q1]
        ,call(mage_controller:T).


%!      new_cell(+Fluents,+Action,-New) is nondet.
%
%       Visit a new cell and possibly turn it into a floor tile.
%
%       Fluents and New are sets of fluents of the form detailed in
%       generator_loop/6. Fluents is the set of fluents before the given
%       Action is taken and New is the set of fluents modified by that
%       Action.
%
%       Action is an action label, denoting the action to be taken to
%       modify Fluents, transforming it into New.
%
new_cell([Ds,M,XY,f],A,[Ds,M,XY_,f]):-
       %print_map(tiles,Ds,M),nl,
        maze_generator_configuration:maze_generation(sparse_diagonal)
        ,generator_action(A,M,Ds,XY,f,XY_,w,_O)
        ,cell_constraints_diag(XY,XY_,M,Ds,w)
        ,!
        ,map_display:nb_write_to_location(XY_,f,M,Ds,true).
new_cell([Ds,M,XY,f],A,[Ds,M,XY_,f]):-
       %print_map(tiles,Ds,M),nl,
        maze_generator_configuration:maze_generation(sparse_orthogonal)
        ,generator_action(A,M,Ds,XY,f,XY_,w,_O)
        ,cell_constraints_orth(XY_,A,Ds,M,w)
        %,!
        ,map_display:nb_write_to_location(XY_,f,M,Ds,true).


%!      generator_action(+A,+Map,+Dims,+XY1,+T1,XY2,T2,-Observation) is nondet.
%
%       Perform action A and return a new Observation label.
%
generator_action(up,Ms,Dims,X/Y,_T,X_/Y_,T_,O):-
        action_generator:peek(X/Y,+,0/1,Ms,Dims,X_/Y_,T_)
        ,generator_actions:look_around(X_/Y_,Ms,Dims,O).

generator_action(down,Ms,Dims,X/Y,_T,X_/Y_,T_,O):-
        action_generator:peek(X/Y,-,0/1,Ms,Dims,X_/Y_,T_)
        ,generator_actions:look_around(X_/Y_,Ms,Dims,O).

generator_action(left,Ms,Dims,X/Y,_T,X_/Y_,T_,O):-
        action_generator:peek(X/Y,-,1/0,Ms,Dims,X_/Y_,T_)
        ,generator_actions:look_around(X_/Y_,Ms,Dims,O).

generator_action(right,Ms,Dims,X/Y,_T,X_/Y_,T_,O):-
        action_generator:peek(X/Y,+,1/0,Ms,Dims,X_/Y_,T_)
        ,generator_actions:look_around(X_/Y_,Ms,Dims,O).


%!      cell_constraints_diag(+Loc,+Map,+Dims,+Tile) is det.
%
%       Constraints on modifying the next tile in a path.
%
%       Loc is a pair X/Y, the current location of the agent in the
%       grid.
%
%       Map and Dims are the map and dimensions of the current maze map,
%       given as a row-major orde array.
%
%       Tile is the tile in Loc.
%
%       This predicate checks two constraints:
%       * The current cell is not a floor cell, i.e. it has not been
%         visited before.
%       * Modifying the current cell will not result in the formation of
%         a plaza (a square block of floor tiles).
%       * Modifying the current cell will not result in the formation of
%         a loop (a line of floor tiles touching itself).
%
%       @tbd The Tile check is not necessary: new_cell/3 only allows the
%       next tile to be a wall tile, so that oscillation is avoided. But
%       leave that argument in for now, just in case I need to check
%       more constraints.
%
cell_constraints_diag(X0/Y0,X1/Y1,M,Ds,w):-
        surrounding_locations_8(X1/Y1,M,Ds,[U:Tu
                                           ,_UR:Tur
                                           ,R:Tr
                                           ,_DR:Tdr
                                           ,D:Td
                                           ,_DL:Tdl
                                           ,L:Tl
                                           ,_UL:Tul
                                           ])
        ,\+ plaza(Tu,Tur,Tr,Tdr,Td,Tdl,Tl,Tul)
        ,\+ loop(X0/Y0,U:Tu,R:Tr,D:Td,L:Tl).


%!      plaza(?Tu,?Tur,?Tr,?Tdr,?Td,?Tdl,?Tl,?Tul) is semidet.
%
%       True when there is a plaza adjacent to the agent's location.
%
%       Inputs are as follows:
%       Tu: Tile in the up direction
%       Tur: Tile in the up-right direction
%       Tr: Tile in the right direction
%       Tdr: Tile in the down-right direction
%       Td: Tile in the down direction
%       Tdl: Tile in the down-left direction
%       Tl: Tile in the left direction
%       Tul: Tile in the up-left direction
%
plaza(f,f,f,_Tdr,_Td,_Tdl,_Tl,_Tul).
plaza(_Tu,_Tur,f,f,f,_Tdl,_Tl,_Tul).
plaza(_Tu,_Tur,_Tr,_Tdr,f,f,f,_Tul).
plaza(f,_Tur,_Tr,_Tdr,_Td,_Tdl,f,f).
% Cause everything to fail.
%plaza(w,w,w,_Tdr,_Td,_Tdl,_Tl,_Tul).
%plaza(_Tu,_Tur,w,w,w,_Tdl,_Tl,_Tul).
%plaza(_Tu,_Tur,_Tr,_Tdr,w,w,w,_Tul).
%plaza(w,_Tur,_Tr,_Tdr,_Td,_Tdl,w,w).


%!      loop(?Coords,+Up,+Right,+Left,+Down) is semidet.
%
%       True when the next step will close a loop of floor tiles.
%
%       Inputs are as follows:
%       * Coords: coordinates of the last cell visited, before
%         replacing the current wall tile with a floor tile.
%       * Up: triple X/Y:T denoting the coordinates of the tile up from
%         Coords and its tile type.
%       * Down: triple X/Y:T denoting the coordinates of the tile below
%         Coords and its tile type.
%       * Right: triple X/Y:T denoting the coordinates of the tile to
%         the right of Coords and its tile type.
%       * Right: triple X/Y:T denoting the coordinates of the tile to
%         the left of Coords and its tile type.
%
%       A loop is present if a) One of the coordinates X/Y in Up, Down,
%       Left or Right is different to Coords, and b) the tile type of
%       the cell at X/Y is a floor tile. If (a) is true, then X/Y is not
%       the last cell visited, so it is a cell adjacent to the next cell
%       to be floored. If (b) is true, then flooring the next cell will
%       close a loop.
%
%       @tbd Illustrate with an example.
%
loop(X0/Y0,Xu/Yu:f,_R:_Tr,_D:_Td,_L:_Tl):- Xu/Yu \== X0/Y0.
loop(X0/Y0,_U:_Tu,Xr/Yr:f,_D:_Td,_L:_Tl):- Xr/Yr \== X0/Y0.
loop(X0/Y0,_U:_Tu,_R:_Tr,Xd/Yd:f,_L:_Tl):- Xd/Yd \== X0/Y0.
loop(X0/Y0,_U:_Tu,_R:_Tr,_D:_Td,Xl/Yl:f):- Xl/Yl \== X0/Y0.


%!      cell_constraints_orth(+Loc,+Action,+Dims,+Map,+Tile) is det.
%
%       Cell constraints for orthogonal-spares generation.
%
%       Loc is the location of the next cell to turn into a floor cell.
%
%       Action is the action by which the cell at Loc is entered.
%
%       Dims, Map are the dimensions and map of a maze.
%
%       Tile is the type of tile at Loc. Should normally always be a
%       wall tile.
%
cell_constraints_orth(X/Y,A,Ds,M,w):-
        closed_cell(X/Y,A,Ds,M).


%!      closed_cell(+Loc,+Action,+Dims,+Map) is det.
%
%       True when a cell is enclosed from all non-origin directions.
%
%       The "origin direction" is the direction of movement into the
%       cell, determined from Action.
%
closed_cell(X/Y,D,Ds,M):-
        look_around_8(X/Y,M,Ds,O)
        ,closed_cell(D,O).

%!      closed_cell(?Action,?Observation) is semidet.
%
%       Determines when a cell is closed from all non-entry directions.
%
%       Action is the action with which the cell was accessed, which
%       tells us the direction from which the cell was accessed.
%
%       Observation is an observation label, denoting the passability of
%       the cells in all eight directions, including diagonals, around
%       the cell that was entred by Action. Observation determines
%       whether that cell is closed on all directions, except the one
%       that the Action came from.
%
%       Some fudging here though: when entering a cell from e.g. above,
%       the observation labels that allow to conclude the cell is closed
%       include those for the cells not only directly, but also
%       diagonally, below the entered cell. That is to so as to allow
%       turns, really.
%
%       A more sparse map can be achieved by a stricter check, that both
%       diagonal directions towards the origin cell are passable.
%       Conversely, a less sparse map will probably need to take into
%       account the observations towards the origin of the _origin_
%       cell. Which can get a bit complicated.
%
closed_cell(up,uuuupuuu).
closed_cell(up,uuuppuuu).
closed_cell(up,uuuuppuu).
%closed_cell(up,uuupppuu).
closed_cell(down,puuuuuuu).
closed_cell(down,puuuuuup).
closed_cell(down,ppuuuuuu).
%closed_cell(down,ppuuuuup).
closed_cell(left,uupuuuuu).
closed_cell(left,uppuuuuu).
closed_cell(left,uuppuuuu).
%closed_cell(left,upppuuuu).
closed_cell(right,uuuuuupu).
closed_cell(right,uuuuuupp).
closed_cell(right,uuuuuppu).
%closed_cell(right,uuuuuppp).


%!      generate_observation(+Determinism,-Label) is nondet.
%
%       Generate an observation Label to guide a controller.
%
%       Determinism is one of [random, nondet], denoting the way that
%       observation labels are to be generated: deterministically (i.e.
%       once for each call) at random, if 'random', or
%       nondeterministically, with all possible labels generated on
%       successive backtracking.
%
%       Observation labels are initially generated with a call to
%       observation_labels/1.
%
%       Known valurs of Determinism:
%       * random: Obaservation labalels are chosen at random from the
%         set generated by observation_labels/1.
%       * nondet: Observation labels are chosen by backtracking over the
%         set generated by observation_labels/1.
%       * random_nondet: as 'nondet' but the set of observations is
%         sorted randomly (with random_permutation/2) before
%         backtracking over it.
%       * constant: 'pppp' is generated on every call.
%
generate_observation(random,O):-
        observation_labels(Ls)
        %,random_permutation(Ls,Ls_)
        %,member(O,Ls_)
        ,random_member(O,Ls).
generate_observation(nondet,O):-
        observation_labels(Ls)
        ,member(O,Ls).
generate_observation(random_nondet,O):-
        observation_labels(Ls)
        ,random_permutation(Ls,Ls_)
        ,member(O,Ls_).
generate_observation(constant,pppp).



%!      write_start_exit(+Targets,+Dimensions,+Maze) is det.
%
%       Write start and exit tiles to a Maze.
%
%       Targets denotes how start and exit locations are chosen.
%       Currently, the only working option is "random" which chooses a
%       random location for each target, while respecing the following
%       constraints:
%       * Start and Exit must not coincide.
%       * Start and Exit locations must initially contain a floor tile.
%
write_start_exit(none,_Ds,_M):-
        !.
write_start_exit(Det,Ds,M):-
        maplist(starting_location(Det,Ds),[Xs/Ys,Xe/Ye])
        ,Xe/Ye \== Xs/Ys
        ,map_location(Xs/Ys,f,M,Ds,true)
        ,map_location(Xe/Ye,f,M,Ds,true)
        ,nb_write_to_location(Xs/Ys,s,M,Ds,true)
        ,nb_write_to_location(Xe/Ye,e,M,Ds,true)
        ,!.



%!      inverted_maze(+Maze,-Inverse) is det.
%
%       Construct the Inverse of a Maze.
%
%       The Inverse of a Maze is a maze where each cell in the original
%       maze is inverted: floors become walls, start becomes end, and if
%       there are "breadcrumbs" their directions are also inverted. See
%       inverse/2 for details of what tile inverts into what other tile.
%
inverted_maze(M,M_):-
        M =.. [S|Ms]
        ,findall(C_
                ,(member(C,Ms)
                 ,(   inverse(C,C_)
                  ->  true
                  ;   C_ = C
                  )
                 )
                ,Ms_)
        ,M_ =.. [S|Ms_].


%!      inverse(?Cell,?Inverse) is semidet.
%
%       A Cell and its Inverse.
%
inverse(w,f).
inverse(f,w).
inverse(s,e).
inverse(e,s).
inverse(l,r).
inverse(r,l).
inverse(d,u).
inverse(u,d).
