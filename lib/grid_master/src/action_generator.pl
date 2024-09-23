:-module(action_generator,[with_primitives/3
                          ,assert_primitives/3
                          ,erase_primitives/1
                          ,write_primitives/0
                          ,generate_actions/2
                          ,step/6
                          ,passable_tiles/3
                          ,passable_tile/3
                          ,unpassable_tiles/3
                          ,unpassable_tile/3
                          ,start_location/3
                          ,end_location/3
                          ,look_around/4
                          ,look_around_8/4
                          ,surrounding_locations/4
                          ,surrounding_locations_8/4
                          ,write_to_location/5
                          ,nb_write_to_location/5
                          ,map_location/5
                          ]).

:-use_module(src(auxiliaries)).
:-use_module(grid_master_root(grid_master_configuration)).
:-use_module(grid_master_src(map)).
:-use_module(grid_master_src(actions)).

/** <module> Generate background knowledge of grid agents' actions.

*/

%!      with_primitives(+Map,+Module,+Goal) is nondet.
%
%       Call a Goal in the context of a set of primitives.
%
%       Map is a map/3 term used to generate a set of primitives, i.e.
%       an instantiated model. This sert of primitives is asserted to
%       the dynamic database with a call to assert_primitives/2.
%
%       Goal is a goal to be run in the context of the primitives
%       generated from Map. It is expected that Goal will call at least
%       some of the clauses in this set of primitives.
%
%       Module is the name of a module in which to write the generated
%       primitives. Goal is called in Module.
%
%       When Goal exits, all clauses of primitives asserted to the
%       primitives module are erased with a call to erase_primitives/1.
%
%       The point of this predicate is to avoid writing primitives to,
%       and loading them from, a file, because this seems to cause some
%       trouble when trying to create dynamic primitives.
%
with_primitives(Map,M,G):-
        S = assert_primitives(Map,M,Rs)
        ,C = call(M:G)
        ,U = erase_primitives(Rs)
        ,setup_call_cleanup(S,C,U).


%!      assert_primitives(+Map,+Module,-Refs) is det.
%
%       Assert a list of primitives to the dynamic database.
%
%       Map is a map/3 term, used to generate a set of primitives, i.e.
%       an instantiated model. This set of primitives is added to the
%       dynamic database.
%
%       Module is the name of a module in which to write the generated
%       primitives. Goal is called in Module.
%
%       Refs is a list of references of the generated primitives, which
%       can be used to clean them up later.
%
assert_primitives(Map,M,Rs):-
        generate_actions(Map,As)
        ,maplist(varnumbers_names,As,As_,_Ns)
        ,assert_program(M,As_,Rs).


%!      erase_primitives(+Refs) is det.
%
%       Erase a set of primitives, using their Refs.
%
erase_primitives(Rs):-
        erase_program_clauses(Rs).



%!      write_primitives is det.
%
%       Write primitive actions to a file.
%
%       The path of the primitives file is determined by the
%       configuration option primitives_file in
%       generator_configuration.pl.
%
write_primitives:-
        grid_master_configuration:primitives_file(P,_)
        ,S = load_map_files
        ,G = write_primitives(P)
        ,C = unload_all_maps
        ,setup_call_cleanup(S,G,C).



%!      write_primitives(+File) is det.
%
%       Write primitive actions and maps to the given File.
%
%       Filename must be an atom or compound term expandable to a file
%       path.
%
%       write_primitives/1 will write to that file a Prolog module with the
%       primitive actions (step_up/2, step_down/2, step_left/2,
%       step_right/2) generatred by generate_actions/2.
%
%       The generated file can then be imported into an experiment file
%       in Louise to run grid world experiments.
%
write_primitives(F):-
        grid_master_configuration:maps_module(Maps)
        ,S = (expand_file_search_path(F,F_)
             ,open(F_,write,Stm,[alias(primitives_file)
                                ,close_on_abort(true)
                                ])
             )
        ,G = (H = (:-module(primitives,[step_down/2
                                       ,step_left/2
                                       ,step_right/2
                                       ,step_up/2
                                       ])
                  )
             ,portray_clause(Stm,H)
             ,nl(Stm)
             ,Ds = [:-discontiguous(Maps:map/3)
                   ,:-discontiguous(step_down/2)
                   ,:-discontiguous(step_up/2)
                   ,:-discontiguous(step_left/2)
                   ,:-discontiguous(step_right/2)
                   ]
             ,maplist(portray_clause(Stm),Ds)
             ,nl(Stm)
             ,nl(Stm)
             ,forall(Maps:map(Id,Dims,Ms)
                     ,(generate_actions(Id,Vs)
                      ,format(Stm,'%Map of grid world ~w~n',[Id])
                      ,portray_clause(Stm,Maps:map(Id,Dims,Ms),[indent(1)])
                      ,nl(Stm)
                      ,format(Stm,'%Primitive moves for map ~w~n',[Id])
                      ,forall(member(V,Vs)
                             ,portray_clause(Stm,V,[spacing(standard)]))
                      ,nl(Stm)
                      )
                    )
             )
        ,C = close(Stm)
        ,setup_call_cleanup(S,G,C).



%!      generate_actions(+Map,-Actions) is det.
%
%       Generate all possible Actions in a grid world.
%
%       Map is a map/3 term of the form map(Id,Dims,M), where Id is an
%       identifier for the map, Dims is a term Width-Height, the
%       dimensions of the map, and M is a Prolog term storing the map of
%       the grid world as a row-major order "array". See the module
%       comments at the start of this file for more details on map
%       topology.
%
%       Alterantively, Map can be an atomic id of a map/3 term loaded in
%       memory with load_map_file/2 or load_map_files/0.
%
%       Actions is a list of atoms of action predicates. Currently,
%       action predicates are: step_up/2, step_down/2, step_right/2,
%       step_left/2, as generated by the eponymous predicates in this
%       module.
%
generate_actions(Id,Vs):-
        grid_master_configuration:maps_module(Maps)
        ,atomic(Id)
        ,!
        ,Maps:map(Id,Dims,Ms)
        ,generate_actions(map(Id,Dims,Ms),Vs).
generate_actions(map(Id,Dims,Ms),Vs):-
        grid_master_configuration:action_representation(R)
        ,passable_tiles(Ms,Dims,Ts)
        ,findall(M
                ,(member(X/Y,Ts)
                 ,action(R,[map(Id,Dims,Ms),X/Y],M)
                 )
                ,Vs_)
        ,sort(Vs_,Vs).


%!      action(+Representation,+Parameters,-Action) is nondet.
%
%       Generate one clause of one Action.
%
%       As action/4 but the name of the action to generate is taken from
%       the Grid Master configuration.
%
%       Representation is the current value of grid_master_configuration
%       option action_representation/1.
%
%       Parameters are the parameters of the action to generate. Those
%       depend on the action, as defined in the actions module. The name
%       of the action to generate is taken from the
%       grid_master_configuration option action/1.
%
%       Action is one clause of the action as determined by the actions
%       module.
%
action(R,Ps,Ad):-
        grid_master_configuration:action(A)
        ,action(R,A,Ps,Ad).


%!      action(+Representation,+Name,+Parameters,-Action) is nondet.
%
%       Generate one clause of one Action.
%
%       Representation is the current value of grid_master_configuration
%       option action_representation/1.
%
%       Name is the atomic name of the action. This should be defined as
%       a predicate in the actions module.
%
%       Parameters are the parameters of the action to generate. Those
%       depend on the action, as defined in the actions module.
%
%       Action is one clause of the action as determined by the actions
%       module.
%
action(R,A,Ps,Ad):-
        At =.. [A,R,Ps,Ad]
        ,call(actions:At).



%!      step(+Coords,+Delta,+Distance,+Map,+Dims,-Target) is
%!      det.
%
%       Take a step from an initial pair of coordinates.
%
%       Used to define movement actions.
%
%       Coords is a pair X/Y, where X,Y are integers representing a pair
%       of coordinates in the 2-dimensional Cartesian plane with its
%       origin at (0,0). Think of X as the "columns", the Y as the
%       "rows", starting the count at the bottom-left of a 2D grid.
%
%       Delta is one of [+,-], denoting how the given pair of
%       Coordinates is to be modified by the primitive move. "-"
%       increments and "+" decrements, each of X and Y, according to
%       Distance.
%
%       Distance is a pair Dx/Dy, where Dx, Dy are integers,
%       representing the amount by which the corresponding coordinate, X
%       for Dx, Y for Dy, in Coordinates is to be modified by this move.
%
%       Map is a Prolog term representing a map of a 2-dimensional
%       grid world (imagine this world overlayed onto the Cartesian
%       plane; or the other way around) as a row-major order array. Each
%       element of the array, i.e. the content of a "cell" in the grid,
%       is a constant representing the type of terrain of the
%       corresponding map tile. A step action can only begin and end in
%       a "passable" map tile. More categories of tiles may be added
%       later. See comments ar the start of the file for more on map
%       topology.
%
%       Dims is a pair Max_x - Max_y listing the limits of the two map
%       dimensions.
%
%       This predicate applies a Cartesian rotation to map coordinates.
%
step(X/Y,D,Dx/Dy,Ms,Dims,Ex/Ey):-
        passable_tile(X/Y,Ms,Dims)
        ,maplist(ground,[X/Y,D,Dx/Dy,Ms])
	,Mv_x =.. [D,X,Dx]
	,Mv_y =.. [D,Y,Dy]
	,Ex is Mv_x
	,Ey is Mv_y
        ,passable_tile(Ex/Ey,Ms,Dims).



%!      peek(+Coords,+Delta,+Distance,+Map,+Dims,-Target,-Tile) is det.
%
%       Peek at a location from an initial pair of Coords.
%
%       Like step/6 but does not take into account passability of the
%       initial and target location.
%
%       Coords is the pair of coordinates of the locatio to peek at.
%
%       Delta is one of [+,-] denoting how Coords is to be modified.
%
%       Distance is a pair Dx/Dy denoting how much each of Coords is to
%       be modified.
%
%       Map is a map of a grid world.
%
%       Dims is a pair W-H denoting the Width and Height dimensions of
%       the grid world in Map.
%
%       Target is the X/Y pair in Coords modified according to Delta and
%       Distance.
%
%       Tile is the tile type at the Target coordinates.
%
%       Assumes rotation.
%
%       @tbd There's nothing forcing the results of this predicate to be
%       used for "peeking" rather than "stepping", in particular because
%       Target can be taken as the target of a step, just as well as it
%       can be the target of a peek. Maybe, just maybe, don't return a
%       Target?
%
peek(X/Y,D,Dx/Dy,Ms,Dims,Ex/Ey,T):-
        map_location(X/Y,_T,Ms,Dims,true)
        ,maplist(ground,[X/Y,D,Dx/Dy,Ms])
	,Mv_x =.. [D,X,Dx]
	,Mv_y =.. [D,Y,Dy]
	,Ex is Mv_x
	,Ey is Mv_y
        ,map_location(Ex/Ey,T,Ms,Dims,true).



%!      passable_tiles(+Map,+Dimensions,-Tiles) is det.
%
%       Generate a list of all passable Tiles on a Map.
%
%       Map a map/3 term, the map of a grid world.
%
%       Dimensions is a pair Max_x - Max_y listing the limits of the two
%       map dimensions.
%
passable_tiles(Ms,Dims,Ts):-
        findall(X/Y
               ,passable_tile(X/Y,Ms,Dims)
               ,Ts).



%!      passable_tile(?Coordinates,+Map,+Dimensions) is det.
%
%       True for the Coordinates of a passable tile on a Map.
%
%       Coordinates is a pair X/Y, the x and y (column and row)
%       coordinates of a maze map.
%
%       Map is a map/3 term storing the map of a grid world.
%
%       Dimensions is a pair Max_x - Max_y listing the limits of the two
%       map dimensions.
%
%       passable_tile/3 is true when the tile at the given X and Y are
%       marked as passalbe in the given Map.
%
%       Passable tiles are the ones defined in the predicate passable/1.
%
passable_tile(X/Y,Ms,Dims):-
        map_location(X/Y,T,Ms,Dims,true)
        ,grid_master_configuration:passable(T).



%!      unpassable_tiles(+Map,+Dimensions,-Tiles) is det.
%
%       Generate a list of all unpassable Tiles On a Map.
%
%       Map is a map/3 term storing the map of a grid world.
%
%       Dimensions is a pair Max_x - Max_y listing the limits of the two
%       map dimensions.
%
%       Tiles is a list of X/Y coordinates of all tiles in Map that have
%       a tile type T so that \+ passable(T).
%
unpassable_tiles(Ms,Dims,Ts):-
        findall(X/Y
               ,unpassable_tile(X/Y,Ms,Dims)
               ,Ts).



%!      unpassable_tile(?Coordinates,+Map,+Dimensions) is det.
%
%       True for the Coordinates of an unpassable tile on a Map.
%
%       Coordinates is a pair X/Y, the x and y coordinates of a grid
%       world Map.
%
%       Map is a map/3 term storing the map of the grid world.
%
%       Dimensions is a pair Max_x - Max_y listing the limits of the two
%       map dimensions.
%
%       unpassable_tile/3 is true when the type of the tile at the given
%       X and Y is T and passable(T) is false.
%
unpassable_tile(X/Y,Ms,Dims):-
        map_location(X/Y,T,Ms,Dims,true)
        ,unpassable(T).


%!      unpassable(+Tile) is det.
%
%       True when passable(Tile) is false.
%
unpassable(T):-
        \+ grid_master_configuration:passable(T).



%!      start_location(+Map,+Dimensions,-Coords) is nondet.
%
%       An entry point on a Map.
%
%       Dimensions is a pair Max_x - Max_y listing the limits of the two
%       map dimensions.
%
%       Coords is the X/Y pair of coordinates of each start location on
%       the Map.
%
%       All start locations are returned on successive backtracking.
%
start_location(Ms,Dims,X/Y):-
        map_location(X/Y,s,Ms,Dims,true).



%!      end_location(+Map,+Dimensions,-Coords) is nondet.
%
%       An exit point on a Map.
%
%       Dimensions is a pair Max_x - Max_y listing the limits of the two
%       map dimensions.
%
%       Coords is the X/Y pair of coordinaes of each end location on the
%       Map.
%
%       All end locations are returned on successive backtracking.
%
end_location(Ms,Dims,X/Y):-
        map_location(X/Y,e,Ms,Dims,true).



%!      look_around(+Coordinates,+Map,+Dimensions,-Observation) is det.
%
%       Peek all around a pair of Coordinates to observe passability.
%
%       Coordinates is a pair X/Y of coordinates of a location on a map.
%
%       Map is a list-of-lists representing the map of a grid world.
%
%       Dimensions is a pair Width-Height, the maximum x and y
%       dimensions of the maze in Map.
%
%       Observation is an atom of the form URDL, where U, R, D and L
%       are one of [p,u] for "passable" and "unpassable", respectively,
%       according to the tile types in the locations above, to the
%       right, below and to the left, of the location defined by
%       Coordinates.
%
%       @tbd This only looks around in the four cardinal direction but
%       some grid worlds may allow diagonal moves.
%
look_around(X/Y,Ms,Dims,O):-
        look_up(X/Y,Ms,Dims,_,Tu)
        ,look_right(X/Y,Ms,Dims,_,Tr)
        ,look_down(X/Y,Ms,Dims,_,Td)
        ,look_left(X/Y,Ms,Dims,_,Tl)
        ,findall(P
               ,(member(Oi,[Tu,Tr,Td,Tl])
                ,(   passable(Oi)
                 ->  P = p
                 ;   P = u
                 )
                )
               ,Ps)
        ,atomic_list_concat(Ps,'',O).



%!      look_around_8(+Coordinates,+Map,+Dimensions,-Observation)
%!      is det.
%
%       Peek all around a pair of Coordinates to observe passability.
%
%       As look_around/4, but peeks around all eight directions,
%       including the four diagonals.
%
%       @tbd This and look_around/4 can be abstracted away to one
%       predicate receiving as an argument the directions to peek at.
%
look_around_8(X/Y,Ms,Dims,O):-
        look_up(X/Y,Ms,Dims,_,Tu)
        ,look_up_right(X/Y,Ms,Dims,_,Tur)
        ,look_right(X/Y,Ms,Dims,_,Tr)
        ,look_down_right(X/Y,Ms,Dims,_,Tdr)
        ,look_down(X/Y,Ms,Dims,_,Td)
        ,look_down_left(X/Y,Ms,Dims,_,Tdl)
        ,look_left(X/Y,Ms,Dims,_,Tl)
        ,look_up_left(X/Y,Ms,Dims,_,Tul)
        ,findall(P
               ,(member(Oi,[Tu,Tur,Tr,Tdr,Td,Tdl,Tl,Tul])
                ,(   passable(Oi)
                 ->  P = p
                 ;   P = u
                 )
                )
               ,Ps)
        ,atomic_list_concat(Ps,'',O).



%!      surrounding_locations(+Coords,+Map,+Dimensions,-Locations) is
%!      det.
%
%       Collect coordinates of all surrounding Locations.
%
%       As look_around/4, but Locations is a list of pairs
%       [U:Tu,R:Tr,D:Td,L:Tl] where each of U,R,D and L are the X/Y
%       coordinates of the cells Up, to the Right, Down, and Left of the
%       current Coords, and Tu, Tr, Dd and Tl are their respective
%       tile types.
%
surrounding_locations(X/Y,Ms,Dims,[Xu/Yu:Tu,Xr/Yr:Tr,Xd/Yd:Td,Xl/Yl:Tl]):-
        look_up(X/Y,Ms,Dims,Xu/Yu,Tu)
        ,look_right(X/Y,Ms,Dims,Xr/Yr,Tr)
        ,look_down(X/Y,Ms,Dims,Xd/Yd,Td)
        ,look_left(X/Y,Ms,Dims,Xl/Yl,Tl).



%!      surrounding_locations_8(+Coords,+Map,+Dimensions,-Locations) is
%!      det.
%
%       Collect coordinates of all surrounding Locations.
%
%       As surrounding_locations/4, but looks around all eight
%       directions, including diagonally.
%
%       @tbd This and surrounding_locations/4 can be abstracted away to one
%       predicate receiving as an argument the directions to peek at.
%
surrounding_locations_8(X/Y,Ms,Dims,[Xu/Yu:Tu
                                    ,Xur/Yur:Tur
                                    ,Xr/Yr:Tr
                                    ,Xdr/Ydr:Tdr
                                    ,Xd/Yd:Td
                                    ,Xdl/Ydl:Tdl
                                    ,Xl/Yl:Tl
                                    ,Xul/Yul:Tul
                                    ]):-
        look_up(X/Y,Ms,Dims,Xu/Yu,Tu)
        ,look_up_right(X/Y,Ms,Dims,Xur/Yur,Tur)
        ,look_right(X/Y,Ms,Dims,Xr/Yr,Tr)
        ,look_down_right(X/Y,Ms,Dims,Xdr/Ydr,Tdr)
        ,look_down(X/Y,Ms,Dims,Xd/Yd,Td)
        ,look_down_left(X/Y,Ms,Dims,Xdl/Ydl,Tdl)
        ,look_left(X/Y,Ms,Dims,Xl/Yl,Tl)
        ,look_up_left(X/Y,Ms,Dims,Xul/Yul,Tul).



%!      look_up(+Coordinates,+Map,+Dimensions,-Tile) is det.
%
%       Peek above a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, Tile is 'o'.
%
look_up(X/Y,Ms,Dims,X_/Y_,T):-
        peek(X/Y,+,0/1,Ms,Dims,X_/Y_,T)
        ,!.
look_up(_XY,_Ms,_Dims,nil/nil,o).


%!      look_up_right(+Coordinates,+Map,+Dimensions,-Tile) is det.
%
%       Peek above and to the right of a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, Tile is 'o'.
%
look_up_right(X/Y,Ms,Dims,X_/Y_,T):-
        peek(X/Y,+,1/1,Ms,Dims,X_/Y_,T)
        ,!.
look_up_right(_XY,_Ms,_Dims,nil/nil,o).


%!      look_right(+Coordinates,+Map,+Dimensions,-Tile) is det.
%
%       Peek to the right of a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, Tile is 'o'.
%
look_right(X/Y,Ms,Dims,X_/Y_,T):-
        peek(X/Y,+,1/0,Ms,Dims,X_/Y_,T)
        ,!.
look_right(_XY,_Ms,_Dims,nil/nil,o).


%!      look_down_right(+Coordinates,+Map,+Dimensions,-Tile) is det.
%
%       Peek below and to the right of a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, Tile is 'o'.
%
look_down_right(X/Y,Ms,Dims,X_/Y_,T):-
        look_down(X/Y,Ms,Dims,Xr/Yr,_Tr)
        ,Xr/Yr \== nil/nil
        ,look_right(Xr/Yr,Ms,Dims,X_/Y_,T)
        ,!.
look_down_right(_XY,_Ms,_Dims,nil/nil,o).


%!      look_down(+Coordinates,+Map,+Dimensions,-Tile) is det.
%
%       Peek below a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, Tile is 'o'.
%
look_down(X/Y,Ms,Dims,X_/Y_,T):-
        peek(X/Y,-,0/1,Ms,Dims,X_/Y_,T)
        ,!.
look_down(_XY,_Ms,_Dims,nil/nil,o).


%!      look_down_left(+Coordinates,+Map,+Dimensions,-Tile) is det.
%
%       Peek below and to the left of a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, Tile is 'o'.
%
look_down_left(X/Y,Ms,Dims,X_/Y_,T):-
        peek(X/Y,-,1/1,Ms,Dims,X_/Y_,T)
        ,!.
look_down_left(_XY,_Ms,_Dims,nil/nil,o).


%!      look_left(+Coordinates,+Map,+Dimensions,-Tile) is det.
%
%       Peek to the left of a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, Tile is 'o'.
%
look_left(X/Y,Ms,Dims,X_/Y_,T):-
        peek(X/Y,-,1/0,Ms,Dims,X_/Y_,T)
        ,!.
look_left(_XY,_Ms,_Dims,nil/nil,o).


%!      look_up_left(+Coordinates,+Map,+Dimensions,-Tile) is det.
%
%       Peek below and to the left of a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, Tile is 'o'.
%
look_up_left(X/Y,Ms,Dims,X_/Y_,T):-
        look_up(X/Y,Ms,Dims,Xu/Yu,_Tr)
        ,Xu/Yu \== nil/nil
        ,look_left(Xu/Yu,Ms,Dims,X_/Y_,T)
        ,!.
look_up_left(_XY,_Ms,_Dims,nil/nil,o).



%!      write_to_location(?Coordinates,?Tile,+Map,+Dims,+Rot) is nondet.
%
%       Write a Tile to a Map at the given Coordinates.
%
%       Coordinates is a pair of X/Y coordinates on a grid world Map.
%
%       Tile is the type of Tile at the given Coordinates.
%
%       Map is a map/3 term storing the map of a grid world.
%
%       Dims is a pair Width-Height denoting the dimensions of the grid
%       world in Map.
%
%       Rot is a boolean indicating whether a rotation should be applied
%       to Coordinates by calling rotation/3, or not.
%
%       Uses destructive assignment to modify the Tile at Coordinates.
%       Destructive assignment is by setarg/3 so it is undone on
%       backtracking. This is probably safer in the sense that it allows
%       undoing of incorrect choices.
%
write_to_location(X/Y,Tc,M,Ds,true):-
        map_coordinates(X/Y,Ds)
        ,rotation(X/Y,Ds,Xr/Yr)
        ,coordinates_index(Xr/Yr,I,Ds)
        ,setarg(I,M,Tc).
write_to_location(X/Y,T,M,Dims,false):-
        map_coordinates(X/Y,Dims)
        ,coordinates_index(X/Y,I,Dims)
        ,setarg(I,M,T).


%!      nb_write_to_location(?Coordinates,?Tile,+Map,+Dims,+Rot) is
%!      nondet.
%
%       Write a Tile to a Map at the given Coordinates.
%
%       As write_to_location/5 but destructive assignment is with
%       nb_setarg/3, meaning that it is not undone on backtracking.
%
nb_write_to_location(X/Y,Tc,M,Ds,true):-
        map_coordinates(X/Y,Ds)
        ,rotation(X/Y,Ds,Xr/Yr)
        ,coordinates_index(Xr/Yr,I,Ds)
        ,nb_setarg(I,M,Tc).
nb_write_to_location(X/Y,T,M,Dims,false):-
        map_coordinates(X/Y,Dims)
        ,coordinates_index(X/Y,I,Dims)
        ,nb_setarg(I,M,T).


%!      coordinates_index(+Coords,-Index,+Dims) is det.
%
%       Index of a pair of Coords in a row-major order array.
%
coordinates_index(X/Y,I,W-_H):-
        I is 1 + W * Y + X.



%!      map_location(?Coordinates,?Tile,+Map,+Dimensions,+Rotate) is
%!      nondet.
%
%       Accept or generate Coordinates of all Tiles in a Map.
%
%       Coordinates is a pair X/Y, representing coordinates of a Tile in
%       the Map.
%
%       Tile is the symbol of the terrain type at that Tile.
%
%       Map is a Prolog term c(E1, ..., En) where each Ei is the
%       element at index i in a 2-dimensional "array" stored in
%       row-major order.
%
%       Dimensions is a pair Max_x - Max_y listing the limits of the two
%       map dimensions.
%
%       Rotate is a boolean, either 'true' or 'false', denoting whether
%       the X/Y coordinates are to be rotated so that the origin of the
%       map is at the bottom-left corner.
%
map_location(X/Y,T,M,Ds,true):-
        map_coordinates(X/Y,Ds)
        ,rotation(X/Y,Ds,Xr/Yr)
        ,term_at_index(Xr/Yr,T,M,Ds).
map_location(X/Y,T,M,Ds,false):-
        map_coordinates(X/Y,Ds)
        ,term_at_index(X/Y,T,M,Ds).


%!      map_coordinates(+Coordsinates,+Dimenstions) is nondet.
%
%       Recognise or genreate Coordinates in a map.
%
%       Coordinates is a term X/Y where any of X or Y can be a number,
%       or a variable. If either is a variable, it is bound to integers
%       starting at 0 and up to the corresponding term in Dimensions.
%
%       Dimensions is a term W-H, the Width and Height of the map.
%       Dimensions must be ground and W and H must both be numbers.
%
map_coordinates(X/Y,W-H):-
        maplist(number,[X,Y])
        ,maplist(dec,[W,H],[W_,H_])
        ,maplist(between(0),[W_,H_],[X,Y])
        ,!.
map_coordinates(X/Y,W-H):-
% Offset between array lengths and indices
        maplist(dec,[W,H],[W_,H_])
        ,between(0,H_,Y)
        ,between(0,W_,X).

%!      dec(+X,-Y) is det.
%
%       Decrement an integer X.
%
%       Helper arithmetic predicate to be passed to maplist/2 to
%       decrement coordinates.
%
dec(X,X_):- X_ is X - 1.



%!      term_at_index(+Index,+Element,+Term,+Dimensions) is det.
%
%       Locate an Element in a Term indexed in row-major order.
%
%       Index is a pair of X/Y coordinates, corresponding to an index
%       into a 2-dimensional "array" stored as a Prolog term with
%       elements in row-major order, i.e. with reach row side-by-side
%       etc.
%
%       Element is the element at the specified Index.
%
%       Term is the Prolog term storing the "array".
%
%       Dimensions is a pair W-H of the width and height of the "array".
%
term_at_index(X/Y,T,M,W-_H):-
        I is 1 + W * Y + X
        ,arg(I,M,T).


%!      rotation(+Coordinates,+Dimensions,-Rotated) is det.
%
%       Transform between list-based and Cartesian coordinates.
%
%       Coordinates is a pair X/Y representing the x and y coordinates,
%       respectively, to be rotated.
%
%       Dimensions is a pair Max_x - Max_y listing the limits of the two
%       map dimensions.
%
%       Rotated is the pair X/Y transformed so that it is correct in a
%       Cartesian plan with the origin, 0/0, at the bottom-right corner.
%
%       This predicate is applied to the coordinates in a maze map given
%       as a list-of-lists, where each sub-list is a row of the map and
%       each element in a sub-list a tile in the map. The "natural"
%       (i.e. the easy) way to go through this list is by calling nth0/3
%       to find the n'th row, then nth0/3 again to find the n'th tile in
%       that row.
%
%       This "natural way" traverses the map from what is effectively
%       the top-left corner, meaning that the two calls to nth0/3 return
%       "0" as the index of the first element of the first sub-list. We
%       want to treat the maze as a Cartesian plane with the origin at
%       the bottom-left corner instead (because reasons; we may need to
%       calculate Euclidean distance, as a for instance) so this
%       predicate maps between the two representations by applying a
%       simple rotation to the input set of coordinates, to transform
%       them to proper Cartesian coordinates.
%
%       This predicate must be called by any predicate that inspects or
%       manipulates coordinates, e.g. the step_up/2 etc
%       primitive actions. Make sure to call it at the earliest point at
%       which a set of coordinates is encountered, to avoid code
%       duplication.
%
rotation(X/Y,_Mx-My,X/Yt):-
        Yt is (My - 1) - Y
        % Safe for arg/3.
        ,Yt >= 0.
