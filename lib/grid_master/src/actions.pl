:-module(actions, [step_up/3
                  ,step_up_right/3
                  ,step_right/3
                  ,step_down_right/3
                  ,step_down/3
                  ,step_down_left/3
                  ,step_left/3
                  ,step_up_left/3
                  ,look_up/3
                  ,look_up_right/3
                  ,look_right/3
                  ,look_down_right/3
                  ,look_down/3
                  ,look_down_left/3
                  ,look_left/3
                  ,look_up_left/3
                  ,look_around/4
                  ,look_around_8/4
                  ,surrounding_locations/4
                  ,surrounding_locations_8/4
                  ]).

:-use_module(grid_master_src(action_generator)).

/** <module> Action definitions for action generator.

*/


%!      step_up(+Representation,+Parameters,-Step) is det.
%
%       Generate clauses of an action to Step up.
%
%       Representation is the current value of grid_master_configuration
%       option action_representation/1.
%
%       Parameters is a list [Map,Coordinates] where Map is a map/3 term
%       and Coordinates is a pair X/Y, the coordinates of the start
%       location of the step action to be generated.
%
%       Step is an atom of the step_up/2 primitive move action. The
%       form of Step depends on the value of action_representation/1.
%
%       step_up/2 generates steps with a rotation applied to coordinates
%       to transform them into Cartesian coordinates, with the origin
%       at the lower-left corner.
%
step_up(R,Ps,At):-
        step_action(step_up,R,Ps,At).


%!      step_up_right(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to step up and to the right.
%
%       As step_up/3 but steps up and to the right, diagonally.
%
step_up_right(R,Ps,At):-
        step_action(step_up_right,R,Ps,At).


%!      step_right(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to step to the right.
%
%       As step_up/3 but steps to the right.
%
step_right(R,Ps,At):-
        step_action(step_right,R,Ps,At).


%!      step_down_right(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to step down and to the right.
%
%       As step_up/3 but steps down and to the right, diagonally.
%
step_down_right(R,Ps,At):-
        step_action(step_down_right,R,Ps,At).


%!      step_down(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to step down.
%
%       As step_up/3 but steps down.
%
step_down(R,Ps,At):-
        step_action(step_down,R,Ps,At).


%!      step_down_left(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to step down and to the left.
%
%       As step_up/3 but steps down and to the left, diagonally.
%
step_down_left(R,Ps,At):-
        step_action(step_down_left,R,Ps,At).


%!      step_left(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to step to the left.
%
%       As step_up/3 but steps to the left.
%
step_left(R,Ps,At):-
        step_action(step_left,R,Ps,At).


%!      step_up_left(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to step up and to the left.
%
%       As step_up/3 but steps up and to the left, diagonally.
%
step_up_left(R,Ps,At):-
        step_action(step_up_left,R,Ps,At).


%!      step_action(+Name,+Representation,+Params,-Action) is nondet.
%
%       Generate clauses of one step Action.
%
%       Name is the atomic name of the action.
%
%       Representation is the action representation as defined in the
%       grid_master_configuration option action_representation/1.
%
%       Params are the parameters of the step action, currently a map/3
%       term and a pair of X/Y coordinates from which to look in one
%       direction.
%
%       Action is one clause of the named step action with fluents
%       defined according to one of the predicates step_up/5,
%       step_up_right/5, step_right/5, etc.
%
step_action(A,list_based,[map(Id,Dims,Ms),X/Y],At):-
        SA =.. [A,X/Y,Ms,Dims,X_/Y_]
        ,call(SA)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,At =.. [A,[Id,X/Y,T,Vs],[Id,X_/Y_,T_,[A|Vs]]]
        ,Vs = '$VAR'('Vs').

step_action(A,stack_based,[map(Id,Dims,Ms),X/Y],At):-
        SA =.. [A,X/Y,Ms,Dims,X_/Y_]
        ,call(SA)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,At =.. [A,[Id,X/Y,T,[T|Os],[A|As]],[Id,X_/Y_,T_,Os,As]]
        ,As = '$VAR'('As')
        ,Os = '$VAR'('Os').

step_action(A,stack_less,[map(Id,Dims,Ms),X/Y],At):-
        SA =.. [A,X/Y,Ms,Dims,X_/Y_]
        ,call(SA)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,At =.. [A,[Id,X/Y,T],[Id,X_/Y_,T_]].

step_action(A,lookaround,[map(Id,Dims,Ms),X/Y],At):-
        SA =.. [A,X/Y,Ms,Dims,X_/Y_]
        ,call(SA)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,At =.. [A,[Id,X/Y,T,[O|Os],[A|As]],[Id,X_/Y_,T_,Os,As]]
        ,look_around(X/Y,Ms,Dims,O)
        ,As = '$VAR'('As')
        ,Os = '$VAR'('Os').

step_action(A,controller_sequences,[map(Id,Dims,Ms),X/Y],At):-
        SA =.. [A,X/Y,Ms,Dims,X_/Y_]
        ,call(SA)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,state_mapping(A,Q1)
        ,At =.. [A,[Id,X/Y,T,Q0,[Q0|Qs],[O|Os],[A|As],[Q1|Qs_]]
               ,[Id,X_/Y_,T_,Q1,Qs,Os,As,Qs_]]
        ,look_around(X/Y,Ms,Dims,O)
        ,Q0 = '$VAR'('Q0')
        ,Qs = '$VAR'('Qs')
        ,Os = '$VAR'('Os')
        ,As = '$VAR'('As')
        ,Qs_ = '$VAR'('Qs_').


%!      step_up(+Coordinates,+Map,+Dimensions) is det.
%
%       Step up from a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, this
%       predicate fails silently.
%
step_up(X/Y,Ms,Dims,X_/Y_):-
        step(X/Y,+,0/1,Ms,Dims,X_/Y_).


%!      step_up_right(+Coordinates,+Map,+Dimensions) is det.
%
%       Step up and to the right from a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, this
%       predicate fails silently.
%
step_up_right(X/Y,Ms,Dims,X_/Y_):-
        step(X/Y,+,1/1,Ms,Dims,X_/Y_).


%!      step_right(+Coordinates,+Map,+Dimensions) is det.
%
%       Step to the right from a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, this
%       predicate fails silently.
%
step_right(X/Y,Ms,Dims,X_/Y_):-
        step(X/Y,+,1/0,Ms,Dims,X_/Y_).


%!      step_down_right(+Coordinates,+Map,+Dimensions) is det.
%
%       Step down and to the right from a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, this
%       predicate fails silently.
%
step_down_right(X/Y,Ms,Dims,X_/Y_):-
        step_down(X/Y,Ms,Dims,Xr/Yr)
        ,step_right(Xr/Yr,Ms,Dims,X_/Y_).


%!      step_down(+Coordinates,+Map,+Dimensions) is det.
%
%       Step down from a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, this
%       predicate fails silently.
%
step_down(X/Y,Ms,Dims,X_/Y_):-
        step(X/Y,-,0/1,Ms,Dims,X_/Y_).


%!      step_down_left(+Coordinates,+Map,+Dimensions) is det.
%
%       Step down and to the left from a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, this
%       predicate fails silently.
%
step_down_left(X/Y,Ms,Dims,X_/Y_):-
        step(X/Y,-,1/1,Ms,Dims,X_/Y_).


%!      step_left(+Coordinates,+Map,+Dimensions) is det.
%
%       Step left from a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, this
%       predicate fails silently.
%
step_left(X/Y,Ms,Dims,X_/Y_):-
        step(X/Y,-,1/0,Ms,Dims,X_/Y_).


%!      step_up_left(+Coordinates,+Map,+Dimensions) is det.
%
%       Step up and to the left of a pair of Coordinates in a Map.
%
%       If the location at Coordinates is outside the map, this
%       predicate fails silently.
%
step_up_left(X/Y,Ms,Dims,X_/Y_):-
        step_up(X/Y,Ms,Dims,Xu/Yu)
        ,step_left(Xu/Yu,Ms,Dims,X_/Y_).



%!      look_up(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action looking up.
%
%       Representation is the action representation as defined in the
%       grid_master_configuration option action_representation/1.
%
%       Parameters are the parameters of the look action, currently a
%       map/3 term and a pair of X/Y coordinates from which to look in
%       one direction.
%
%       Action is one clause of the named look action, generated by
%       look_action/4.
%
look_up(R,Ps,At):-
        look_action(look_up,R,Ps,At).


%!      look_up_right(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to look up and to the right.
%
look_up_right(R,Ps,At):-
        look_action(look_up_right,R,Ps,At).


%!      look_right(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to look to the right.
%
look_right(R,Ps,At):-
        look_action(look_right,R,Ps,At).


%!      look_down_right(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to look down and to the right.
%
look_down_right(R,Ps,At):-
        look_action(look_down_right,R,Ps,At).


%!      look_down(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to look down.
%
look_down(R,Ps,At):-
        look_action(look_down,R,Ps,At).


%!      look_down_left(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to look down and to the left.
%
look_down_left(R,Ps,At):-
        look_action(look_down_left,R,Ps,At).


%!      look_left(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to look to the left.
%
look_left(R,Ps,At):-
        look_action(look_left,R,Ps,At).


%!      look_up_left(+Representation,+Parameters,-Action) is nondet.
%
%       Generate clauses of an Action to look up and to the left.
%
look_up_left(R,Ps,At):-
        look_action(look_up_left,R,Ps,At).


%!      look_action(+Name,+Representation,+Params,-Action) is nondet.
%
%       Generate clauses of one observation Action.
%
%       Name is the atomic name of the action.
%
%       Representation is the action representation as defined in the
%       grid_master_configuration option action_representation/1.
%
%       Params are the parameters of the look action, currently a map/3
%       term and a pair of X/Y coordinates from which to look in one
%       direction.
%
%       Action is one clause of the named look action with fluents
%       defined according to one of the predicates look_up/5,
%       look_up_right/5, look_right/5, etc.
%
look_action(A,list_based,[map(Id,Dims,Ms),X/Y],At):-
        LA =.. [A,X/Y,Ms,Dims,X_/Y_,T_]
        ,call(LA)
        ,peek(X/Y,+,0/0,Ms,Dims,X/Y,T)
        ,At =.. [A,[Id,X/Y,T,Vs],[Id,X_/Y_,T_,[A|Vs]]]
        ,Vs = '$VAR'('Vs').

look_action(A,stack_based,[map(Id,Dims,Ms),X/Y],At):-
        LA =.. [A,X/Y,Ms,Dims,X_/Y_,T_]
        ,call(LA)
        ,peek(X/Y,+,0/0,Ms,Dims,X/Y,T)
        ,At =.. [A,[Id,X/Y,T,[T|Os],[A|As]],[Id,X_/Y_,T_,Os,As]]
        ,As = '$VAR'('As')
        ,Os = '$VAR'('Os').

look_action(A,stack_less,[map(Id,Dims,Ms),X/Y],At):-
        LA =.. [A,X/Y,Ms,Dims,X_/Y_,T_]
        ,call(LA)
        ,peek(X/Y,+,0/0,Ms,Dims,X/Y,T)
        ,At =.. [A,[Id,X/Y,T],[Id,X_/Y_,T_]].

look_action(A,lookaround,[map(Id,Dims,Ms),X/Y],At):-
        LA =.. [A,X/Y,Ms,Dims,X_/Y_,T_]
        ,call(LA)
        ,peek(X/Y,+,0/0,Ms,Dims,X/Y,T)
        ,look_around(X/Y,Ms,Dims,O)
        ,At =.. [A,[Id,X/Y,T,[O|Os],[A|As]],[Id,X_/Y_,T_,Os,As]]
        ,As = '$VAR'('As')
        ,Os = '$VAR'('Os').

look_action(A,controller_sequences,[map(Id,Dims,Ms),X/Y],At):-
        LA =.. [A,X/Y,Ms,Dims,X_/Y_,T_]
        ,call(LA)
        ,peek(X/Y,+,0/0,Ms,Dims,X/Y,T)
        ,look_around(X/Y,Ms,Dims,O)
        ,state_mapping(A,Q1)
        ,At =.. [A,[Id,X/Y,T,Q0,[Q0|Qs],[O|Os],[A|As],[Q1|Qs_]]
                   ,[Id,X_/Y_,T_,Q1,Qs,Os,As,Qs_]]
        ,Q0 = '$VAR'('Q0')
        ,Qs = '$VAR'('Qs')
        ,Os = '$VAR'('Os')
        ,As = '$VAR'('As')
        ,Qs_ = '$VAR'('Qs_').


%!      state_mapping(?Action,?State) is semidet.
%
%       Mapping between Action and State labels.
%
%       Completely arbitrary but set in stone here.
%
%       @tbd Maybe leave as a configurable option? Also, maybe also
%       allow mapping of State label to Observation label?
%
% Step actions:
state_mapping(step_up,q0).
state_mapping(step_up_right,q01).
state_mapping(step_right,q1).
state_mapping(step_down_right,q12).
state_mapping(step_down,q2).
state_mapping(step_down_left,q23).
state_mapping(step_left,q3).
state_mapping(step_up_left,q30).
% Look actions:
state_mapping(look_up,q0).
state_mapping(look_up_right,q01).
state_mapping(look_right,q1).
state_mapping(look_down_right,q12).
state_mapping(look_down,q2).
state_mapping(look_down_left,q23).
state_mapping(look_left,q3).
state_mapping(look_up_left,q30).



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
