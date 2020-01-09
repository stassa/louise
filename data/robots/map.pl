:-module(world_map, [map_to_world/4
		    ,read_map/4
                    ]).

:-use_module(generator_configuration).
:-use_module(world).

/** <module> Create a grid world from a map.

*/

%!	map_to_world(+Map,+Name,?Initialised,-State) is det.
%
%	Convert a Map read from a file to an initial World state.
%
%       Map is a list of lists representing rows in a grid world. Name
%       is the name of the grid world (according to the worlds known to
%       world.pl). Initialised is an initial, empty state-list of that
%       world. And World is the same state-list but with the location of
%       each object as it is found in Map.
%
map_to_world(Ms,Wr,Ws,Ws_):-
	reverse(Ms, Ms_)
        ,map_to_world_(0,Ms_,Wr,Ws,Ws_)
        ,map_dimensions(Ms,Ws_).


%!      map_to_world_(+Height,+Map,+World,?Initial,-Instantiated) is
%!       det.
%
%	Instantiate an Initial world state-list with objects' locations.
%
%       Map is a list of lists representing rows of a grid world. Height
%       is the Y dimension in that world. World is the name of the
%       world, as known by world.pl. Initial is an initialised, empty
%       state-list for that world.
%
%       Instantiated is the same list as Initial but with the locations
%       of objects in the world filled in.
%
map_to_world_(_H,[],_Wr,Ws,Ws):-
	!.
map_to_world_(H,[Ls|Ms],Wr,Acc,Bind):-
	line_to_row(H,Ls,Wr,Acc,Acc_)
        ,succ(H,H_)
	,map_to_world_(H_,Ms,Wr,Acc_,Bind).


%!	line_to_row(+Height,+Row,+World,?Initial,-Instantiated) is det.
%
%	Initialise one row of a world's state-list.
%
%       Height is the Y dimension in a grid world. Row is one row in
%       the world. World is the name of the grid world. Initial is an
%       initial, empty state in the world and Instantiated is the same
%       state with the locations of objects in the present Row filled
%       in.
%
line_to_row(H,Ls,Wr,Ss,Ss_):-
	line_to_row_(0,H,Ls,Wr,Ss,Ss_).

%!      line_to_row_(+Width,+Height,+Line,+World,?Initial,-Instantiated)
%       is det.
%
%	Business end of line_to_row/5.
%
%       Initial plays the role of an accumulator here. It is updated
%       _non_ destructively. This is possible because it's partially
%       initialised to begin with.
%
%       @tbd Most likely, both Initial and Instantiated are not needed
%       and we can do the same with just one.
%
line_to_row_(_X,_Y,[],_Wr,Rs,Rs):-
	!.
line_to_row_(X,Y,[' '|Ls],Wr,Acc,Bind):-
	!
	,line_to_row_(X,Y,Ls,Wr,Acc,Bind).
line_to_row_(X,Y,[S|Ls],Wr,Acc,Bind):-
	symbol(floor,_,S)
        ,!
        ,succ(X,X_)
	,line_to_row_(X_,Y,Ls,Wr,Acc,Bind).
line_to_row_(X,Y,[S|Ls],Wr,Acc,Bind):-
	symbol(O,_,S)
	,object_location(Wr,Acc,O,X/Y)
        ,succ(X,X_)
	,line_to_row_(X_,Y,Ls,Wr,Acc,Bind).


%!      map_dimensions(+Map,?State) is det.
%
%       Insert a Width-Height term into an initial world State.
%
%       Map is a list of rows in a grid world. State is a partially
%       initialised state-list for that world, missing the dimensions
%       term at the end. This predicate binds the correct dimensions
%       term to State and sends it on its way.
%
map_dimensions(Ms,Ws):-
	length(Ms,H)
        ,Ms = [Rs|_]
        ,findall(1
                ,(member(S,Rs)
                 ,S \= ' '
                 )
                ,Ss)
        ,sumlist(Ss,W)
        ,W_ is W - 1
        ,H_ is H - 1
        ,last(Ws,W_-H_).



%!	read_map(+Filename,-Map,-World,-State) is det.
%
%	Read a grid world Map from a file.
%
%       Map is a list of lists: Rs = [R1,...,Rn] where each Ri is a row
%       in a grid world as read from the map file Filename.
%
%       World is the name of the world represented in Map and State is
%       an initialised, empty state-list for that world.
%
read_map(Fn,Ms,W,Ss):-
	open(Fn,read,S,[alias(world_map)])
	,G = read_map_lines(S,Ms,W,Ss)
	,C = close(S)
        ,call_cleanup(G,C).


%!	read_map_lines(+Stream,-Lines,-World,-State) is det.
%
%	Read a list of map Lines from a Stream.
%
read_map_lines(S,Ls,W,Ss):-
	read_line_to_string(S,W_)
        ,atom_string(W,W_)
	,read_map_lines_(S,[],Ls_)
	,maplist(string_chars,Ls_,Ls)
        ,world_state(W,Ss).

%!	read_map_lines_(+Stream,+Acc,-Lines) is det.
%
%	Business end of read_map_lines/2.
%
read_map_lines_(_S,[end_of_file|Acc],Ls):-
	reverse(Acc,Ls)
        ,!.
read_map_lines_(S,Acc,Bind):-
	read_line_to_string(S,L)
        ,read_map_lines_(S,[L|Acc],Bind).


%!	world_state(?World,?State) is semidet.
%
%	Initialise a World to a default, empty State.
%
world_state(empty_world,[_,_,_]).
world_state(simple_world,[_,_,_,false,_]).
world_state(obstacles_world,[_,_,_,_,false,_]).
world_state(rugby_world,[rugby_world,_,_,_,_,false,_]).

