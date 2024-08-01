:-module(map, [new_map/3
              ,map_term/4
              ,load_map_files/0
              ,unload_all_maps/0
              ,load_map_file/2
              ,unload_map/1
              ,read_map_file/3
              ]).

/** <module> Predicates to read and write and store maps.

*/

:-grid_master_configuration:maps_module(Maps)
 ,dynamic(Maps:map/3).
:-grid_master_configuration:maps_module(Maps)
 ,multifile(Maps:map/3).


%!      new_map(+Dimensions,+Tile,-Map) is det.
%
%       Generate a Map filled with Tile and with the given Dimensions.
%
%       Dimensions is a pair Width-Height where Width and Height are
%       integers, from 1 upwards.
%
%       Tile is a constant, representing a map tile, i.e. the terrain
%       type at a cell in a maap.
%
%       Map is a list of Height elements each of which is a lists of
%       Width elements, each element filled in with the given Tile.
%
%       @tbd Copied from maze_generator, renamed from filled_maze. May
%       want to find a better place to put it.
%
%       Also needs maze_observations to be imported and that is not
%       going to work. Maybe this shold be in grid master? I don't think
%       controller freak needs to know anyhing about grids and maps etc.
%
new_map(W-H,T,M):-
        findall(Rs
               ,(between(1,H,_K)
                ,filled_path(W,T,Rs)
                )
               ,Ms_)
        ,maze_observations:lists_to_arrays([Ms_],[M]).


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



%!      map_term(+Id,+Ds,+M,-Term) is det.
%
%       Assemble a map Term from Id, Dimensions and a map array.
%
%       Helper to turn disparate map elements into a map/3 term.
%
map_term(Id,Ds,M,map(Id,Ds,M)):-
        grid_master_configuration:maps_module(Maps)
        ,Maps:map(Id,Ds,M)
        ,!.
map_term(Id,Ds,M,map(Id,Ds,M)).



%!      load_map_files is det.
%
%       Load a list of map files defined in the configuration.
%
%       As load_map_file/2, but the path of map files to be loaded is
%       taken from the grid_master_configuration option map_file/1.
%
%       The Id of each map file is taken from its base name, minus
%       extension.
%
load_map_files:-
        forall(grid_master_configuration:map_file(F)
              ,(path_module_name(F,N)
               ,load_map_file(F,N)
               )
              ).


%!      path_module_name(+Path,-Name) is det.
%
%       Derive a module Name from the Path to its source file.
%
path_module_name(F,B):-
        expand_file_search_path(F,E)
        ,file_base_name(E,N)
        ,file_name_extension(B,_Ext,N).



%!      unload_all_maps is det.
%
%       Remove all map/3 terms from memory NOW!
%
unload_all_maps:-
        grid_master_configuration:maps_module(Maps)
        ,forall(Maps:map(Id,_Ds,_M)
               ,unload_map(Id)
               ).



%!      load_map_file(+File,+Id) is det.
%
%       Load a map from a File to memory.
%
%       File is the path to a file map.
%
%       Id is the identifier for the map.
%
%       The map/3 term created from the map in the given file is
%       asserted to the dynamic database, with the given Id as its first
%       argument. Maps are asserted to the module named in
%       grid_master_configuration option maps_module/1.
%
%       @tbd Nothing stops multiple maps with the same id from being
%       loaded to memory. Consider disallowing.
%
load_map_file(F,Id):-
        grid_master_configuration:maps_module(Maps)
        ,read_map_file(F,Id,Map)
        ,assert(Maps:Map).



%!      unload_map(+Id) is det.
%
%       Remove a map from memory.
%
%       Id is the identifier for the map which should correspond to the
%       ide of a single map/3 term in the dynamic database.
%
%       The map/3 term with the given Id is retracted from the dynamic
%       database. Maps are stored in the module named in
%       grid_master_configuration option maps_module/1.
%
unload_map(Id):-
        grid_master_configuration:maps_module(Maps)
        ,Maps:map(Id,Ds,M)
        ,retract(Maps:map(Id,Ds,M)).



%!      read_map_file(+Path,-Map) is det.
%
%       Read a Map term from a File.
%
%       File should be a path to a file holding a map of a grid world in
%       any of the formats that Grid Master can understand.
%
%       Map is the map in File in Grid Master's internal representation
%       as a row-major order "array" (a compound Prolog term).
%
read_map_file(F,Id,map(Id,Ds,Ms)):-
        must_be(atomic,Id)
        ,parse_map_file(F,Ds,Ms).


%!      parse_map_file(+Path,-Dims,-Map) is det.
%
%       Read a map of a grid world from a file Path.
%
%       Path is the path to the file storing the rows and columns of a
%       map of a grid world.
%
%       Dims is a pair Width-Height, the width and height of the grid
%       world.
%
%       Map is a compound Prolog term storing the cells of the grid
%       world map as a row-major order "array".
%
parse_map_file(F,W-H,Ms):-
        read_lines(F,Ls)
        ,parse_lines(Ls,Ls_p)
        ,Ls_p = [L|_Ls]
        ,maplist(length,[L,Ls_p],[W,H])
        ,flatten(Ls_p,Ls_f)
        ,Ms =.. [c|Ls_f].


%!      read_lines(+File, -Lines) is det.
%
%       Read lines from a File until the end_of_file marker.
%
read_lines(F,Ls):-
        O = (expand_file_search_path(F,F_)
            ,open(F_,read,S,[alias(input_file)
                      ,close_on_abort(true)
                      ])
            )
        ,R = read_lines(S,[],Ls)
        ,C = close(S)
        ,setup_call_cleanup(O,R,C).

%!      read_lines(+Stream,+Acc,-Lines) is det.
%
%       Business end of read_lines/2.
%
read_lines(S,Acc,Bind):-
        read_line_to_codes(S,Cs)
        ,is_list(Cs)
        ,!
        ,read_lines(S,[Cs|Acc],Bind).
read_lines(S,Acc,Ls):-
        read_line_to_codes(S,end_of_file)
        ,reverse(Acc,Ls).


%!      parse_lines(+Lines,-Parsed) is det.
%
%       Parse Lines from a file into a list of characters.
%
parse_lines(Ls,Ps):-
        parse_lines(Ls, [], Ps).

parse_lines([], Acc, Ls):-
        reverse(Acc, Ls)
        ,!.
parse_lines([L|Ls], Acc, Bind):-
        phrase(map_file(S), L)
        ,!
        ,parse_lines(Ls,[S|Acc],Bind).
parse_lines([_L|Ls], Acc, Bind):-
        parse_lines(Ls, Acc, Bind).


map_file(Ss) --> syms(Ss).

% Mainly used to skip whitespace.
sp --> ` `, !.
sp --> `\t`, !.
sp --> `\n`, !.
sp --> [].


syms([A|Ss]) --> sp, [S], { atom_codes(A_,[S]), swap(A_,A) }, sp, syms(Ss).
syms([]) --> [].


swap(T,T_):-
        grid_master_configuration:tile_alias(T,T_)
        ,!.
swap(T,T).
