:-module(grid_master_configuration, [action/1
                                    ,action_representation/1
                                    ,display_engine/1
                                    ,maps_module/1
                                    ,map_file/1
                                    ,passable/1
                                    ,primitives_file/2
                                    ,test_primitives_file/2
                                    ,theme/1
                                    ,tile/4
                                    ,tile_alias/2
                                    ]).

/** <module> Configuration options for Grid Master.

*/
:-dynamic action/1
         ,action_representation/1
         ,primitives_file/2
         ,theme/1.


%!      action(?Name) is semidet.
%
%       The Name of an action to generate a definition for.
%
%       Used by action_generator module to choose what actions
%       to generate.
%
action(step_up).
%action(step_up_right).
action(step_right).
%action(step_down_right).
action(step_down).
%action(step_down_left).
action(step_left).
%action(step_up_left).
%action(look_up).
%action(look_up_right).
%action(look_right).
%action(look_down_right).
%action(look_down).
%action(look_down_left).
%action(look_left).
%action(look_up_left).


%!      action_representation(?Representation) is semidet.
%
%       Whether the state vector will have an action-stack or not.
%
%       Currently known representations:
%       * stack_based: the state vector has an action stack where action
%       tokens are pushed to each time an action is taken.
%       * memoryless: the state vector has no action stack.
%
%action_representation(list_based).
%action_representation(stack_less).
%action_representation(stack_based).
%action_representation(lookaround).
action_representation(controller_sequences).


%!      display_engine(?Engine) is semidet.
%
%       The display Engine to use in map_display.
%
%       @tbd Not currently used.
%
display_engine(prolog).
%display_engine(python).


%!      maps_module(?Module) is semidet.
%
%       Name of the module storing map/3 terms.
%
maps_module(maps).


%!      map_file(?Path) is semidet.
%
%       Path loading generated maze maps.
%
%       Path should be the path to a Prolog file with load directives
%       for the mazes generated by James' map generator (or any other
%       generator we end up using).
%
%       That file is loaded to import the generated maps into the
%       move_generator module, from where they are combined with
%       primitive moves and written to a new file, defined in
%       primitives_file/1.
%
map_file(grid_master_data(maps/'zero.map')).
map_file(grid_master_data(maps/'tessera_1.map')).
map_file(grid_master_data(maps/'tessera_2.map')).
map_file(grid_master_data(maps/'tessera_3.map')).
map_file(grid_master_data(maps/'tessera_4.map')).
map_file(grid_master_data(maps/'test_1.map')).
map_file(grid_master_data(maps/'room.map')).
map_file(grid_master_data(maps/'big_room.map')).
map_file(grid_master_data(maps/'hall_a.map')).
map_file(grid_master_data(maps/'hall_a_small.map')).


%!      passable(?Symbol) is semidet.
%
%       Symbol of a passable tile in a map.
%
%       Passable tiles can be traversed by an agent. Other tiles,
%       cannot.
%
passable(f).
passable(s).
passable(e).
passable(u).
passable(r).
passable(d).
passable(l).
passable(x).
passable(@). % Traversing
passable(a). % Patrolling landmark A
passable(b). % Patrolling landmark B
passable(c). % Patrolling landmark C
passable(g). % Patrolling landmark D


%!      primitives_file(?Path,?Module) is semidet.
%
%       Path to the Prolog Module holding primitive moves and maze maps.
%
%primitives_file(grid_master_data('primitives.pl'),primitives).
%primitives_file(grid_master_data('primitives_stack_less.pl'),primitives).
%primitives_file(grid_master_data('primitives_stack_based.pl'),primitives).
%primitives_file(grid_master_data('primitives_lookaround.pl'),primitives).
primitives_file(grid_master_data('primitives_controller_sequences.pl'),primitives).
%primitives_file(grid_master_data('primitives_list_based.pl'),primitives).


%!      test_primitives_file(?Path,?Module) is semidet.
%
%       Path and Module name for a file with primitives for testing.
%
test_primitives_file(grid_master_data('test_primitives.pl'),primitives).


%!      theme(?Theme) is semidet.
%
%       Theme for map and path printing.
%
%       Known themes:
%
%       * text: prints map in coloured text characters.
%       * boxes: prints map in coloured box drawings.
%
%       Example of 'text' theme (without the colours):
%       ==
%       ?- map_display:theme(T).
%       T = text.
%
%       ?- trace_path(0).
%       ˅ w f f f f f
%       ˅ w w w f w w
%       ˃ ˃ ˅ w f f f
%       w w ˅ w w w f
%       f w ˃ ˃ ˃ ˃ ˅
%       f w w w w w ˅
%       f f f f f f e
%       true.
%       ==
%
%       % Example of 'boxes' theme (without the colours):
%       ==
%       ?- map_display:theme(T).
%       T = boxes.
%
%       ?- trace_path(0).
%       ▼ ■ □ □ □ □ □
%       ▼ ■ ■ ■ □ ■ ■
%       ► ► ▼ ■ □ □ □
%       ■ ■ ▼ ■ ■ ■ □
%       □ ■ ► ► ► ► ▼
%       □ ■ ■ ■ ■ ■ ▼
%       □ □ □ □ □ □ █
%       true.
%       ==
%
%theme(text).
theme(boxes).


%!      tile(?Token,?Theme,?Style,?Symbol) is semidet.
%
%       Mapping between tiles and their printing elements.
%
%       Token is the symbol of a tile, or a primitive move.
%
%       Theme is the atomic identifier of a printing theme, as defined
%       in theme/1.
%
%       Style is a list of attributes passed to the second
%       argument of ansi_format/3, that determines how Tile will be
%       printed by print_tile/4.
%
%       Symbol is the symbol to be printed. This allows arbitrary
%       mapping between the representation of map tiles and move
%       symbols, and printing characters.
%
tile(s,text,[fg(cyan)],s).
tile(f,text,[fg(green)],f).
tile(w,text,[fg(red)],w).
tile(e,text,[bold,fg(green)],e).
tile(@,text,[fg(yellow)],@).
tile(u,text,[fg(yellow)],˄).
tile(d,text,[fg(yellow)],˅).
tile(r,text,[fg(yellow)],˃).
tile(l,text,[fg(yellow)],˂).
tile(x,text,[fg('#808080')],x).
tile(a,text,[bold,fg(yellow)],'A'). % Patrolling landmark
tile(b,text,[bold,fg(yellow)],'B'). % Patrolling landmark
tile(c,text,[bold,fg(yellow)],'C'). % Patrolling landmark
tile(g,text,[bold,fg(yellow)],'D'). % Patrolling landmark
tile(?,text,[bold,fg('#808080')],.). % Patrolling middle area
% ASCII box drawings
%tile(s,boxes,[fg(cyan)],█).
tile(s,boxes,[fg(cyan)],'S').
tile(f,boxes,[fg(green)],□).
tile(w,boxes,[fg(red)],■).
%tile(e,boxes,[bold,fg(green)],█).
tile(e,boxes,[bold,fg(green)],'E').
tile(@,boxes,[bold,fg(yellow)],@).
tile(u,boxes,[bold,fg(yellow)],▲).
tile(d,boxes,[bold,fg(yellow)],▼).
tile(r,boxes,[bold,fg(yellow)],►).
tile(l,boxes,[bold,fg(yellow)],◄).
tile(x,boxes,[bold,fg('#808080')],.).
tile(a,boxes,[bold,fg(yellow)],'A'). % Patrolling landmark
tile(b,boxes,[bold,fg(yellow)],'B'). % Patrolling landmark
tile(c,boxes,[bold,fg(yellow)],'C'). % Patrolling landmark
tile(g,boxes,[bold,fg(yellow)],'D'). % Patrolling landmark
tile(?,boxes,[bold,fg('#808080')],.). % Patrolling middle area
tile('X',boxes,[bold,fg(yellow)],'X').


%!      tile_alias(?Tile,?Alias) is semidet.
%
%       An Alias for a Tile.
%
%       Used to translate input maps to a desired notation. Only
%       applies when loading a map from a file.
%
tile_alias(#,w).
tile_alias(.,f).
%tile_alias(.,&).
