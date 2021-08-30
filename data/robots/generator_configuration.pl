:-module(generator_configuration, [experiment_world/1
				  ,world_dimensions/2
				  ,output_directory/1
                                  ,output_to/1
                                  ,symbol/3
                                  ,exported_moves/2
				  ]).

/** <module> Configuration parameters for move_generator.pl
*/


%!	experiment_world(?World) is semidet.
%
%	Name of the current experiment World.
%
%	Use to generate grids for positive examples.
%
experiment_world(empty_world).
%experiment_world(simple_world).
%experiment_world(obstacles_world).


%!	world_dimensions(?Width,?Height) is semidet.
%
%	Dimensions of a robot experiment world.
%
%	Used to generate examples for robot navigation plans.
%
%world_dimensions(2,2).
world_dimensions(4,4).


%!	output_directory(?Root) is semidet.
%
%	Name of Root directory for generated dataset file.
%
output_directory(worlds).


%!	output_to(?Where) is semidet.
%
%	Where to send rendering output.
%
%	One of: [console, log].
%
%	If "console", rendered states are output to the Swi-Prolog
%	top-level.
%
%	If "log", rendered states are output to the current debug stream
%	for the debug subject "robots".
%
%	Used by output/2 and output_nl/0 to decide where to output
%	rendered states.
%
output_to(console).
%output_to(log).


%!	symbol(?Object,+Holds_Ball,?Symbol) is semidet.
%
%	The Symbol used to represent an Object.
%
symbol(robot,false,#).
symbol(robot,true,$).
symbol(ball,_,@).
symbol(obstacle,_,o).
symbol(goal,_,x).
symbol(floor,_,.).


%!      experiment(?World,?Moves) is semidet.
%
%       Set of Moves exported by a World dataset.
%
exported_moves(empty_world,[move_right/2,move_left/2,move_up/2,move_down/2]).
exported_moves(simple_world,[move_right/2,move_left/2,move_up/2,move_down/2
                            ,pick_up/2,put_down/2]).
exported_moves(obstacles_world,[move_right/2,move_left/2,move_up/2,move_down/2
                               ,pick_up/2,put_down/2]).
