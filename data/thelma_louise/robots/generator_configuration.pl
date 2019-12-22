:-module(generator_configuration, [experiment_world/1
				  ,world_dimensions/2
				  ,output_directory/1
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


%!	world_dimensions(?Width,?Height) is semidet.
%
%	Dimensions of a robot experiment world.
%
%	Used to generate examples for robot navigation plans.
%
world_dimensions(4,4).


%!	output_directory(?Root) is semidet.
%
%	Root directory for generated dataset module file.
%
%       Make sure that Root starts with '/' and ends with '/' otherwise
%       you will get errors.
%
output_directory('/worlds/').
