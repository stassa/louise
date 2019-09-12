:-module(generator_configuration, [experiment_world/1
				  ,world_dimensions/2
				  ,dataset_root_path/1
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
world_dimensions(2,2).


%!	dataset_root_path(?Path) is semidet.
%
%	Root Path of generated dataset module file.
%
dataset_root_path('data/robots/generator/'):-
	configuration:learner(louise)
	,!.
dataset_root_path('data/louise/robots/generator/'):-
	configuration:learner(thelma).

