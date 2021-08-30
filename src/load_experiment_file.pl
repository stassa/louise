:-module(load_experiment_file, [load_experiment_file/1
                               ]).


/** <module> Experiment file interface module.

This module serves as an interface module to experiment files with
different names, so that predicates defined in each can be called as
"experiment_file:<predicate_name>" rather than through that experiment
file module's name.

*/

%!      load_experiment_file(+Filename) is det.
%
%       Load a new experiment file, unloading the old one.
%
load_experiment_file(F):-
        load_files(F, [module(experiment_file)
                      ,redefine_module(true)
                      ]).
