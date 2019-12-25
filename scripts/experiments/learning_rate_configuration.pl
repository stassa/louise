:-module(learning_rate_configuration, [logging_directory/1
                                      ,plotting_directory/1
                                      ,r_data_file/1
                                      ,time_limit/1
                                      ]).

/** <module> Configuration options for learning rate experiments.

*/


%!	logging_directory(?Directory) is semidet.
%
%	Directory to place learning rate experiment logs.
%
logging_directory('logs/learning_rate/').


%!	logging_directory(?Directory) is semidet.
%
%	Directory to place learning rate experiment logs.
%
plotting_directory('scripts/plotting/learning_rate/').


%!	r_data_file(?Path) is semidet.
%
%	Path to the output file for R plotting script data.
%
%	Paths is relative to the louise/ root directory.
%
r_data_file('learning_rate_data.r').


%!	time_limit(?Limit) is semidet.
%
%	Time Limit after which a learning attempt will fail.
%
time_limit(600).
