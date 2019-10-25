:-module(defaults, [default_extend_metarules/1
		   ,default_max_invented/1
		   ,default_recursion_depth_limit/1
		   ,default_recursive_reduction/1
		   ,default_reduction/1
		   ,default_resolutions/1
		   ,default_theorem_prover/1
		   ]).

/** <module> Default values for dynamic configuration options.

Predicates in this module store the default values for configuration
options that are declared dynamic so that they can be manipulated by
set_configuration_option/2. The auxilary predicate reset_defaults/0 sets
dynamic configuration options to the values specified in this file.
*/

%!	default_extend_metarules(?Default) is semidet.
%
%	Default value for extend_metarules/1 option.
%
%	The default value is "false".
%
default_extend_metarules(false).


%!	default_max_invented(?Default) is semidet.
%
%	Default value for max_invented/1 option.
%
%	The default value is "1".
%
default_max_invented(1).


%!	default_recursion_depth_limit(?Default) is semidet.
%
%	Default value for recursion_depth_limit/2 option.
%
%	The default value is "[dynamic_learning, 100]". Note that while
%	the configuration option takes two arguments, the default has a
%	single argument.
%
default_recursion_depth_limit([dynamic_learning, 100]).


%!	default_recursive_reduction(?Default) is semidet.
%
%	Default value for recursive_reduction/1 option.
%
%	The default value is "false".
%
default_recursive_reduction(false).


%!	default_reduction(?Default) is semidet.
%
%	Default value for reduction/1 option.
%
%	The default value is "plotkins".
%
default_reduction(plotkins).


%!	default_resolutions(?Default) is semidet.
%
%	Default value for resolutions/1 option.
%
%	The default value is "5000".
%
default_resolutions(5000).


%!	default_theorem_prover(?Default) is semidet.
%
%	Default value for theorem_prover/1 option.
%
%	The default value is "resolution".
%
default_theorem_prover(resolution).
