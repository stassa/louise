:-module(reduction_configuration, [call_limit/1
				  ,depth_limit/1
				  ,derivation_depth/1
				  ,inference_limit/1
				  ,meta_interpreter/1
				  ,program_module/1
				  ,recursion_depth/1
				  ,resolutions/1
				  ,time_limit/1]).

:-dynamic derivation_depth/1
	 ,resolutions/1.

/** <module> Configuration options for the rest of the project.
*/

%!	call_limit(?Clause) is semidet.
%
%	Limitation placed on meta-interpreter calls.
%
%	Currently, one of:
%	* none: bare-bones call to the current meta-interpreter (i.e. no
%	call/2 variant wrapper is used to limit the call to the
%	meta-interpreter by depth, time or inferences).
%	*inference: limits processing by inferences, using Swi-Prolog's
%	inference-number bounded call/2 variant.
%	* depth: limits processing by recursion depth limit, using
%	Swi-Prolog's recursion-depth limit bounded call/2 variant.
%	* time: limits preocessing by time, using Swi-Prolog's
%	time-bounded call/2 variant.
%
%	Depth, time and inference limits are placed on the call to the
%	meta-interpreter selected in meta_interpreter/1. This call is
%	wrapped by the Swi-Prolog limited call/2 variant specified here,
%	each in a different subsumed/2 clause (for clarity, mostly,
%	although call_with_time_limit/2 requires special treatment
%	because it exits with an exception, whereas the other two bind
%	the exception term to the output variable instead).
%
call_limit(none).
%call_limit(inference).
%call_limit(depth).
%call_limit(time).

%!	recursion_depth(?Depth) is semidet.
%
%	Maximum recursion depth.
%
%	Used with call_with_depth_limit/3 built-in meta-interpreter.
%
depth_limit(10_000).

%!	recursion_depth(?Depth) is semidet.
%
%	Maximum depth of derivation branches.
%
%	Used with solve_to_depth/3 and solve_to_limit/4
%	meta-interpreters.
%
%derivation_depth(3).
%derivation_depth(5).
%derivation_depth(8).
derivation_depth(9).
%derivation_depth(10).
%derivation_depth(20).

%!	inference_limit(?Limit) is semidet.
%
%	Maximum number of inferences.
%
%	Used with call_with_inference_limit/2 built-in meta-interpreter.
%
inference_limit(10_000).

%!	meta_interpreter(?Name) is semidet.
%
%	The meta-interpreter to use in testing redundancy.
%
%	Currently, one of:
%	* call: No meta-interpreter; just use call/2.
%	* solve: Garden-variety meta-interpreter; allows unbounded
%	recursion.
%	* solve_to_depth: Iterative deepening meta-interpreter.
%	* solve_to_limit: As solve_to_depth but also limits the number
%	of recursion steps to a proof.
%
%meta_interpreter(call).
%meta_interpreter(solve).
meta_interpreter(solve_to_depth).
%meta_interpreter(solve_to_limit).

%!	program_module(?Module) is semidet.
%
%	Name of the Module where program clauses are asserted.
%
%	Used when adding or removing clauses to the dynamic database.
%	Also required to point meta-interpreters to the clauses of a
%	program tested for redundancy.
%
program_module(program).

%!	proof_length(?Depth) is semidet.
%
%	Maximum number of recursive steps.
%
%	Used with solve_to_limit/4.
%
recursion_depth(10).

%!	resolutions(?Resolutions) is semidet.
%
%	Maximum number of resolutions.
%
%	Used with solve_to_depth/3.
%
resolutions(5000).
%resolutions(1000).

%!	time_limit(?Seconds) is semidet.
%
%	Time limit for meta-interpreters.
%
%	Used with call_with_time_limit/2 built-in meta-interpreter.
%
time_limit(2).
