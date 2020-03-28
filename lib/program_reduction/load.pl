:-prolog_load_context(directory, Dir)
  ,asserta(user:file_search_path(project_root, Dir)).

user:file_search_path(data, project_root(data)).

:-doc_browser.

:-use_module(reduction_configuration).
:-use_module(program_reduction).
:-use_module(clause_reduction).
:-use_module(reduction_auxiliaries).

edit_files:-
	edit(project_root(load))
	,edit(project_root(reduction_auxiliaries))
	,edit(project_root(meta_interpreters))
	,edit(project_root(clause_reduction))
	,edit(project_root(program_reduction))
	,edit(reduction_configuration)
	,edit(data(examples))
	.
:-edit_files.

% Uncomment to run tests on startup.
%:-['tests/program_reduction.plt'].
%:-run_tests.

% Print skolem constants in the tracer.
:- current_prolog_flag(debugger_write_options, X),
  set_prolog_flag(debugger_write_options, [numbervars(false)|X]).
