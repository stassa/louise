% Start in dark mode
%:-use_module(library(theme/dark)).
% Start in dork mode
%:-use_module((themes/dork)).

:- set_prolog_flag(encoding, utf8).

:-prolog_load_context(directory, Dir)
,asserta(user:file_search_path(project_root, Dir)).

user:file_search_path(src, project_root(src)).
user:file_search_path(subsystems, src(subsystems)).
user:file_search_path(lib, project_root(lib)).
user:file_search_path(data, project_root(data)).
user:file_search_path(output, project_root(output)).
user:file_search_path(scripts, data(scripts)).

:-doc_browser.

:-use_module(configuration).
:-use_module(src(louise)).
:-use_module(src(mil_problem)).
:-use_module(src(auxiliaries)).
:-use_module(subsystems(examples_invention)).
:-use_module(subsystems(metagen)).
:-use_module(subsystems(metarule_extraction)).
:-use_module(subsystems(toil)).
:-use_module(subsystems(minimal_program)).
:-use_module(subsystems(thelma/thelma)).
:-use_module(lib(evaluation/evaluation)).
:-use_module(lib(folding_unfolding/folding_unfolding)).
:-use_module(lib(sampling/sampling)).

edit_files:-
% Uncomment, or add, the paths of files you want to open in the IDE when
% the project loads, below.
	configuration:experiment_file(P,_)
	,edit(project_root(load_project))
	%,edit(project_root(load_headless))
	,edit(project_root(configuration))
	%,edit(src(mil_problem))
	,edit(src(louise))
	,edit(src(auxiliaries))
	%,edit(src(metarules_parser))
	%,edit(src(subhypothesis_selection))
	%,edit(subsystems(examples_invention))
	%,edit(subsystems(incremental_refinement))
%	,edit(subsystems(thelma/thelma))
	%,edit(subsystems(thelma/thelma_auxiliaries))
	%,edit(subsystems(thelma/thelma_configuration))
	%,edit(subsystems(metagen))
	%,edit(subsystems(toil))
	%,edit(subsystems(minimal_program))
	%,edit(lib(evaluation/evaluation))
%	,edit(lib(folding_unfolding/folding_unfolding))
	%,edit(lib(program_reduction/program_reduction))
	,edit(P)
	.
:-edit_files.

%:-load_test_files([]).
%:-run_tests.

% Large data may require a larger stack.
:- set_prolog_flag(stack_limit, 2_147_483_648).
%:- set_prolog_flag(stack_limit, 4_294_967_296).
%:-set_prolog_flag(stack_limit, 8_589_934_592).
%:-set_prolog_flag(stack_limit, 17_179_869_184).
:-current_prolog_flag(stack_limit, V)
 ,format('Global stack limit ~D~n',[V]).

% Large hypotheses may require large tables particularly for evaluation
% purposes
:-set_prolog_flag(table_space, 2_147_483_648).
%:-set_prolog_flag(table_space, 4_294_967_296).
%:-set_prolog_flag(table_space, 8_589_934_592).
%:-set_prolog_flag(table_space, 17_179_869_184).
:-current_prolog_flag(table_space, V)
 ,format('Table space ~D~n',[V]).
