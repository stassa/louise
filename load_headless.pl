:-prolog_load_context(directory, Dir)
,asserta(user:file_search_path(project_root, Dir)).

user:file_search_path(src, project_root(src)).
user:file_search_path(lib, project_root(lib)).
user:file_search_path(data, project_root(data)).
user:file_search_path(output, project_root(output)).

:-use_module(configuration).
:-use_module(src(louise)).
:-use_module(src(auxiliaries)).
:-use_module(lib(evaluation/evaluation)).
:-use_module(lib(sampling/sampling)).

%:-load_test_files([]).
%:-run_tests.

% Large data may require a larger stack.
%:-set_prolog_stack(global, limit(2**9*10**6)).
:-current_prolog_flag(stack_limit, V)
 ,format('Global stack limit ~D~n',[V]).
