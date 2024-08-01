:-prolog_load_context(directory, Dir)
,asserta(user:file_search_path(grid_master_root, Dir)).

user:file_search_path(grid_master_src, grid_master_root(src)).
user:file_search_path(grid_master_lib, grid_master_root(lib)).
user:file_search_path(grid_master_data, grid_master_root(data)).
user:file_search_path(grid_master_output, grid_master_root(output)).

:-ensure_loaded(grid_master_configuration).
:-ensure_loaded(grid_master_src(map)).
:-ensure_loaded(grid_master_src(action_generator)).
:-ensure_loaded(grid_master_src(map_display)).

%:-load_test_files([]).
%:-run_tests.

% Large data may require a larger stack.
%:-set_prolog_flag(stack_limit, 2_147_483_648).
%:- set_prolog_flag(stack_limit, 4_294_967_296).
%:-set_prolog_flag(stack_limit, 8_589_934_592).
%:-set_prolog_flag(stack_limit, 17_179_869_184).
:-current_prolog_flag(stack_limit, X)
 ,format('Global stack limit ~D~n',[X]).

% Large hypotheses may require large tables particularly for evaluation
% purposes
:-set_prolog_flag(table_space, 2_147_483_648).
%:-set_prolog_flag(table_space, 4_294_967_296).
%:-set_prolog_flag(table_space, 8_589_934_592).
%:-set_prolog_flag(table_space, 17_179_869_184).
%:-set_prolog_flag(table_space, 33_554_432_000).
%:-set_prolog_flag(table_space, 53_687_091_200).
:-current_prolog_flag(table_space, V)
 ,format('Table space ~D~n',[V]).

