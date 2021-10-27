:-module(auxiliaries, [% Experiment auxiliaries
	               protocol_experiment/3
		       % Configuration auxiliareis
		      ,debug_config/1
		      ,list_config/0
		      ,print_config/3
		      ,reset_defaults/0
		      ,set_configuration_option/2
		       % MIL Problem auxiliaries
		      ,hypothesis_language/1
		      ,hypothesis_language/2
		      ,hypothesis_language/4
		      ,invented_symbol/3
		      ,invented_symbol/2
		      ,invented_symbols/3
		      ,invented_symbols/2
		      ,known_metarules/1
		      ,predicate_signature/2
		      ,predicate_signature/3
		      ,background_predicate/2
		      ,same_metarule/2
		      ,write_problem/3
		      ,write_encapsulated_problem/1
		      ,write_encapsulated_problem/4
		       % Debugging auxiliaries
		      ,print_or_debug/3
		      ,list_encapsulated_problem/1
		      ,list_learning_results/0
		      ,list_mil_problem/1
		      ,list_problem_statistics/1
		      ,list_top_program_reduction/1
		      ,list_top_program/1
		      ,list_top_program/3
		      ,print_metarules/1
		      ,print_quantified_metarules/1
		      ,debug_quantified_metarules/2
		      ,debug_quantified_metarules/3
		       % Database auxiliaries
		      ,assert_program/3
		      ,erase_program_clauses/1
		      ,eraseall_program_clauses/1
		       % Experiment file auxiliaries
		      ,cleanup_experiment/0
		      ,experiment_data/5
		      ,initialise_experiment/0
	              ,learning_target/1
		      ,learning_targets/1
		      ,edit_experiment_file/0
		      ,tp_safe_experiment_data/5
		       % Program auxiliaries
		      ,built_in_or_library_predicate/1
		      ,closure/3
		      ,program_symbols/2
		      ,debug_clauses/3
		      ,debug_clauses/2
		      ,print_clauses/2
		      ,print_clauses/1
		      ,program/3
		      ,fold_recursive/2
	              % Timing auxiliaries
	              ,timing/2
		      ,timing/3
		      % Learning predicate auxiliaries
		      ,learning_query/5
		      ]).

:-use_module(project_root(configuration)).
:-user:use_module(lib(term_utilities/term_utilities)).
:-user:use_module(lib(program_reduction/program_reduction)).
:-user:use_module(lib(mathemancy/mathemancy)).
:-use_module(src/load_experiment_file).
:-use_module(src(mil_problem)).
:-use_module(src(defaults)).
:-use_module(src(louise)).

/** <module> Auxiliary predicates.

This module defines and exports predicates supporting other predicates
in the main project modules (louise, mil_problem and dynamic_learning),
or support the user with useful facilities for inspection and
manipulation of configuration and experiment files, or the elements of a
MIL problem loaded from an experiment file.

There are already quite a few predicates in this module and their number
should be expected to grow as the project is developed further. Perhaps
some should be moved to specialised libraries. However, to find a
balance between fragmentation of project source files and proliferation
of auxiliaries, the source code in this file is split into sections
grouping the definitions of predicates with similar functionality.

To facilitate browsing the source code in this module, the different
sections are listed in the ToC below. Each section is assigned a tag
made up of sec_ and a contracted section description, enclosed in square
braces. These tags can be used to jump to the relevant section by
searching the text for the text of a tag.

The ToC below can also help browsing the structured documentation,
rather than the source code, of predicates defined in this module. In
the Swi-Prolog documentation browser, the predicate indicators of
predicates listed under a section's title in the ToC should be rendered
as links that can be navigated-to in the user's browser, by clicking on
them.


Table of Contents
-----------------

1. Experiment auxiliaries [sec_prot]
   * protocol_experiment/3

2. Configuration auxiliaries [sec_config]
   * debug_config/1
   * list_config/0
   * print_config/3
   * reset_defaults/0
   * set_configuration_option/2

3. MIL problem auxiliaries [sec_prob]
   * invented_symbol/3
   * invented_symbol/2
   * invented_symbols/3
   * invented_symbols/2
   * known_metarules/1
   * predicate_signature/2
   * background_predicate/2
   * same_metarule/2
   * write_problem/3
   * write_encapsulated_problem/1
   * write_encapsulated_problem/4

4. Debugging auxiliaries [sec_debug]
   * print_or_debug/3
   * list_encapsulated_problem/1
   * list_learning_results/0
   * list_mil_problem/1
   * list_problem_statistics/1
   * list_top_program_reduction/1
   * list_top_program/1
   * list_top_program/3
   * debug_clauses/3
   * debug_clauses/2
   * print_clauses/2
   * print_clauses/1
   * print_metarules/1
   * print_quantified_metarules/1
   * debug_quantified_metarules/2
   * debug_quantified_metarules/3

5. Database auxiliaries [sec_dynm]
   * assert_program/3
   * erase_program_clauses/1

6. Experiment file auxiliaries [sec_expr]
   * cleanup_experiment/0
   * experiment_data/5
   * initialise_experiment/0
   * learning_target/1
   * learning_targets/1
   * edit_experiment_file/0
   * tp_safe_experiment_data/5

7. Program auxiliaries [sec_prog]
   * built_in_or_library_predicate/1
   * closure/3
   * program_symbols/2
   * debug_clauses/2
   * print_clauses/1
   * program/3
   * fold_recursive/2

8. Timing auxiliaries [sec_learn]
   * learning_query/5

*/


% [sec_prot]
% ================================================================================
% Experiment auxiliaries
% ================================================================================
% Predicates to manage experiments logged with protocol/1.


%!	protocol_experiment(+Target,+Protocol,+Goal) is det.
%
%	Run an experimental Goal and log results with protocol/1.
%
%	Target is a learning target in the current experiment file.
%
%	Protocol is the name of a file in which to save output of
%	executing Goal using protocol/1.
%
%	Goal is an arbitrary Prolog goal. It's expected that Goal has
%	something to do with Target.
%
%	Use this predicate to run quick, simple experiments keeping a
%	record of their configuration options, problem statisticts,
%	experiment Goal and result.
%
protocol_experiment(T,N,G):-
	protocol(N)
	,writeln('Current configuration:')
	,list_config
	,nl
	,writeln('Problem statistics:')
	,list_problem_statistics(T)
	,nl
	,copy_term(G,G_)
	,numbervars(G_)
	,writeln('Goal:')
	,print(G_)
	,nl
	,nl
	,writeln('Results:')
	,user:call(G)
	,noprotocol.




% [sec_config]
% ================================================================================
% Configuration auxiliaries
% ================================================================================
% Predicates for inspecting and manipulating configuration options.


%!	debug_config(+Subject) is det.
%
%	Log configuration options to the debug stream for Subject.
%
%	Alias for print_config(print,user_output,configuration).
%
%	Only configuration options actually defined in the configuration
%	module (i.e. not re-exported from other configuration files) are
%	logged.
%
debug_config(S):-
	print_config(debug,S,main).


%!	list_config is det.
%
%	Print configuration options to the console.
%
%	Alias for print_config(print,user_output,configuration).
%
%	Only configuration options actually defined in the configuration
%	module (i.e. not re-exported from other configuration files) are
%	printed.
%
list_config:-
	print_config(print,user_output,main).


%!	print_config(+Print_or_Debug,+Stream_or_Subject,+Scope) is det.
%
%	Print or debug current configuration options.
%
%	Print_or_Debug is one of [print,debug] which should be
%	self-explanatory.
%
%	Stream_or_Subject is either a stream alias or a debug subject,
%	depending on the value of Print_or_Debug.
%
%	Scope is one of [main,all]. If Scope is "main", only
%	configuration options whose implementation module is
%	"configuration" are printed. If Scope is "all", all
%	configuration options re-exported by configuration.pl are
%	printed, which includes options defined elsewhere, e.g.
%	configuration files of libraries that are re-exported by
%	configuration.pl to avoid cluttering it etc.
%
%	If Scope is "all" configuration options are prepended by the
%	name of their implementation module, to help identification.
%
%	If Scope is something other than "main" or "all", print_config/3
%	raised an existence error.
%
%	Configuration options are printed in alphabetical order, which
%	includes the alphabetical order of their implementation modules'
%	names.
%
print_config(T,S,Sc):-
	must_be(oneof([main,all]), Sc)
	,module_property(configuration, exports(Es))
	,findall(M:Opt_
		,(member(F/A,Es)
		 ,\+ memberchk(F/A, [metarule/2,metarule_constraints/2,tautology/1])
		 ,functor(Opt,F,A)
		 ,predicate_property(Opt, implementation_module(M))
		 ,call(configuration:Opt)
		 % Convert to list to sort by functor only.
		 % Standard order of terms also sorts by arity.
		 ,Opt =.. Opt_
		 )
		,Opts)
	% Sort alphabetically
	,sort(Opts, Opts_)
	,(   Sc = all
	 ->  true
	 ;   Sc = main
	 ->  Mod = configuration
	 )
	,forall(member(Mod:Opt, Opts_)
	       ,(Opt_ =.. Opt
		,(   Sc = all
		 ->  print_or_debug(T,S,Mod:Opt_)
		 ;   Sc = main
		 ->  print_or_debug(T,S,Opt_)
		 )
		)
	       ).



%!	reset_defaults is det.
%
%	Reset dynamic configuration options to their defaults.
%
%	This predicate should be called after dynamic configuration
%	options are changed dynamically with set_configuration_option/2
%	to reset the changed options to their defaults.
%
%	Defaults of dynamic configuration options are stored in the file
%	src(defaults).
%
%	Note that only options declared dynamic and whose implementation
%	module is configuration are reset.
%
%	Some options may be originally implemented in a different
%	module, but the configuration module may re-export them. Such
%	options are _not_ rest.
%
%	Some options might be originally implemented in a different
%	module, but the configuration module re-defines them and
%	excludes them from its re-export list. For example, that is the
%	case with the resolutions/1 option, originally defined in
%	reduction_configuration.pl and declared dynamic. Such options
%	_are_ reset to their defaults.
%
%	Consult the default.pl file for a definitive source of the
%	options that should be reset by this predicate.
%
reset_defaults:-
	module_property(configuration, exports(Es))
	,forall(member(F/A, Es)
	       ,(functor(P,F,A)
		,(  \+ memberchk(F, [learning_predicate
				    ,metarule
				    ,metarule_constraints])
		   ,predicate_property(P, dynamic))
		   ,predicate_property(P, implementation_module(configuration))
		->  atom_concat(default_,F,DF)
		   ,functor(D,DF,1)
		   ,call(defaults:D)
		   ,D =.. [_|Vs]
		   ,set_configuration_option(F,Vs)
		;  true
		)
	       ).



%!	set_configuration_option(+Option,+Value) is det.
%
%	Change the Value of a configuration Option.
%
%	Option is an atom, the name of a configuration option defined in
%	(or exported to) module configuration.
%
%	Value is the set of the arguments of Option. Iff Option has a
%	single argument, Value can be a single atomic constant.
%	Otherwise, it must be a list.
%
%	set_configuration_option/2 first retracts _all_ clauses of the
%	named Option, then asserts a new clause with the given Value.
%
%	Only configuration options declared as dynamic can be changed
%	using set_configuration_option/2. Attempting to change a static
%	configuration option will raise a permission error.
%
%	@tbd This predicate cannot change configuration options with
%	multiple clauses (or at least can't change any but their first
%	clause). Such functionality may or may not be necessary to add.
%
%	@bug Sorta bug, but if set_configuration_option/2 is used as
%	intended, at the start of an experiment file, to set a necessary
%	configuration option, the configuration option thus changed will
%	remain changed until the option is changed with
%	set_configuration_option/2 or by editing the configuration file.
%	And note that just reloading the configuration file will not
%	reset the option- it will just add an extra clause of it in the
%	database, which will often cause unepxected backtracking. This
%	may cause some confusion, for example when setting the value of
%	extend_metarules/1 to something other than false for one
%	experiment, which then of course affects subsequent experiments.
%	It happened to me, it could happen to you.
%
set_configuration_option(N, V):-
	atomic(V)
	,!
	,set_configuration_option(N,[V]).
set_configuration_option(N, Vs):-
	length(Vs, A)
	,functor(T,N,A)
	,T_ =.. [N|Vs]
	,retractall(configuration:T)
	,assert(configuration:T_).




% [sec_prob]
% ================================================================================
% MIL problem auxiliaries
% ================================================================================
% Predicates for inspecting and manipulating a MIL problem.


%!	hypothesis_language(+Ts) is det.
%
%	Generate and print the Hypothesis Language for a MIL problem.
%
hypothesis_language(Ts):-
	hypothesis_language(Ts,Hs)
	,print_clauses(Hs).



%!	hypothesis_language(+Ts,-Ls) is det.
%
%	Generate and print the Hypothesis Language for a MIL problem.
%
hypothesis_language(Ts,Hs):-
	experiment_data(Ts,Pos,_Neg,BK,MS)
	,hypothesis_language(Pos,BK,MS,Hs).



%!	hypothesis_language(+Pos,BK,MS,-Ls) is det.
%
%	Generate and print the Hypothesis Language for a MIL problem.
%
%	Pos, BK and MS are the positive examples, BK and metarules for a
%	MIL problem. Ls is a list of all instances of the metarules in
%	Ms constructible with predicate symbols and constants in Pos and
%	BK.
%
%	Ls is constructed by first generalising all examples in Pos, by
%	replacing each of their terms with variables. The result is a
%	set of atoms that are the most general atoms of their
%	respective predicaets. The set of generalised examples is then
%	passed to learn/5 with an empty negative examples list. This
%	generates all clauses that entail any atom of the target
%	predicates (because they entail the most-general atoms of those
%	target predicates).
%
hypothesis_language(Pos,BK,MS,Hs):-
	configuration:reduction(R)
	,configuration:resolutions(N)
	,generalised_examples(Pos,Gs)
	,S = (set_configuration_option(reduction,[plotkins])
	     ,set_configuration_option(resolutions,[0])
	     )
	,G = (learn(Gs,[],BK,MS,Hs_)
	     ,subtract(Hs_,Gs,Hs)
	     )
	,C = (retract(configuration:reduction(plotkins))
	     ,retract(configuration:resolutions(0))
	     ,set_configuration_option(reduction,[R])
	     ,set_configuration_option(resolutions,[N])
	     )
	,setup_call_cleanup(S,G,C).


%!	generalised_examples(+Examples,-Generalised) is det.
%
%	Generalise a set of Examples.
%
%	Examples are ground atoms of one or more target predicates.
%
%	Generalised is a list of atoms with the symbol and arities of
%	the atoms in Examples but with fresh variables in place of each
%	term.
%
%	For example:
%	==
%	?- generalised_examples([f(a,b), f(a,b,c,d), p(1,2,3,4)], Gs).
%	Gs = [f(_1722, _1724),f(_1830, _1832, _1834, _1836),p(_1962, _1964, _1966, _1968)].
%	==
%
generalised_examples(Es,Gs):-
	setof(E_
	     ,E^Es^F^A^(member(E,Es)
		       ,functor(E,F,A)
		       ,functor(E_,F,A)
		       ,numbervars(E_)
		       )
	     ,Gs_)
	,maplist(varnumbers,Gs_,Gs).



%!	invented_symbol(+Index,?Arity,?Symbol) is nondet.
%
%	An invented Symbol witn an index in [1,Index].
%
%	Symbol is a predicate indicator, S/A, where S is an invented
%	symbol and A is Arity.
%
%	Use this predicate to generate invented symbols of a given
%	Arity or verify that a Prolog term is an invented symbol of tha
%	Arity.
%
invented_symbol(I,A,S/A):-
	between(1,I,K)
	,atom_concat('$',K,S).



%!	invented_symbol(+Index,?Symbol) is nondet.
%
%	An invented Symbol witn an index in [1,Index].
%
%	As invented_symbol/2 but Symbol is an atomic term without an
%	arity.
%
%	Use this predicate to generate invented symbols or verify that
%	a Prolog term is an invented symbol.
%
invented_symbol(I,S):-
	between(1,I,K)
	,atom_concat('$',K,S).



%!	invented_symbols(+Index,?Arity,?Symbols) is det.
%
%	A list of invented Symbols up to some maximum Index.
%
%	Symbols is a list of symbols S/A, where each S is an invented
%	symbol and A is Arity.
%
%	Use this predicate to generate invented symbols of a given Arity
%	or verify that a list of Prolog terms is a list of invented
%	symbols of that Arity.
%
invented_symbols(I,A,Ss):-
	findall(S
	       ,invented_symbol(I,A,S)
	       ,Ss).



%!	invented_symbols(+Index,?Symbols) is det.
%
%	A list of invented Symbols up to some maximum Index.
%
%	As invented_symbols/3 but Symbols are atomic terms without
%	arities.
%
%	Use this predicate to generate invented symbols or verify that a
%	list of Prolog terms is a list of invented symbols.
%
invented_symbols(I,Ss):-
	findall(S
	       ,invented_symbol(I,S)
	       ,Ss).



%!	known_metarules(-Ids) is det.
%
%	Collect the Ids of metarules known to the system.
%
known_metarules(Ids):-
	setof(Id
	       ,H^Ps^B^(configuration:current_predicate(metarule, H)
			 ,H =.. [metarule,Id|Ps]
			 ,clause(H,B)
			 )
	     ,Ids).



%!	predicate_signature(+Targets,-Signature) is det.
%
%	Construct the predicate Signature given a learning Target.
%
%	Targets is a list of the predicate indicators of one or more
%	learning targets defined in the current experiment file (which
%	must be loaded). Targets can also be a single predicate
%	indicator.
%
%	Signature is a list of predicate indicators: [T1, T2, ..., I1,
%	..., In, B1, ..., Bm], where each Ti is in Targets, each Ii is
%	an invented symbol, n is the current value of max_invented/1,
%	and each Bi is the symbol of a predicate declared as background
%	knowledge for a learning target in Targets.
%
%	Each invented symbol is an atom '$I' where I is a numerical
%	index from 1 to the value of max_invented/1. The ordering if
%	invented symbols in Signature is according to their indices.
%
%	The ordering of predicate symbols in Signature is identical to
%	their order in the background_knowledge/2 declaration for
%	Target.
%
predicate_signature(Ts,Ss):-
	configuration:max_invented(I)
	,predicate_signature(Ts,I,Ss).


%!	predicate_signature(+Targets,+Invented,-Signature) is det.
%
%	As predicate_signature/2 but allows setting Invented max.
%
predicate_signature(T/A,I,Ss):-
	predicate_signature([T/A],I,Ss)
	,!.
predicate_signature(Ts,I,Ss):-
	setof(S
	      ,T^A^Ts^BK^(member(T/A,Ts)
			 ,experiment_file:background_knowledge(T/A,BK)
			 ,member(S,BK)
			 ,\+memberchk(S,Ts)
			 )
	      ,Bs)
	% Branching allows for 0 invented predicates.
	,(   setof(I_i
		  ,T^A^Bs^I^Is_i^(member(T/A,Bs)
				 ,invented_symbols(I,A,Is_i)
				 ,member(I_i, Is_i)
			     )
		  ,Is)
	 ->  true
	 ;   Is = []
	 )
	,sort(Ts,Ts_)
	,append(Ts_,Is,Ps)
	,append(Bs,Ps,Ss).



%!	background_predicate(?Target,?Predicate) is nondet.
%
%	True when Predicate is declared as a BK predicate for Target.
%
%	Target and Predicate are predicate indicators, F/A.
%
%	Mode (+,-) generates all predicates declared as BK for Target.
%	Mode (-,+) generates all Target prediates for which Predicate is
%	declared as BK.
%	Mode (-,-) nondeterministically generates all pairs of Target
%	and Predicate.
%
%	BK declarations are retrieved from the background_knowledge/2
%	clauses in the current experiment file. That means that an
%	experiment file must be loaded into memory (as normally done by
%	starting the project with a clause of experiment_file/2 in
%	configuration.pl).
%
%	@tbd This predicate includes all the symbols in the predicate
%	signature to the symbols in BK - that's most likely wrong,
%	although examples are also used as BK.
%
background_predicate(T,P):-
	predicate_signature(T,Ss)
	,memberchk(P,Ss).



%!	same_metarule(+Metarule1,+Metarule2) is det.
%
%	True when Metarule1 and Metarule2 are identical.
%
%	To compare the two metarules, first they are skolemised using
%	numbervars/1, then their head and body literals are compared.
%
%	This predicate is useful for debugging experiment files, in
%	particular when metarule extension is turned on. Extended
%	metarules can become too long to easily read and compare by the
%	naked eye.
%
%	Example of use
%	--------------
%	The query below compares metarules expanded from an initial set
%	of chain and inverse to the un-expanded chain metarule. The
%	chain metarule itself is in the output of expanded_metarules/2
%	so the query is true once.
%
%	==
%	?- _Sub1 = metarule(chain,_P,_Q,_R)
%	,clause(_Sub1, _MB), _M1 = (_Sub1:-_MB)
%	,expanded_metarules([chain,inverse], _Ms)
%	, member(_M2, _Ms), same_metarule(_M1, _M2), print_clauses([_M2,_M1]).
%	m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E).
%	metarule(chain,A,B,C):-configuration:(m(A,D,E),m(B,D,F),m(C,F,E)).
%	true ;
%	false.
%	==
%
same_metarule(M1,M2):-
	skolem_copy(M1,M1_)
	,skolem_copy(M2,M2_)
	,M1_ = M2_
	,!.
same_metarule(M1,M2):-
	skolem_copy(M1,M1_)
	,skolem_copy(M2,M2_)
	,metarule_head_body_name(M1_,Sub,B,Id)
	,metarule_head_body_name(M2_,Sub,B,Id).


%!	skolem_copy(+Term,-Skolemised) is det.
%
%	Copy and skolemise a Prolog Term.
%
%	Term is an arbitrary Prolog term and Skolemised is a copy of
%	this Term with variables replaced by numbered constants with
%	varnumbers/1. Skolemised is copied before passing it to
%	numbervars so the variables in Term remain unaffected by the
%	skolemisation.
%
skolem_copy(T,T_):-
	copy_term(T,T_)
	,numbervars(T_).


%!	metarule_head_body_name(+Metarule,-Metasubstitution,-Body,-Id) is det.
%
%	Extract a metarule's Metasubstitution and Body literals and Id.
%
metarule_head_body_name(M,Sub_,B_,Id):-
	M = (Sub:-B)
	,strip_module(B,_M,B_)
	,Sub =.. [_F,_|Ps]
	,Sub_ =.. [m,_|Ps]
	,Id = '$metarule'.



%!	write_problem(+Module,+Elements,-Refs) is det.
%
%	Write the Elements of a MIL problem to a Module.
%
%	Refs is a list of references of the clauses assserted to the
%	dynamic database. These are meant to be used later to erase the
%	asserted clauses.
%
write_problem(M,Es,Rs):-
	findall(Rs_i
		,(member(P, Es)
		 ,assert_program(M,P,Rs_i)
		 )
		,Rs_)
	,flatten(Rs_,Rs).



%!	write_encapsulated_problem(+Target) is det.
%
%	Write an encapsulated MIL problem to the dynamic db.
%
%	The MIL problem for Target is obtained from the current
%	experiment file.
%
write_encapsulated_problem(T):-
	experiment_data(T,Pos,Neg,BK,MS)
	,write_encapsulated_problem(Pos,Neg,BK,MS).



%!	write_encapsulated_problem(+Pos,+Neg,+BK,+Metarules) is det.
%
%	Write an encapsulated MIL problem to the dynamic db.
%
%	Writes to the dynamic database the encapsulated positive and
%	negative examples, BK and metarules given as inputs.
%
%	Useful for debugging purposes. Remember to call
%	cleanup_experiment/0 to remove clauses asserted to the dynamic
%	db.
write_encapsulated_problem(Pos,Neg,BK,MS):-
	encapsulated_problem(Pos,Neg,BK,MS,Es)
	,flatten(Es, Es_)
	,assert_program(user,Es_,_).




% [sec_debug]
% ================================================================================
% Debugging auxiliaries
% ================================================================================
% Predicates to facilitate experiment debugging and data inspection.


%!	print_or_debug(+Print_or_Debug,+Stream_or_Subject,+Atom) is
%!	det.
%
%	Print or debug an Atom.
%
%	Print_or_Debug can be one of: [print,debug,both]. If "print",
%	Stream_or_Subject should be the name or alias of a stream
%	and Atom is printed at that Stream. If "debug",
%	Stream_or_Subject should be a debug subject and Atom is printed
%	to the current debug stream, iff the specified subject is being
%	debugged. If "both", Stream_or_Subject should be a term Str/Sub,
%	where Str the name or alias of a stream and Sub the name of
%	debug topic; then Atom is printed to the specified stream and
%	also to the current debug topic if Sub is being debugged.
%
print_or_debug(debug,S,C):-
	debug(S,'~w',[C]).
print_or_debug(print,S,C):-
	format(S,'~w~n',[C]).
print_or_debug(both,Str/Sub,C):-
	print_or_debug(print,Str,C)
	,print_or_debug(debug,Sub,C).



%!	list_encapsulated_problem(+Target) is det.
%
%	Pretty-print the encapsulation of a MIL problem.
%
%	Target is the symbol and arity of the target predicate in the
%	encapsulated MIL problem to be listed.
%
list_encapsulated_problem(T):-
	experiment_data(T,Pos,Neg,BK,MS)
	,encapsulated_clauses(Pos,Pos_)
	,format_underlined('Positive examples')
	,print_clauses(Pos_)
	,nl
	,encapsulated_clauses(Neg,Neg_)
	,format_underlined('Negative examples')
	,print_clauses(Neg_)
	,nl
	,encapsulated_bk(BK,T,BK_)
	,expanded_metarules(MS,MS_)
	,format_underlined('Background knowledge')
	,forall(member(P,BK_)
	       ,print_clauses(P)
	       )
	,nl
	,format_underlined('Metarules')
	,forall(member(M,MS_)
	       ,(print_clauses(M)
		)
	       ).



%!	list_learning_results is det.
%
%	List results for all learning targets.
%
%	Prints to the console the results of training on each learning
%	target defined in the current experiment file.
%
%	Learning targets are obtained with a call to learning_targets/1.
%
%	By default, each learning target is passed to learn/1.
%	Alternatively, the user may declare a clause of the dynamic,
%	multifile predicate learning_predicate/1 to select a different
%	learning predicate.
%
%	Alternative learning predicates must be one of [learn_dynamic/1,
%	learn_with_examples_invention/2]. learn/2 can also be specified,
%	but it will have the same results as learn/1.
%
%	If a predicate with a symbol other than learn, learn_dynamic or
%	learn_with_examples_invention, or with arity other than 1 or 2
%	is specified, an informative error is raised.
%
%	@see learning_predicate/1, learning_targets/1
%
list_learning_results:-
	configuration:learning_predicate(P)
	,!
	,list_learning_results(P).
list_learning_results:-
	list_learning_results(learn/1).

%!	list_learning_results(+Learning_Predicate) is det.
%
%	Business end of list_learning_results/0.
%
%	Learning_Predicate is a predicate indicator, the symbol and
%	arity of one of the learning predicates in Louise.
%
%	Clauses are selected according to Learning_Predicate. Known
%	learning predicates with arity in [1,2] are called on all
%	learning targets and the results output to console. Predicates
%	with a symbol that is not one of the known learning predicates
%	or an arity other than an integer in [1,2], raise an appropriate
%	error.
%
list_learning_results(P/N):-
	\+ memberchk(P,[learn
		       ,learn_dynamic
		       ,learn_minimal
		       ,learn_with_examples_invention
		       ,learn_meta
		       ])
	,format(atom(A),'Unknown learning predicate: ~w',[P/N])
	,throw(A)
	% Actually needed to raise this error if the next also applies.
	,!.
list_learning_results(P/N):-
	\+ memberchk(N, [1,2])
	,format(atom(A),'Learning predicate arity must be in [1,2]: got ~w',[P/N])
	,throw(A).
list_learning_results(P/1):-
	!
	,learning_targets(Ts)
	,forall(member(T,Ts)
	       ,(call(P,T)
		,nl
		)
	       ).
list_learning_results(P/2):-
	learning_targets(Ts)
	,forall(member(T,Ts)
	       ,(call(P,T,Ps)
		,print_clauses(Ps)
		,nl
		)
	       ).



%!	list_mil_problem(+Target) is det.
%
%	List the elements of a MIL problem.
%
%	Target is the symbol and arity of the target predicate in the
%	MIL problem to be listed.
%
list_mil_problem(T):-
	experiment_data(T,Pos,Neg,BK,MS)
	,format_underlined('Positive examples')
	,print_clauses(Pos)
	,nl
	,format_underlined('Negative examples')
	,print_clauses(Neg)
	,nl
	,format_underlined('Background knowledge')
	,forall(member(P,BK)
	       ,(program(P,experiment_file,Ps)
		,format('~w:~n',[P])
		,print_clauses(Ps)
		,format('~n',[])
		)
	       )
	,format_underlined('Metarules')
	,print_quantified_metarules(MS).


%!	format_underlined(+Atom) is det.
%
%	Print an atom and underline it.
%
format_underlined(A):-
	atom_underline(A,A_)
	,format('~w~n',[A])
	,format('~w~n',[A_]).


%!	atom_underline(+Atom,-Underlined) is det.
%
%	Create an Underline for an Atom.
%
atom_underline(A,A_):-
	atom_length(A, N)
	,findall(-
		,between(1,N,_)
		,Ds)
	,atomic_list_concat(Ds,A_).



%!	list_problem_statistics(+Target) is det.
%
%	List statistics of the MIL problem for Target.
%
%	Currently this only lists the numbers of positive and negative
%	examples, background definitions and metarules in the initial
%	MIL problem for Target (i.e. before any automatic modifications
%	such as metarule extension).
%
list_problem_statistics(T):-
	experiment_data(T,Pos,Neg,BK,MS)
	,maplist(length,[Pos,Neg,BK,MS],[I,J,K,N])
	,format('Positive examples:    ~w~n', [I])
	,format('Negative examples:    ~w~n', [J])
	,format('Background knowledge: ~w ~w~n', [K,BK])
	,format('Metarules:            ~w ~w ~n', [N,MS]).



%!	list_top_program_reduction(+Target) is det.
%
%	List the top-program reduction step for a learning Target.
%
%	@tbd: This does no feedbacksies. So you only see the first step
%	of the reduction.
%
list_top_program_reduction(T):-
	experiment_data(T,Pos,Neg,BK,MS)
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,top_program(Pos_,Neg_,BK_,MS_,Ms)
	,flatten([Pos_,BK_,Ms,MS_],Ps_)
	,reduction_report(Ps_).



%!	list_top_program(+Target) is det.
%
%	Pretty-print the Top program for a Target predicate.
%
%	Same as list_top_program(Target,true,true).
%
list_top_program(T):-
	list_top_program(T,true,true).



%!	list_top_program(+Target,+Apply,+Excapsulate) is det.
%
%	Pretty-print the Top program for a Target predicate.
%
%	Apply is one of [true,false]. If true, the Top program is
%	applied to the corresponding metarules to expand it into a list
%	of definite clauses with Target (and any invented predicates) as
%	their predicate symbol(s) before printing. Otherwise it is printed as
%	a list of metasubstitutions.
%
%	Excapsulate is one of [true,false]. If true, the Top program is
%	excapsulated before printing. The Top program can't be
%	excapsulated unless it's applied, therefore
%	list_top_program(Target,false,true) will have the same effect as
%	list_top_program(Target,true,true).
%
list_top_program(T,U,E):-
	experiment_data(T,Pos,Neg,BK,MS)
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,write_problem(user,[Pos_,BK_],Refs)
	,generalise(Pos_,MS_,Ss_Pos_)
	,write_and_count(T,'Generalisation:',MS,Ss_Pos_,U,E)
	,specialise(Ss_Pos_,Neg_,Ss_Neg)
	,nl
	,erase_program_clauses(Refs)
	,write_and_count(T,'Specialisation:',MS,Ss_Neg,U,E).


%!	write_and_count(+Target,+Message,+Metasubs,+Apply,+Excapsulate)
%!	is det.
%
%	Auxiliary to list_top_program/2.
%
%	Pretty-print a set of Metasubs, its cardinality and a Message.
%	Apply and Excapsulate are booleans inherited from
%	list_top_program/2. Apply determines whether the Top program is
%	applied to its metasubstitutions to forma list of definite
%	clauses before printing. Excapsulate determines whether the Top
%	program is excapsulated before printing. The Top program can't
%	be excapsulated unless it's applied to its metarules.
%
write_and_count(T,Msg,MS,Cs,U,E):-
	(   \+ is_list(T)
	->  Ts = [T]
	;   Ts = T
	)
	,(   U = true
	->  expanded_metarules(MS, MS_)
	   ,applied_metarules(Cs, MS_, Cs_1)
	;   U = false
	->  Cs_1 = Cs
	% Else fail silently to flumox the user. Nyahahaha!
	)
	,(   E = true
	    ,U = true % Can't excapsulate applied meatasubs!
	 ->  excapsulated_clauses(Ts,Cs_1,Cs_2)
	 ;   Cs_2 = Cs_1
	 )
	,length(Cs_2, N)
	,format_underlined(Msg)
	,print_clauses(Cs_2)
	,format('Length:~w~n',[N]).



%!	debug_clauses(+Topic,+Message,-Clauses) is det.
%
%	Log a Message followed by a set of Clauses.
%
debug_clauses(T,M,Cs):-
	debug(T,'~w',[M])
	,debug_clauses(T,Cs).


%!	debug_clauses(+Topic,+Clauses) is det.
%
%	Debug a list of Clauses if Topic is being debugged.
%
debug_clauses(T,[]):-
	!
	,debug(T,'[]',[]).
debug_clauses(T,L):-
	\+ is_list(L)
	,!
	,debug_clauses(T,[L]).
debug_clauses(T,Cs):-
	forall(member(C,Cs)
	      ,(copy_term(C,C_)
	       ,numbervars(C_)
	       ,format(atom(A),'~W',[C_, [fullstop(true)
					 ,numbervars(true)
					 ,quoted(true)]
				    ])
	       ,debug(T,'~w',A)
	       )
	      ).



%!	print_clauses(+Message,-Clauses) is det.
%
%	Print a Message followed by a set of Clauses.
%
print_clauses(M,Cs):-
	format('~w~n',[M])
	,print_clauses(Cs).


%!	print_clauses(+Clauses) is det.
%
%	Print a list of Clauses to standard output.
%
print_clauses([]):-
	!
	,writeln([]).
print_clauses(L):-
	\+ is_list(L)
	,!
	,print_clauses([L]).
print_clauses(Cs):-
	forall(member(C,Cs)
	      ,(copy_term(C,C_)
	       ,numbervars(C_)
	       ,write_term(C_, [fullstop(true)
			       ,nl(true)
			       ,numbervars(true)
			       ,quoted(true)
			       ])
	       )
	      ).



%!	print_metarules(+Metarules) is det.
%
%	Pretty-print a list of Metarules.
%
%	Metarules may be a list of atomic metarule IDs, or a list of
%	expanded metarules, as returned by metarule_expansion/2 or
%	expanded_metarules/2.
%
%	The given metarules are printed at the top-level with variables
%	replaced by common predicate and variable names, e.g.
%	second-order existentially quantified variables are printed as
%	P, Q, R etc, whereas first-order existentialy or universally
%	quantified variables are printed as X, Y, Z etc. The names for
%	each type of variable are determined by the appropriate clause
%	of symbol_range/3.
%
%	Motivation
%	----------
%
%	This predicate is the recommended predicate to pretty-print
%	metarules at the top-level for the purpose of debugging a MIL
%	problem.
%
%	Metarules can also be printed to the top-level using
%	print_clauses/1, however that predicate assigns variable names
%	to metarule variables according to their order. This can make
%	metarules difficult to read especially for the user who has the
%	notation used in MIL literature in mind.
%
%	Example:
%       ==
%       ?- print_metarules([inverse,identity,chain]).
%       m(inverse,P,Q):-m(P,X,Y),m(Q,Y,X)
%       m(identity,P,Q):-m(P,X,Y),m(Q,X,Y)
%       m(chain,P,Q,R):-m(P,X,Y),m(Q,X,Z),m(R,Z,Y)
%       true.
%       ==
%
%	Note the difference with the alternative of printing metarules
%	using print_clauses/2:
%	==
%	?- expanded_metarules([inverse,identity,chain],_MS)
%	,print_clauses(_MS).
%
%	m(inverse,A,B):-m(A,C,D),m(B,D,C).
%	m(identity,A,B):-m(A,C,D),m(B,C,D).
%	m(chain,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E).
%	true.
%	==
%
print_metarules(M):-
	\+ is_list(M)
	,!
	,print_metarules([M]).
print_metarules(MS):-
	forall(member(M,MS)
	      ,(print_metarule(M)
	       ,nl
	       )
	      ).


%!	print_metarule(+Metarule) is det.
%
%	Pretty-print a Metarule at the top-level.
%
%	Metarule may be an atomic identifier for a metarule in the
%	program database, or an expanded metarule, as returned by
%	metarule_expansion/2.
%
print_metarule(Id):-
	atom(Id)
	,!
	,once(metarule_expansion(Id,M))
	,print_metarule(M).
print_metarule(MR):-
	must_be(nonvar, MR)
	,metarule_variables(MR,Es,Us)
	,length(Es,N)
	,numbered_symbols(N,Ps,predicate)
	,findall('$VAR'(P)
		,(nth1(I,Ps,P)
		 ,nth1(I,Es,'$VAR'(P))
		 )
		,Es)
	,length(Us,M)
	,numbered_symbols(M,Vs,variable)
	,findall('$VAR'(V)
		,(nth1(I,Vs,V)
		 ,nth1(I,Us,'$VAR'(V))
		 )
		,Us)
	,print(MR).


%!	metarule_variables(+Metarule,-Second_order,-First_order) is det.
%
%	Collect variables in a Metarule
%
%	Second_order is the list of second-order existentially
%	quantified variables in Metarule. First_order is the list of
%	first-order existentially and universally quantified variables
%	in Metarule.
%
%	The two output lists, Second_order and First_order only include
%	each variable once and the variables are in the order in which
%	they were originally in the metarule's literals. Unfortunately
%	this means that sort/2 cannot be used to remove duplicates
%	because it also imposes a new order on the variables in each
%	list, which may not be their original order. For this reason,
%	two auxiliaries, second_order/3 and first_order/3 go through
%	a list of variables each time one or more variables must be
%	added to it, and ensure that only new variables are added. This
%	of course is rather expensive (it's O(N^2)) so this is not a
%	predicate that should ever be in the transitive closure of a
%	heavy-lifting predicate.
%
%	@tbd Well, add this warning at print_metarule/1, no?
%
metarule_variables(_A:-M,Ss,Fs):-
	metarule_variables(M,[],Ss_,[],Fs_)
	,maplist(reverse,[Ss_,Fs_],[Ss,Fs]).

%!	metarule_variables(+Literals,+Ss_Acc,-Ss,+Fs_Acc,-Fs) is det.
%
%	Business end of metarule_variables/3.
%
metarule_variables((L,Ls),Ss_Acc,Ss_Bind,Fs_Acc,Fs_Bind):-
	! % Avoid backtracking here for more of the same.
	,L =.. [m,P|Fs]
	,second_order(P,Ss_Acc,Ss_Acc_)
	,first_order(Fs,Fs_Acc,Fs_Acc_)
	,metarule_variables(Ls,Ss_Acc_,Ss_Bind,Fs_Acc_,Fs_Bind).
metarule_variables(L,Ss_Acc,Ss,Fs_Acc,Fs):-
	L \= (_,_)
	,L =.. [m,P|Fs_L]
	,second_order(P,Ss_Acc,Ss)
	,first_order(Fs_L,Fs_Acc,Fs).


%!	second_order(+Variable,+Acc,-New) is det.
%
%	Add a Variable to an Accumulator if it is not already there.
%
second_order(P,Acc,[P|Acc]):-
	\+ in_vars(P,Acc)
	,!.
second_order(_P,Acc,Acc).


%!	first_order(+Variables,+Acc,-New) is det.
%
%	Add a list of Variables to an Accumulator if not already there.
%
first_order([],Fs,Fs):-
	!.
first_order([V|Fs],Acc,Bind):-
	\+ in_vars(V,Acc)
	,!
	,first_order(Fs,[V|Acc],Bind).
first_order([_V|Fs],Acc,Bind):-
	first_order(Fs,Acc,Bind).


%!	in_vars(?Variable,?Variables) is det.
%
%	True when a Variable is in a list of Variables.
%
%	Version of memeber/2 that avoids unfiying Variable with every
%	other variable in Variables and thereby making an awful
%	mish-mashed mess of unexpectedly identical variables. We need to
%	preserve Variables and their bindings throughout the project.
%
%	@tbd This is copied verbatim from metagen module. Maybe add it
%	to er, this module and make public? Or perhaps in
%	library(term_utilities)?
%
in_vars(V,[V1|_Vs]):-
	V == V1
	,!.
in_vars(V,[_|Vs]):-
	in_vars(V,Vs).



%!	numbered_symbols(+N,-Symbols,+Type) is det.
%
%	Generate a list of N predicate Symbols of the given Type.
%
%	N is an integer, the number of Symbols to generate.
%
%	Type is one of [predicate, variable], denoting whether Symbols
%	should be predicate symbols or variable names.
%
%	Symbols is a list of atoms representing predicate or variable
%	symbols to be used as the names of variables in a metarule when
%	pretty-printing the metarule. Predicate symbols are assigned to
%	second-order existentially quantified variables and variable
%	symbols to first-order universally and existentially quantified
%	variables.
%
%	Generating predicate symbols
%	----------------------------
%
%	When T = predicate, Symbols is a list of atomic predicate
%	symbols to be assigned to the second-order existentially
%	quantified variables of a metarule.
%
%	Predicate symbols are given names from an initial set of M
%	symbols, Ns, generated by symbol_range/3 when its first agrument
%	is the atom 'predicate'. M is the "symbol range" i.e. the
%	cardinality of Ns. When more than M symbols are generated, each
%	symbol afte the first M is indexed by a number, according to how
%	many times the same symbol has been used before.
%
%	As an example of how numbers are assigned to predicate symbols,
%	suppose that Ns = [P,Q,R,S,T] so M = 5.
%
%	Then, for N in [1,5] predicate symbols are given the names in Ns
%	so that the first element in Symbols is 'P', the second is 'Q',
%	etc.
%
%	For N > 6, predicate symbols are given the names in Ns, numbered
%	with the division of 5 in which N falls. For example, the N's in
%	[6,10] fall within the first division of 5, the N's in [11,15]
%	fall in the second division of 5, etc.
%
%	This is illustrated with an example, below.
%
%	Example:
%	==
%	% Generating predicate symbols for varying N.
%	% Assuming:
%	% symbol_range(predicate, ['P','Q','R','S','T'], 5).
%
%	?- auxiliaries:predicate_symbols(5, S).
%	S = ['P', 'Q', 'R', 'S', 'T']. % N in 0'th division of 5
%
%	?- auxiliaries:predicate_symbols(10, S).
%	S = ['P', 'Q', 'R', 'S', 'T'  % N in 'th division of 5
%	,'P1', 'Q1', 'R1', 'S1', 'T1' % N in 1st division of 5
%	].
%
%	?- auxiliaries:predicate_symbols(15, S).
%	S = ['P', 'Q', 'R', 'S', 'T'  % N in 0'th division of 5
%	,'P1', 'Q1', 'R1', 'S1', 'T1' % N in 1st division of 5
%	,'P2', 'Q2', 'R2', 'S2', 'T2' % N in 2nd division of 5
%	].
%
%	?- auxiliaries:predicate_symbols(20, S).
%	S = ['P', 'Q', 'R', 'S', 'T'  % N in 0'th division of 5
%	,'P1', 'Q1', 'R1', 'S1', 'T1' % N in 1st division of 5
%	,'P2', 'Q2', 'R2', 'S2', 'T2' % N in 2nd division of 5
%	,'P3', 'Q3', 'R3', 'S3', 'T3' % N in 3d division of 5
%	].
%
%	% etc
%	==
%
%	Generating variable names
%	-------------------------
%
%	When T = variable, Symbols is a list of atomic variable names to
%	be assigned to the first-order existentially and universally
%	quantified variables of a metarule.
%
%	Variable symbols are given names from an initial set of M
%	symbols, Ns, generated by symbol_range/3 when its first argument
%	is the atom 'variable'. As for predicate symbols, M is the
%	symbol range for variables, i.e. the cardinality of Ns. When
%	more than M symbols are generated, each symbol after the first M
%	is indexed by a number according to how many times that symbol
%	has been used before.
%
%	As an example of how numbers are assigned to variable symbols,
%	suppose that Ns = [X,Y,Z] and M = 3.
%
%	Then, for N in [1,3], variable symbols are given the names in Ns
%	so that the first element in Symbols is 'X', the second 'Y' and
%	the third is 'Q'.
%
%	For N > 3, variable symbols are given the names in Ns, numbered
%	with the division of 3 in which N falls. For example, the N's in
%	[4,6] fall in the first division of 3, the N's in [7,9] fall in
%	the second division of 3, etc.
%
%	This is illustrated with an example, below.
%
%	Example:
%	==
%	% Generating variable symbols for varying N.
%	% Assuming:
%	%symbol_range(variable, ['X','Y','Z'], 3).
%
%	?- auxiliaries:numbered_symbols(3, S, variable).
%	S = ['X', 'Y', 'Z']. % N in 0'th division of 3
%
%	?- auxiliaries:numbered_symbols(6, S, variable).
%	S = ['X', 'Y', 'Z' % N in 0'th division of 3
%	,'X1', 'Y1', 'Z1'  % N in 1st division of 3
%	].
%
%	?- auxiliaries:numbered_symbols(9, S, variable).
%	S = ['X', 'Y', 'Z' % N in 0'th division of 3
%	,'X1', 'Y1', 'Z1'  % N in 1st division of 3
%	,'X2', 'Y2', 'Z2'  % N in 2nd division of 3
%	].
%
%	?- auxiliaries:numbered_symbols(12, S, variable).
%	S = ['X', 'Y', 'Z' % N in 0'th division of 3
%	,'X1', 'Y1', 'Z1'  % N in 1st division of 3
%	,'X2', 'Y2', 'Z2'  % N in 2nd division of 3
%	,'X3', 'Y3', 'Z3'  % N in 3d division of 3
%	].
%
%	% etc.
%	==
%
numbered_symbols(N,Ss,T):-
	% Off-by-one correction for symbols' indices.
	succ(N, N_)
	,numbered_symbols(T,1,N_,[],Ss).

%!	numbered_symbols(+Type,+Current,+Max,+Acc,-Symbols) is det.
%
%	Business end of numbered_symbols/3.
%
numbered_symbols(_T,N,N,Acc,Ss):-
	!
	,reverse(Acc, Ss).
numbered_symbols(T,I,K,Acc,Bind):-
	once(numbered_symbol(T,I,S))
	,succ(I,I_)
	,numbered_symbols(T,I_,K,[S|Acc],Bind).


%!	numbered_symbol(+Type,+Index,-Symbol) is nondet.
%
%	Generate a Symbol of the given Type.
%
%	Index is the index of the symbol in the list of symbols
%	generated by numbered_symbols/3. Index is used to determine
%	whether, and how, to number Symbol to distinguish it from other
%	symbols with the same letter generated so far. See
%	numbered_symbols/3 for an explanation of how symbol numbering
%	works.
%
numbered_symbol(T,I,S):-
	symbol_range(T,Ss,_M)
	,nth1(I,Ss,S)
	,!.
numbered_symbol(T,I,S):-
	symbol_range(T,Ss,M)
	,% K'th division of M, counting from 0
	K is ceiling(I/M) - 1
	% I_'th item in K'th division of M
	, I_ is I - M * K
	,nth1(I_,Ss,S_)
	% Or:
	%,numbered_symbol(T,I_,S_)
	,atomic_list_concat([S_,K],'',S).


%!	symbol_range(+Type,-Symbols,-N) is det.
%
%	The list of Symbols for a Type.
%
%	Type is one of [predicate,variable], denoting the type of
%	symbols in the currenr range.
%
%	Symbols is a list of symbols of the given Type. The value of
%	Symbols is taken from the configuration option symbol_range/2.
%
%	N is the number of symbols in Symbols. This is used by
%	numbered_symbol/3 to index symbols accordig to how many times
%	they have already been used.
%
symbol_range(T,Ss,N):-
	configuration:symbol_range(T,Ss)
	,length(Ss,N).



%!	print_quantified_metarules(+Metarules) is det.
%
%	Pretty-print a list of Metarules with quantifiers.
%
%	Metarules is a list of encapsulated metarules, or a list of
%	atomic metarule IDs.
%
print_quantified_metarules(M):-
	\+ is_list(M)
	,!
	,print_quantified_metarules([M]).
print_quantified_metarules(MS):-
	forall(member(M,MS)
	      ,output_quantified_metarule(print,user_output,M)
	      ).



%!	debug_quantified_metarules(+Subject,+Message,+Metarules) is det.
%
%	Log a message followed by a pretty-printed list of Metarules.
%
%	Subject is the name of a debug subject.
%
%	Message is a string that wll be printed above the list of
%	quantified Metarules when logging Subject.
%
%	Metarules is a list of encapsulated metarules or metarule IDs
%	that will be pretty-printed when logging Subject.
%
debug_quantified_metarules(S,M,MS):-
	debug(S,'~w',M)
	,debug_quantified_metarules(S,MS).



%!	debug_quantified_metarules(+Subject,+Metarles) is det.
%
%	Log a list of pretty-printed Metarules.
%
%	Subject is the name of a debug subject.
%
%	Metarules is a list of metarule IDs or encapsulated metarules
%	that wll be pretty-printed when logging Subject.
%
debug_quantified_metarules(S,M):-
	\+ is_list(M)
	,!
	,debug_quantified_metarules(S,[M]).
debug_quantified_metarules(S,MS):-
	forall(member(M,MS)
	      ,output_quantified_metarule(debug,S,M)
	      ).



%!	print_quantified_metarule(+How,+Where,+Metarule) is det.
%
%	Pretty-print a Metarule with quantifiers.
%
%	How is one of "print" or "debug". Where is the name of the
%	stream to print to, or the debug subject.
%
output_quantified_metarule(H,W,higher_order(Min,Max)):-
% Pretty-print third-order metarules.
	!
	,findall(M_
	       ,(between(Min,Max,N)
		,length(Ps,N)
		,variables_symbols(predicate,Ps)
		,once(list_tree(Ps,Vs))
		,(   Vs = (Hd,B)
		 ->  format(atom(M),'~w \u2190 ~w',[Hd,B])
		 ;   M = Vs
		 )
		,maplist(arg(1),Ps,Es)
		,atomic_list_concat(Es,',',Es_)
		,format(atom(Es_A),'\u2203.~w',[Es_])
		,format(atom(M_),'(TOM-~w) ~w: ~w',[N,Es_A,M])
		)
	       ,Hs)
	,forall(member(Hom,Hs)
	       ,print_or_debug(H,W,Hom)
	       ).
output_quantified_metarule(H,W,Id):-
	atom(Id)
	,!
	,once(metarule_expansion(Id,M))
	,output_quantified_metarule(H,W,M).
output_quantified_metarule(H,W,M):-
	metarule_symbols(M)
	,excapsulated_metarule(M,M_)
	,metarule_Id(M, Id)
	,metarule_quantifiers(M,Es,Us)
	,pretty_metarule_id(Id,Id_)
	,format(atom(A),'(~w) ~w~w: ~w',[Id_,Es,Us,M_])
	,print_or_debug(H,W,A).


%!	metarule_symbols(+Metarule) is det.
%
%	Assign common names to the variables in a Metarule.
%
%	This is a very poorly named predicate but I couldn't really find
%	anything better. What it does is that it binds the variables in
%	Metarule to appropriate symbols, depending on whether they're
%	first- or second-order and existentially or universally
%	quantified. "Appropriate" symbols then are the symbols that are
%	typically assigned to the relevant kind of variable in the MIL
%	literature.
%
%	The specific symbols used are defined by the user in the
%	configuration option symbol_range/2 however this predicate
%	enforces lower-casing of first-order, universally quantified
%	variables. However, capitalisation of any-order existentially
%	quantified variables is not enforced.
%
%	@bug The decision to not force capitalisation of existentially
%	quantified variables might well lead to ugly results if the user
%	chooses a symbol_range/2 that doesn't agree with this scheme.
%	e.g. a symbol range for variables where all variables are lower-
%	case, like symbol_range(variable, [x,y,z]) will cause
%	existentially quantified first-order variables to be printed in
%	lower-case.
%
metarule_symbols(M):-
	metarule_variables(M,Ss,Fs)
	,variables_symbols(predicate,Ss)
	,variables_symbols(variable,Fs)
	,existential_vars(M,Es)
	,quantification_case(Es,Fs).


%!	variables_symbols(+Type, ?Variables) is det.
%
%	Bind Variables to appropriate symbols of the given Type.
%
variables_symbols(T,Vs):-
	length(Vs,N)
	,numbered_symbols(N,Ps,T)
	,findall('$VAR'(P)
		,(nth1(I,Ps,P)
		 ,nth1(I,Vs,'$VAR'(P))
		 )
		,Vs).


%!	existential_vars(+Metarule, -Existential) is det.
%
%	Collect Existentially quantified variables from a Metarule.
%
existential_vars(A:-_M,Es):-
	A =.. [m,_Id|Es].


%!	quantification_case(+Existential,+First_Order) is det.
%
%	Ensure variables' symbols are the appropriate case.
%
%	Existential is the list of existentially quantified variables in
%	a metarule, which may include first- and second-order variables.
%	First_Order is a list of the first-order variables in a
%	metarule. Both lists of variables are already ground to
%	'$VAR'(S) terms where S is a meaningful symbol depending on the
%	variable's order and quantification.
%
%	What this predicate does then is to downcase first-order,
%	universally quantified variables, which are the variables in
%	First_Order that are not also in Existential.
%
%	@tbd This predicate is very, very naughty. Because Existential
%	and First_Order are both lists of "Skolem" terms, like
%	'$VAR'(S), and in order to avoid having to re-construct the
%	metarule thse variables have come from, it uses arg/3 and
%	nb_setarg/3 to modify the symbols in '$VAR'(S) terms where
%	appropriate, by downcasing them.
%
quantification_case(_,[]):-
	!.
quantification_case(Es,[V|Fs]):-
	\+ memberchk(V,Es)
	,!
	,arg(1,V,S)
	,downcase_atom(S,S_)
	,nb_setarg(1,V,S_)
	,quantification_case(Es,Fs).
quantification_case(Es,[_V|Fs]):-
	quantification_case(Es,Fs).


%!	excapsulated_metarule(+Metarule,-Atomic) is det.
%
%	Excapsulate a metarule into an Atomic representation.
%
%	Metarule is an expanded metarule with variables bound '$VAR'(S)
%	terms where each S is an appropriate name for a variable
%	depending on its order and quantification.
%
%	Atomic is the same metarule in a second-order representation,
%	although at this point not yet with quantifiers. The
%	representatin is an atom, because Prolog cannot represent
%	second-order terms otherwise.
%
excapsulated_metarule((_A:-M),A):-
	clause_literals(M,Ls)
	,excapsulated_literals(Ls,[],Ls_)
	,once(list_tree(Ls_,C))
	,(   C = (H,B)
	->   format(atom(A),'~w\u2190 ~w',[H,B])
	     % Unit clause metarule.
	 ;   format(atom(A),'~w\u2190',[C])
	 ).


%!	excapsulated_literals(+Literals,+Acc,-Excapsulated) is det.
%
%	Excapsulate each of the Literals of an expanded metarule.
%
excapsulated_literals([],Acc,Ls):-
	!
	,reverse(Acc,Ls).
excapsulated_literals([L|Ls],Acc,Bind):-
	L =.. [m|As]
	,findall(S
		,member('$VAR'(S),As)
		,Ss)
	,S_ =.. Ss
	,excapsulated_literals(Ls,[S_|Acc],Bind).


%!	metarule_Id(+Metarule, -Id) is det.
%
%	Simple helper to extract metarule identifiers from metarules.
%
metarule_Id(A:-_M,Id):-
	A =.. [m,Id|_Ps].


%!	metarule_quantifiers(+Metarule,-Existential,-Universal) is det.
%
%	Collect quantifiers and quantified variables in a Metarule.
%
%	Metarule is an expanded metarule.
%
%	Existential is an atomic represention of the existentially
%	quantified variables in Metarule, preceded by an existential
%	quantifier, in the form \exists.P,Q,R... etc.
%
%	Univerasl is a film studio in Hollywood. Also, in this
%	predicate, it's an atomic representation of the universally
%	quantified variables in Metarule, preceeded by a universal
%	quantifier, in the form \forall.X,Y,Z... etc.
%
metarule_quantifiers(M,Es,Us):-
	metarule_variables(M,_Ss,Fs)
	,existential_vars(M,Es_)
	,subtract(Fs,Es_,Us_)
	,maplist(arg(1),Es_,Ss_E)
	,maplist(arg(1),Us_,Ss_U)
	,atomic_list_concat(Ss_E,',',Es_1)
	,atomic_list_concat(Ss_U,',',Us_1)
	,format(atom(Es),'\u2203.~w',[Es_1])
	,(   Ss_U = [_]
	 ->  format(atom(Us),' \u2200~w',[Us_1])
	 ;   Ss_U = [_|_]
	 ->  format(atom(Us),' \u2200.~w',[Us_1])
	 ;   Ss_U = []
	 ->  Us = ''
	 ).


%!	pretty_metarule_id(+Id,-Pretty) is det.
%
%	Prettify a metarule Id.
%
%	The prettified Id has its first letter up-cased and each
%	underscore turned to a hyphen.
%
%	@tbd  Other prettifications may need to be added later.
%
pretty_metarule_id(Id,Id_):-
	atom_chars(Id, [C|Cs])
	,upcase_atom(C,C_)
	,findall(Ck
		,(member(Ci,Cs)
		 ,(   Ci == '_'
		  ->  Ck = -
		  ;   Ck = Ci
		  )
		 )
		,Cs_)
	,atom_chars(Id_,[C_|Cs_]).




% [sec_dynm]
% ================================================================================
% Database auxiliaries
% ================================================================================
% Predicates for manipulating the Prolog dynamic database.
% Remember: the dynamic database is evil.


%!	assert_program(+Module,+Program,-Clause_References) is det.
%
%	As assert_program/2 but also binds a list of Clause_References.
%
assert_program(M,Ps,Rs):-
	assert_program(M,Ps,[],Rs).

assert_program(_,[],Rs,Rs):-
	!.
assert_program(M,[A|P],Acc,Bind):-
	copy_term(A,A_)
	,numbervars(A_)
	,clause(M:A_,true)
	,!
	,assert_program(M,P,Acc,Bind).
assert_program(M,[C|P],Acc,Bind):-
	copy_term(C,H:-B)
	,numbervars(H:-B)
	,clause(M:H,B)
	,!
	,assert_program(M,P,Acc,Bind).
assert_program(M,[C|P],Acc,Bind):-
	assert(M:C,Ref)
	,assert_program(M,P,[Ref|Acc],Bind).



%!	erase_program_clauses(-Clause_References) is det.
%
%	Erase a list of Clause_References from the dynamic database.
%
%	Clause_References is meant to be a list of references of a
%	program's clauses asserted to the dynamic database with
%	assert_program/3.
%
%	The purpose of this predicate is to allow a set of clauses
%	previously asserted by invoking assert_program/3 to be removed
%	from the dynamic database without stumbling over module scoping
%	that can be complicated when a predicate is declared in one
%	module and then clauses of it are added in another module.
%
%	For example, the following is what you should expect to see in
%	the dynamic database after a theory of father/2 is learned and
%	asserted in the dynamic database, while there is also background
%	knowledge of father/2:
%
%	==
%	% Example copied from Thelma.
%	[debug] [1]  ?- listing(thelma:father/2).
%	:- dynamic tiny_kinship:father/2.
%
%	tiny_kinship:father(stathis, kostas).
%	tiny_kinship:father(stefanos, dora).
%	tiny_kinship:father(kostas, stassa).
%	tiny_kinship:father(A, C) :-
%	    thelma:
%	    (   father_1(A, B),
%	        parent(B, C)
%	    ).
%
%	true.
%	==
%
%	This happens whenever new clauses of a previous defined
%	predicate are asserted in a different module than the
%	predicate's original implementation module. The reason we may
%	wish to do that is to create "multiple worlds" each with
%	different definitions of a predicate. For example, Thelma, where
%	the above example is taken from, asserts a learned hypothesis to
%	the dynamic database in order to test it against the negative
%	examples. However, the clauses of the learned hypothesis can get
%	mixed up with the examples of the target predicate. This creates
%	an unholy mess that is very fiddly to manage.
%	erase_program_clauses/1 helps a little, but, ultimately, one
%	must never forget that the dynamic database is evil.
%
erase_program_clauses([]):-
	!.
erase_program_clauses([Ref|Rs]):-
	erase(Ref)
	,erase_program_clauses(Rs).



%!	eraseall_program_clauses(+Refs) is det.
%
%	retractall-like clause erasure.
%
%	"Retractall-like" in the sense that if a reference in Refs is
%	now found, this doesn't fail and simply continues with the next
%	reference.
%
eraseall_program_clauses([]):-
	!.
eraseall_program_clauses([Ref|Rs]):-
	erase(Ref)
	,!
	,eraseall_program_clauses(Rs).
eraseall_program_clauses([_Ref|Rs]):-
	eraseall_program_clauses(Rs).




% [sec_expr]
% ================================================================================
% Experiment file auxiliaries
% ================================================================================
% Auxiliaries for inspecting and manipulating experiment files.


%!	cleanup_experiment is det.
%
%	Clean up after a learning session.
%
%	Currently this only removes clauses of m/n asserted to the
%	dynamic database.
%
%	Remember to run initialise_experiment/0 after this one to
%	re-load any necessary clauses.
%
cleanup_experiment:-
	% Retract encapsulated examples, BK and metarule clauses.
	forall(user:current_predicate(m,H)
	      ,(user:retractall(H)
	       % Clauses in program module are asserted
	       % by predicates in program_reduction module
	       ,program:retractall(H)
	       )
	      )
	% Retract encapsulated clauses of predicates in BK closure.
	,forall(user:current_predicate(p,H)
	      ,(user:retractall(H)
	       ,program:retractall(H)
	       )
	      )
	% Remove tabling for all tabled predicates
	,abolish_all_tables.



%!	experiment_data(+Targets,-Positive,-Negative,-BK,-Metarules) is
%!	det.
%
%	Collect experiment file data for one or more learning Targets.
%
%	Targets is either a single predicate indicator, or a list of
%	predicate indicators of the predicates to be learned.
%
%	experiment_data/5 expects an experiment file to be loaded into
%	memory and will fail without warning otherwise.
%	initialise_experiment/0 should be called before it, and
%	cleanup_experiment/0 after it if cleanup is required between
%	experiments.
%
experiment_data(Ts,_,_,_,_):-
% A list of learning targets must be ground.
	is_list(Ts)
	,learning_targets(Ls)
	,forall(member(T,Ts)
	       ,(   \+ memberchk(T,Ls)
		->  throw('Unknown learning target':T)
		;   true
		)
	       )
	,fail.
experiment_data(T,_,_,_,_):-
% A single learning target's predicate indicator must be ground.
	\+ is_list(T)
	,learning_targets(Ts)
	,\+ memberchk(T,Ts)
	,throw('Unknown learning target':T).
experiment_data(T,Pos,Neg,BK,MS):-
	signed_examples(positive,experiment_file,T,Pos_)
	,signed_examples(negative,experiment_file,T,Neg_)
	,maplist(list_to_set,[Pos_,Neg_],[Pos,Neg])
	,bk_or_metarules(background_knowledge,experiment_file,T,BK)
	,bk_or_metarules(metarules,experiment_file,T,MS_)
	,(   (MS_ == [all]
	     ;	 memberchk(all,MS_)
	     )
	 ->  configuration_metarules(MS)
	 ;   MS = MS_
	 ).


%!	signed_examples(+Sign,+Module,+Targets,-Examples) is det.
%
%	Collect positive or negative Examples of one or more Targets.
%
%	Sign is one of [positive,negative] denoting the kind of
%	examples to collect. Module is the module name of the current
%	experiment file. Targets is either a list of learning targets'
%	symbols and arities as F/A predicate indicators, or a single
%	predicate indicator.
%
%	Examples is a list of examples of all the learning Targets. If
%	Sign is "positive", Examples is a list of positive examples
%	(ground unit clauses). If Sign is "negative", Examples is a list
%	of negative examples (ground unit clauses prefixed with ":-").
%
signed_examples(S,M,Ts,Es):-
% Ts is a list of learning targets.
	is_list(Ts)
	,!
	,atom_concat(S,'_example',F)
	,C =.. [F,T,Ep]
	,findall(Ep_
		,(member(T,Ts)
		 ,M:C
		 ,signed_example(S,Ep,Ep_)
		 )
		,Es_)
	,flatten(Es_,Es).
signed_examples(S,M,T,Es):-
% T is a single learning target.
	atom_concat(S,'_example',F)
	,C =.. [F,T,Ep]
	,findall(Ep_
		,(M:C
		 ,signed_example(S,Ep,Ep_)
		 )
		,Es).


%!	signed_example(+Example,-Signed) is nondet.
%
%	Ensure an Example is Signed if nessary.
%
%	Negative examples declared in an experiment file as ground unit
%	clauses or sets of literals must be prefixed with ":-" (so that
%	they are properly definite goals).
%
%	Negative examples can also be definite clauses with a head
%	literal, in which case it's not necessary to change them.
%
%	Positive examples don't need to be signed.
%
signed_example(_,H:-B,H:-B):-
	!.
signed_example(positive,E,E).
signed_example(negative,E,:-E).


%!	bk_or_metarules(+Bias,+Module,+Targets,-Delarations) is det.
%
%	Retrieve the BK or metarule Declarations for a MIL problem.
%
%	Bias is one of [background_knowledge,metarules]. Module is the
%	module name of the current experiment file. Targets is either a
%	list of learning targets or a single target, each as an F/A
%	predicate indicator.
%
%	If Bias is "background_knowledge", Declarations is a list of the
%	F/A predicate indicators of predicates declared as background
%	knowledge for each learning target in Targets.
%
%	If Bias is "metarules", Declarations is a list of atomic
%	metarule identifiers.
%
bk_or_metarules(B,M,Ts,Bs):-
% Ts is a list of learning targets.
	is_list(Ts)
	,!
	,C =.. [B,T,Bs_]
	,findall(E_
		,(member(T,Ts)
		 ,M:C
		 ,member(E_,Bs_)
		 )
	      ,Bs_)
	,list_to_set(Bs_, Bs)
	.
bk_or_metarules(B,M,T,Bs):-
% T is a single learning target.
	C =.. [B,T,Bs]
	,M:C.


%!	configuration_metarules(-Metarules) is det.
%
%	Collect the names of all Metarules defined in the configuration.
%
%	Used when the list of metarules for a learning targets includes
%	the atom "all", meaning that all known metarules should be used
%	for that learning target.
%
configuration_metarules(MS):-
	findall(Id
	       ,(configuration:current_predicate(metarule,H)
		,predicate_property(H, implementation_module(configuration))
		,H =.. [metarule,Id|_]
		,clause(H, _B)
		)
	       ,MS).



%!	initialise_experiment is det.
%
%	Load and initialise the current experiment file.
%
%	@deprecated Use load_experiment_file/1 instead.
%
initialise_experiment:-
	configuration:experiment_file(P,_M)
	,load_experiment_file(P).



%!	learning_targets(?Target) is nondet.
%
%	Generate each learning Target in an experiment file.
%
learning_target(T):-
	learning_targets(Ts)
	,member(T,Ts).



%!	learning_targets(-Targets) is det.
%
%	Collect learning Targets defined in an experiment file.
%
%	Targets is the list of predicate symbols and arities of each of
%	the target predicates that have background knowledge
%	declarations in background/2 clauses in the current experiment
%	file.
%
learning_targets(Ts):-
	findall(T
		,experiment_file:background_knowledge(T, _BK)
		,Ts_)
	,flatten(Ts_,Ts).



%!	load_experiment_file is det.
%
%	Load the current experiment file into module user.
%
load_experiment_file:-
	experiment_file(P,_M)
	,user:use_module(P).



%!	edit_experiment_file is det.
%
%	Open the current experiment file in the Swi-Prolog IDE.
%
edit_experiment_file:-
	configuration:experiment_file(P,_M)
	,edit(P).



%!	tp_safe_experiment_data(+Target,-Pos,-Neg,-BK,-MS) is det.
%
%	Ensure experiment data is safe for TP operator predicates.
%
%	Basically just removes the ":-" in front of the negative
%	examples, since they are not recognised by the Top program
%	construction predicates that use a TP operator.
%
tp_safe_experiment_data(T,Pos,Neg_,BK,MS):-
	configuration:theorem_prover(TP)
	,experiment_data(T,Pos,Neg,BK,MS)
	,(   TP == tp
	 ->  setof(E
		  ,Neg^member((:-E),Neg)
		  ,Neg_)
	 ;   Neg_ = Neg
	 )
	,!.
tp_safe_experiment_data(T,Pos,[],BK,MS):-
% If there are no negative examples there's nothing to sanitise.
	experiment_data(T,Pos,[],BK,MS).




% [sec_prog]
% ================================================================================
% Program auxiliaries
% ================================================================================
% Predicates for inspecting a program.


%!	built_in_or_library_predicate(+Predicate) is det.
%
%	True for a built-in or autoloaded Predicate.
%
%	Thin wrapper around predicate_property/2. Used to decide what
%	programs to collect with closure/3 and what programs to
%	encapsulate.
%
built_in_or_library_predicate(H):-
	predicate_property(H, built_in)
	,!.
built_in_or_library_predicate(H):-
	predicate_property(H, autoload(_)).



%!	closure(+Progam_Symbols,+Module,-Closure) is det.
%
%	Collect all clauses of a program and its Closure.
%
%	As program/3, but also collects the definitions of programs in
%	the closure of a progam.
%
%	Progam_Symbols is a list of predicate symbols and arities, F/A,
%	of clauses in a program. Closure is the set of definitions of
%	the Symbols in Program_Symbols, and the definitions of the
%	programs in the closure of each program in Program_Symbols.
%
%	Module is the definition module of each program in Closure, or
%	a module importing that module. To ensure each program in
%	Closure is accessible the best thing to do is to export
%	everything to the user module.
%
%	Example
%	-------
%	==
%	?- closure([ancestor/2],user,_Cs),forall(member(P,_Cs),print_clauses(P)).
%
%	ancestor(A,B):-parent(A,B).
%	ancestor(A,B):-parent(A,C),ancestor(C,B).
%	parent(A,B):-father(A,B).
%	parent(A,B):-mother(A,B).
%	father(stathis,kostas).
%	father(stefanos,dora).
%	father(kostas,stassa).
%	mother(alexandra,kostas).
%	mother(paraskevi,dora).
%	mother(dora,stassa).
%	true.
%	==
%
%	@tbd Why is closure/3 returning a list of lists rather than a
%	flat list? In mil_problem:encapsulated_bk/2 it's flattened after
%	being called. Why do we need to do that?
%
closure(Ss,M,Cs):-
	closure(Ss,[],_Ps,M,[],Cs_)
	,reverse(Cs_, Cs).

%!	closure(+Symbols,+Path_Acc,-Path,+Module,+Acc,-Closure) is det.
%
%	Business end of closure/3.
%
closure([],Ps,Ps,_M,Cs,Cs):-
	!.
closure([F/A|Ss],Ps_Acc,Ps_Bind,M,Acc,Bind):-
	functor(S,F,A)
	,built_in_or_library_predicate(S)
	,!
	,closure(Ss,Ps_Acc,Ps_Bind,M,Acc,Bind).
closure([S|Ss],Ps_Acc,Ps_Bind,M,Acc,Bind):-
	\+ memberchk(S,Ps_Acc)
	,!
	,debug(closure,'Looking for definition of ~w in module ~w.', [S,M])
	,program(S,M,Cs)
	,(   Cs \= []
	 ->  debug(closure,'Found definition of ~w in module ~w.~n', [S,M])
	 ;   debug(closure,'Found empty definition of ~w in module ~w.~n', [S,M])
	 )
	,closure(Ss,[S|Ps_Acc],Ps_Acc_,M,[Cs|Acc],Acc_)
	,program_symbols(Cs,Ss_)
	,closure(Ss_,Ps_Acc_,Ps_Bind,M,Acc_,Bind).
closure([_S|Ss],Ps,Ps_Acc,M,Acc,Bind):-
	closure(Ss,Ps,Ps_Acc,M,Acc,Bind).



%!	program_symbols(+Program,-Symbols) is det.
%
%	Collect symbols of body literals in a Program.
%
program_symbols(Ps,Ss):-
	clauses_literals(Ps,Ls)
	,setof(F/A
	      ,L^Ls^(member(L,Ls)
		    ,functor(L,F,A)
		    )
	      ,Ss).



%!	program(+Symbols,+Module,-Program) is det.
%
%	Collect all clauses of a Program.
%
%	Symbols is the list of predicate indicators, F/A, of clauses in
%	Program.
%
%	Module is the definition module for Progam. This can be set to
%	user if the Program is not defined in a module.
%
%	Program is a list of all the clauses of the predicates in
%	Symbols.
%
%	@tbd This doesn't attempt to sort the list of Symbols to exclude
%	duplicates- if the same Symbol is passed in more than once, the
%	same definition will be included that many times in Programs.
%
program(F/A,M,Ps):-
	!
	,program([F/A],M,Ps).
program(Ss,M,Ps):-
	findall(P
	       ,(member(F/A,Ss)
		,functor(H,F,A)
		,M:clause(H,B)
		,(   B == true
		 ->  P = H
		 ;   P = (H:-B)
		 )
		)
	       ,Ps).



%!	fold_recursive(+Clauses,-Folded) is det.
%
%	Fold a set of Clauses into recursive variants, if possible.
%
%	Clauses is a set of clauses. Folded is a set of clauses such
%	that each clause C in Clauses is folded into a clause C' by
%	replacing any sequence of literals L1, ..., Ln in C by the head
%	of a clause in Clauses.
%
fold_recursive(Cs,Fs):-
	program_reduction(Cs,Rs,_)
	,sort_clauses(Rs, Rs_)
	,fold_recursive_all(Rs_,Fs).


%!	fold_recursive_all(+Clauses,-Folded) is det.
%
%	Fold a set of clauses until no redundancies are left.
%
%	Ensures every clause that can be folded into another, is.
%
fold_recursive_all(Cs,Rs):-
	fold_clauses(Cs,Fs)
	,program_reduction(Fs, Fs_r,_)
	,sort_clauses(Fs_r, Fs_)
	,Fs_ \= Cs
	,!
	,fold_recursive_all(Fs_,Rs).
fold_recursive_all(Rs,Rs).


%!	sort_clauses(+Clauses,-Sorted) is det.
%
%	Sort a set of clauses, ignoring variable ages.
%
sort_clauses(Cs,Cs_s):-
	setof(C
              ,Cs^(member(C,Cs)
		  ,numbervars(C)
               )
              ,Cs_)
	,findall(C_1
		,(member(C_,Cs_)
		 ,varnumbers(C_,C_1)
		 )
		,Cs_s).


%!	fold_clauses(+Clauses,-Folded) is det.
%
%	Fold a set of Clauses to add recursion, if possible.
%
fold_clauses(Cs,Fs):-
        fold_clauses(Cs,Cs,[],Fs).


%!	fold_clauses(+ToFold,+Clauses,+Acc,-Folded) is det.
%
%	Business end of fold_clauses/2.
%
%	ToFold and Clauses begin as the same list of clauses to be
%	folded. Each clause in ToFold is attempted to be folded by
%	replacing some of its body literals with the head literal of a
%	head in Clauses, if possible.
%
fold_clauses([],_,Fs,Fs):-
        !.
fold_clauses([C|Cs],Cs_,Acc_1,Bind):-
        fold_clause(C,Cs_,Acc_1,Acc_2)
        ,fold_clauses(Cs,Cs_,Acc_2,Bind).


%!	fold_clause(+Clause,+Clauses,+Acc,-Folded) is det.
%
%	Fold a Clause to add recursion, if possible.
%
%	Folds each fold given as the first argument in fold_clauses/4
%	with each of the clauses in the list given as the first argument
%	in fold_clauses/4, replacing Clause with a folded clause with a
%	recursive call, if possible.
%
fold_clause(_C1,[],Fs,Fs):-
        !.
fold_clause(C1,[C2|Cs],Acc,Bind):-
        folding(C1,C2,C3)
        ,fold_clause(C1,Cs,[C3|Acc],Bind).


%!	folding(+Clause1,+Clause2,-Folded) is det.
%
%	Fold two clauses to introduce recursion, if possible.
%
%	Let Clause1 = H1:- L11, ..., Li, ..., Lk, ..., Ln and let
%	Clause2 = H2:- L21, ..., L2m. Then, if H1 and H2 have the same
%	predicate symbol and arity AND L21 ,..., L2m = Li, ..., Lk,
%	Clause3 is the clause H2:- L11,..., H2, ..., Ln.
%
%	Note that in the process of derving Clause3 from Clause2, first
%	L21 ,..., L2m = Li, ..., Lk are unified. Unification changes the
%	variables in Clause1, resulting in new variable names in
%	Clause3, but not in a new "wiring" of the variables.
%
%	The result of all this is that Clause 3 has one or more
%	recursive body literals.
%
%	If there is no sub-sequence Li, ..., Lk of the body literals in
%	Clause1 that matches the body literals of Clause2, then Clause3
%	is Clause1, unchanged.
%
folding(C1, C2, H_1:-Ls_4):-
        maplist(clause_literals,[C1,C2],[[H_1|Ls_1],[H_2|Ls_2]])
        ,replace_all(H_2,Ls_1,Ls_2,Ls_3)
        ,once(list_tree(Ls_3,Ls_4)).


%!	replace_all(+Head,+Body1,+Body2,-Replaced) is det.
%
%	Replace all occurrences of a set of literals with a new one.
%
%	Head is the head of a clause that has Body2 as its body. Body1
%	is a sequence of body literals of a possibly different clause,
%	that has Body2 as its sub-sequence. Replaced is the sequence of
%	literals in Body1, where the sub-sequence matching Body2 is
%	replaced by Head.
%
%	To replace Body2 in Body1 with Head, literals in Body1 and Body2
%	are unified, resulting in binding of variables in Head, Body1
%	and Body2. This is necessary to preserver the "wiring" of
%	literals in the original clauses and to make sure the clause in
%	Replaced is fully-connected (or "closed"), i.e. that it doesn't
%	have any free variables left dangling without sharing.
%
replace_all(S,Xs,Ys,Zs):-
        replace_all_(S,Xs,Ys,Zs).

%!	replace_all_(+Head,+Body1,+Body2,-Replaced) is det.
%
%	Business end of replace_all/4.
%
replace_all_(S,Xs,Ys,Bind):-
        replace(S,Xs,Ys,Xs_)
        ,!
        ,replace_all_(S,Xs_,Ys,Bind).
replace_all_(_S,Zs,_Ys,Zs).


%!	replace(+Head,+Body1,+Body2,-Replaced) is det.
%
%	Replace all occurrences of Body2 in Body1 with Head.
%
replace(S,Xs,Ys,Zs):-
        replace(S,Xs,Ys,[],Zs).

%!	replace(+Head,+Body1,+Body2,+Acc,-Replaced) is det.
%
%	Business end of replace/4.
%
replace(S,[X|Xs],[Y|Ys],Acc,Bind):-
% Elements don't match.
        X \= Y
        ,!
        ,replace(S,Xs,[Y|Ys],[X|Acc],Bind).
replace(S,[Y|Xs],[Y|Ys],Acc,Zs):-
% Matchine elements of a _possible_ shared sub-sequence.
        drop(Xs,Ys,Acc_2)
        ,!
        ,reverse(Acc,Acc_1)
        ,append(Acc_1,[S|Acc_2],Zs).
replace(S,[X|Xs],[Y|Ys],Acc,Zs):-
% Matching elements _not_ in a shared subsequence.
        replace(S,Xs,[Y|Ys],[X|Acc],Zs).


%!	drop(+Sequence,+Subsequence,-Dropped) is det.
%
%	Drop all elemeents of a Subsequence from a Sequence.
%
%	Sequence and Subsequence are lists, interpreted as sequences.
%	Dropped is the result of removing Subsequence from Sequence.
%
drop(Xs,[],Xs):-
        !.
drop([Y|Xs],[Y|Ys],Acc):-
        drop(Xs,Ys,Acc).




% [sec_time]
% ================================================================================
% Timing auxiliaries
% ================================================================================
% Predicates to time the execution of a goal.

%!	timing(+Goal,-Time) is det.
%
%	Call a Goal and report the Time it took.
%
%	@tbd: If Goal fails, no Time is reported. Wrap in a conditional?
%
timing(G, T):-
	S is cputime
	,call(G)
	,E is cputime
	,T is E - S.



%!	timing(+Goal,+Limit,-Time) is det.
%
%	Call a Goal with a time Limit and report the Time it took.
%
%	@tbd: If Goal fails, no Time is reported. Wrap in a conditional?
%
timing(G, L, T):-
	S is cputime
	,C = call_with_time_limit(L,G)
	,R = debug(learning_curve_full,'Exceeded Time limit: ~w sec',[L])
	,catch(C,time_limit_exceeded,R)
	,E is cputime
	,T is E - S.




% [sec_learn]
% ================================================================================
% Learning predicate auxiliaries
% ================================================================================
% Helpers for learning predicates.


%!	learning_query(+Pos,+Neg,+BK,+MS,?Ps) is det.
%
%	Construct a learning query for training and evaluation.
%
%	This predicate allows evaluation of hypotheses learned with
%	different learning predicates as long as they conform to a
%	common interface. This is as follows:
%
%	==
%	P(+List:Pos,+List:Neg,+List:BK,+List:MS,-PS)
%	==
%
%	In particular, learning predicates must have 5 arguments, the
%	same as learning_query/5. The first four are the positive and
%	negative examples, list of background predicate symbols and
%	arities and the identifiers of metarules, while the 5th is the
%	learned hypothesis. learning_query/5 is responsible for
%	constructing a query with a learning predicate and the elements
%	of the MIL problem passed to it as arguments, then binding the
%	result to Ps (the program).
%
%	The learning predicate used to construct a learning query is
%	defined in the configuration option learning_predicate/1. If
%	this is not set, learning_query/5 defaults to learn/5.
%
%	The motivation for this predicate is to allow the evaluation
%	module to be used with different learning settings in the same
%	learner and with different learners, each of which may declare
%	differently-named learning predicates.
%
learning_query(Pos,Neg,BK,MS,Ps):-
% Use a learning predicate required by an experiment file.
	configuration:learning_predicate(F/_A)
	,!
	,Q =.. [F,Pos,Neg,BK,MS,Ps]
	,call(Q).
learning_query(Pos,Neg,BK,MS,Ps):-
% The learning predicate may need to be called by module.
	configuration:learning_predicate(M:F/_A)
	,!
	,Q =.. [F,Pos,Neg,BK,MS,Ps]
	,M:call(Q).
learning_query(Pos,Neg,BK,MS,Ps):-
% Default to learn/5.
	Q =.. [learn,Pos,Neg,BK,MS,Ps]
	,call(Q).
