:-module(auxiliaries, [% Printing auxiliaries
	               print_or_debug/3
		       % Configuration auxiliareis
		      ,debug_config/1
		      ,list_config/0
		      ,reset_defaults/0
		      ,set_configuration_option/2
		       % MIL Problem auxiliaries
		      ,known_metarules/1
		      ,same_metarule/2
		      ,write_encapsulated_problem/1
		      ,write_encapsulated_problem/4
		       % Debugging auxiliaries
		      ,list_encapsulated_problem/1
		      ,list_mil_problem/1
		      ,list_problem_statistics/1
		      ,list_top_program_reduction/1
		      ,list_top_program/1
		      ,list_top_program/2
		       % Database auxiliaries
		      ,assert_program/3
		      ,erase_program_clauses/1
		       % Experiment file auxiliaries
		      ,cleanup_experiment/0
		      ,experiment_data/5
		      ,initialise_experiment/0
		      ,learning_targets/1
		      ,load_experiment_file/0
		      ,tp_safe_experiment_data/5
		       % Program auxiliaries
		      ,built_in_or_library_predicate/1
		      ,closure/3
		      ,debug_clauses/2
		      ,print_clauses/1
		      ,program/3
		      ]).

:-user:use_module(lib(term_utilities/term_utilities)).
:-user:use_module(lib(program_reduction/program_reduction)).
:-user:use_module(lib(mathemancy/mathemancy)).
:-use_module(src(mil_problem)).
:-use_module(src(defaults)).

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

1. Printing auxiliaries [sec_print]
   * print_or_debug/3

2. Configuration auxiliaries [sec_config]
   * debug_config/1
   * list_config/0
   * reset_defaults/0
   * set_configuration_option/2

3. MIL problem auxiliaries [sec_prob]
   * known_metarules/1
   * same_metarule/2
   * write_encapsulated_problem/1
   * write_encapsulated_problem/4

4. Debugging auxiliaries [sec_debug]
   * list_encapsulated_problem/1
   * list_mil_problem/1
   * list_problem_statistics/1
   * list_top_program_reduction/1
   * list_top_program/1
   * list_top_program/2

5. Database auxiliaries [sec_dynm]
   * assert_program/3
   * erase_program_clauses/1

6. Experiment file auxiliaries [sec_expr]
   * cleanup_experiment/0
   * experiment_data/5
   * initialise_experiment/0
   * learning_targets/1
   * load_experiment_file/0
   * tp_safe_experiment_data/5

7. Program auxiliaries [sec_prog]
   * built_in_or_library_predicate/1
   * closure/3
   * debug_clauses/2
   * print_clauses/1
   * program/3

*/


% [sec_print]
% ================================================================================
% Printing auxiliaries
% ================================================================================
% Predicates for printing to streams (including user_output).


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




% [sec_config]
% ================================================================================
% Configuration auxiliaries
% ================================================================================
% Predicates for inspecting and manipulating configuration options.


%!	debug_config(+Subject) is det.
%
%	Log configuration options to the debug stream for Subject.
%
%	Only configuration options actually defined in the configuration
%	module (i.e. not re-exported from other configuration files) are
%	logged.
%
debug_config(S):-
	list_config(debug,S).


%!	list_config is det.
%
%	Print configuration options to the console.
%
%	Only configuration options actually defined in the configuration
%	module (i.e. not re-exported from other configuration files) are
%	printed.
%
list_config:-
	list_config(print,user_output).


%!	list_config(+Print_or_Debug,+Atom) is det.
%
%	Print or debug current configuration options.
%
list_config(T,S):-
	module_property(configuration, exports(Es))
	,findall(Opt_
		,(member(F/A,Es)
		 ,\+ memberchk(F, [metarule,metarule_constraints])
		 ,functor(Opt,F,A)
		 ,predicate_property(Opt, implementation_module(configuration))
		 ,call(configuration:Opt)
		 % Keep as list to sort by functor only
		 % Standard order of terms sorts by arity also.
		 ,Opt =.. Opt_
		 )
		,Opts)
	,sort(1,@<,Opts, Opts_)
	,forall(member(Opt, Opts_)
	       ,(Opt_ =.. Opt
		,print_or_debug(T,S,Opt_)
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
		,(  \+ memberchk(F, [metarule,metarule_constraints])
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


%!	known_metarules(-Ids) is det.
%
%	Collect the Ids of metarules known to the system.
%
known_metarules(Ids):-
	findall(Id
	       ,(configuration:current_predicate(metarule, H)
		,H =.. [metarule,Id|_]
		,clause(H,_B)
		)
	       ,Ids).



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
% Predicates to facilitate debugging of experiments.


%!	list_encapsulated_problem(+Target) is det.
%
%	Pretty-print the encapsulation of a MIL problem.
%
%	Target is the symbol and arity of the target predicate in the
%	encapsulated MIL problem to be listed.
%
list_encapsulated_problem(T):-
	configuration:extend_metarules(E)
	,experiment_data(T,Pos,Neg,BK,MS)
	,encapsulated_clauses(Pos,Pos_)
	,format_underlined('Positive examples')
	,print_clauses(Pos_)
	,nl
	,encapsulated_clauses(Neg,Neg_)
	,format_underlined('Negative examples')
	,print_clauses(Neg_)
	,nl
	,encapsulated_bk(BK,BK_)
	,(   E \== false
	 ->  extended_metarules(MS,MS_)
	 ;   expanded_metarules(MS,MS_)
	 )
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
	       ,(program(P,user,Ps)
		,format('~w:~n',[P])
		,print_clauses(Ps)
		,format('~n',[])
		)
	       )
	,format_underlined('Metarules')
	,forall(member(Id,MS)
	       ,(configuration:current_predicate(metarule,H)
		,H =.. [metarule,Id|_]
		,clause(H, B)
		,print_clauses([H:-B])
		)
	       ).


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
	,format('Background knowledge: ~w~n', [K])
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
%	Same as list_top_program(Target, true).
%
list_top_program(T):-
	list_top_program(T,true).



%!	list_top_program(+Target, +Unfold) is det.
%
%	Pretty-print the Top program for a Target predicate.
%
%	Unfold is one of [true,false]. If true, the Top program is
%	unfolded into a list of definite clauses before printing.
%	Otherwise it is printed as a list of metasubstitutions.
%
list_top_program(T,U):-
	experiment_data(T,Pos,Neg,BK,MS)
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,louise:write_program(Pos_,BK_,Refs)
	,louise:generalise(Pos_,MS_,Ss_Pos)
	,findall(S
		,member(S-_M,Ss_Pos)
		,Ss_Pos_)
	,write_and_count('Generalisation:',MS,Ss_Pos_,U)
	,louise:specialise(Ss_Pos,Neg_,Ss_Neg)
	,nl
	,erase_program_clauses(Refs)
	,write_and_count('Specialisation:',MS,Ss_Neg,U).


%!	write_and_count(+Message,+Metasubs,+Unfold) is det.
%
%	Auxiliary to list_top_program/2.
%
%	Pretty-print a set of Metasubs, its cardinality and a Message.
%	Unfold is a boolean inherited from list_top_program/2,
%	determining whether the Top program is unfolded to a list of
%	definite clauses before printing.
%
write_and_count(Msg,MS,Cs,U):-
	(   U = true
	->  unfolded_metasubs(Cs, MS, Cs_)
	;   U = false
	->  Cs_ = Cs
	% Else fail silently to flumox the user. Nyahahaha!
	)
	,length(Cs_, N)
	,format_underlined(Msg)
	,print_clauses(Cs_)
	,format('Length:~w~n',[N]).



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
	clause(M:A,true)
	,!
	,assert_program(M,P,Acc,Bind).
assert_program(M,[H:-B|P],Acc,Bind):-
	clause(M:H,B)
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
	      ).



%!	experiment_data(+Target,-Positive,-Negative,-BK,-Metarules) is
%!	det.
%
%	Data about a Target theory from the current experiment file.
%
%	Target is the predicate indicator of the predicate to be
%	learned.
%
%	experiment_data/5 expects an experiment file to be loaded into
%	memory and will fail without warning otherwise.
%	initialise_experiment/0 should be called before it, and
%	cleanup_experiment/0 after it if cleanup is required between
%	experiments.
%
experiment_data(T,Pos,Neg,BK,MS):-
	configuration:experiment_file(P,M)
	,user:use_module(P)
	,findall(Ep
		,M:positive_example(T,Ep)
		,Pos_)
	,sort(Pos_, Pos)
	,findall(:-En
		,M:negative_example(T,En)
		,Neg_)
	,sort(Neg_, Neg)
	,once(M:background_knowledge(T,BK))
	,once(M:metarules(T,MS_))
	,(   MS_ == [all]
	 ->  configuration_metarules(MS)
	 ;   MS = MS_
	 ).


%!	configuration_metarules(+Metarules) is det.
%
%	Collect the names of all Metarules defined in the configuration.
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
initialise_experiment:-
	configuration:experiment_file(P,_M)
	,user:use_module(P).



%!	learning_targets(+Targets) is det.
%
%	Collect learning Targets defined in an experiment file.
%
%	Targets is the list of predicate symbols and arities of each of
%	the target predicates that have background knowledge
%	declarations in background/2 clauses in the current experiment
%	file.
%
learning_targets(Ts):-
	initialise_experiment
	,experiment_file(_P, M)
	,findall(T
		,M:background_knowledge(T, _BK)
		,Ts).



%!	load_experiment_file is det.
%
%	Load the current experiment file into module user.
%
load_experiment_file:-
	experiment_file(P,_M)
	,user:use_module(P).



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
	,program(S,M,Cs)
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



%!	debug_clauses(+Topic,+Clauses) is det.
%
%	Debug a list of Clauses if Topic is being debugged.
%
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



%!	print_clauses(+Clauses) is det.
%
%	Print a list of Clauses to standard output.
%
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
