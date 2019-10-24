:-module(experiment_file, [reload/0
			  ,experiment_file_interface/1]).

/** <module> Experiment file interface and reloading.

The most important predicate in this module is reload/0. This is, well,
a bit of a hack, or rather a whole bunch of hacks, performed in order to
allow switching between experiment files without having to exit Prolog
and start a new session to clear up interface predicates from memory.
See the strucured documentation of that predicate for more details.

This module also exports experiment_file_interface/1 that determines the
interface of an experiment file, i.e. the least predicates it must
export. This is currently only cosmetic (although it is used by
reload/0, nothing else requires it).

*/

:-dynamic '$experiment_file'/2.

%!	reload is det.
%
%	Reload the currently configured experiment file.
%
%	Overview
%	--------
%
%	Ensures interface predicates are updated to the ones exported by
%	the currently configured experiment file module. The currently
%	configured experiment file module is the one specified in the
%	configuration option experiment_file/2.
%
%	Interface predicate definitions are updated with a call to
%	reload/0 from the configuration module, in configuration.pl,
%	every time the configuration module is modified and recompiled
%	(e.g. by make/0).
%
%	The recommended way to load a new experiment file into memory is
%	to edit the configuration option experiment_file/2 to specify
%	the new experiment file's path and module name. Saving and
%	loading the configuration module will then call this predicate
%	and update the definitions of the interface predicates
%	in the database to the ones exported by the experiment file in
%	the specified path.
%
%	Motivation
%	----------
%
%	Experiment file modules have a common interface, consisting of a
%	number of predicate symbols that they are all expected to
%	export. However, in Swi-Prolog, attempting to export the same
%	predicate symbols from two different modules raises a permission
%	error. This means that to switch to a new experiment file
%	without raising errors the user would normally have to exit
%	Swi-Prolog and start a new session.
%
%	To avoid this inconvenience, this predicate performs a number of
%	steps that ensure that only the interface predicates exported by
%	the experiment file currently specified in the configuration
%	option experiment_file/2 are loaded in memory and that loading a
%	new experiment file does not raise permission errors.
%
%	Reloading process
%	-----------------
%
%	The steps taken by this predicate to update the definitions of
%	the experiment file interface predicates are as follows:
%
%	  0. A dynamic predicate, '$experiment_file'/2 is instatiated to
%	  the arguments of experiment_file/2 store the file path and
%	  module name given in the configuration option.
%
%	  1. If a clause of '$experiment_file'/2 does not exist in the
%	  database, one is created.
%
%	  2. If a clause of '$experiment_file'/2 exists in the database,
%	  the file at the path specified in its first argument is
%	  unloaded. This removes from memory all clauses of the
%	  interface predicates, and all other predicates, loaded from
%	  this file.
%
%	  3. All clauses of '$experiment_file'/2 are retracted from the
%	  database.
%
%	  4. A clause of '$experiment_file'/2 is asserted to the
%	  database with the file path and module name given in the
%	  configuration option experiment_file/2 as its arguments.
%
%	  5. The experiment file interface predicates are abolished from
%	  module user.
%
%	  6. The experiment file interface predicates are abolished from
%	  the module defined in experiment_file.pl (i.e. the present
%	  module).
%
%	  7. The experiment file interface predicates defined in the
%	  currently configured experiment file module are re-exported by
%	  the module in experiment_file.pl (i.e. this here module).
%
%	The end result of the process described above is that only the
%	definitions of the experiment file interface predicates that are
%	defined in the experiment file currently specified in
%	experiment_file/2 are in the database at any one point.
%
%	The last step in the process above, (8), is how the experiment
%	file interface predicates end up in the module in
%	experiment_file.pl (this module) and also the user module. This
%	should explain why these interface predicates must be abolished
%	from the module in experiment_file.pl and not just from user.
%
%	The experiment file interface predicates are loaded into module
%	user so that they can be readily accessed from any predicate in
%	the project, and on the command line. To do this, the module in
%	experiment_file.pl is loaded into module user by the
%	configuration module (using a directive
%	":-use_module(src/experiment_file)"). The configuration module
%	is the first module loaded when the project is started so the
%	predicates defined in the currently configured experiment file
%	module are always loaded into memory when the project starts up.
%
%	The configuration module calls this predicate with a directive:
%	":-experiment_file:reload".
%
%	This directive is at the end of the configuration module so that
%	it can always "see" the latest value of the arguments of the
%	configuration option experiment_file/2. The directive is reached
%	at the end of compilation every time that the configuration
%	module is changed and reloaded (e.g. by make/0). This way, every
%	time the configuration file is is reloaded, if the
%	experiment_file/2 values have changed, a new experiment file
%	module is loaded following the procedure specified above.
%
%	@bug Loading two experiment files exporting the same definitions
%	of some predicates other than the interface predicates, one (not
%	necessarily immediately) after the other, will raise permission
%	errors saying you're not allowed to redefine the predicates
%	exported by both modules. The only reasonably easy way to avoid
%	such errors is, whenever you want to load a new experiment file
%	that exports some of the same predicates as an experiment file
%	module loaded previously in the same session, to first start a
%	new session. There is just no easy way to avoid errors if you
%	load two modules that export (some of) the same predicates in
%	the same session.
%
reload:-
	configuration:experiment_file(P, M)
	,replace_experiment_file(P,M)
	,abolish_experiment_file_interface(user)
	,abolish_experiment_file_interface(experiment_file)
	,interface_reexports(Es)
	,reexport(P, except(Es)).


%!	replace_experiment_file(+Path,+Module) is det.
%
%	Replace the previous experiment file with a new one.
%
%	Path and Module are the path and module of the experiment file
%	specified in the configuration option experiment_file/2.
%
%	This predicate is responsible for registering the latest
%	experiment file Path and Module name, while unregistering those
%	of the previous one. It is called by reload/0. If an experiment
%	file is not already registered, the new experiment file Path and
%	Module name are registered. If an experiment file is already
%	registered, it is first unregistered and then the new Path and
%	Module name are registered.
%
%	By "register" and "unregister" what is meant is that a clause of
%	the dynamic predicate '$experiment_file'/2 is added to the
%	database that has the values passed to the Path and Module
%	arguments of this predicate as paramters.
%
replace_experiment_file(P,M):-
	\+ '$experiment_file'(_,_)
	,assert('$experiment_file'(P,M))
	,!.
replace_experiment_file(P1,M1):-
	'$experiment_file'(P0,_M0)
	,unload_file(P0)
	,retractall('$experiment_file'(_,_))
	,assert('$experiment_file'(P1,M1)).


%!	abolish_experiment_file_interface(+Module) is det.
%
%	Abolish experiment file interface predicates from Module.
%
%	Used to clean up the user module and experiment_file module from
%	definitions of the experiment file predicates that are created
%	when these are re-exported from an actual experiment file
%	module. Called by reload/0.
%
abolish_experiment_file_interface(M):-
	experiment_file_interface(Is)
	,forall(member(F/A, Is)
	       ,abolish(M:F/A)
	       ).


%!	experiment_file_interface(?Interface) is semidet.
%
%	List of predicate symbols exported by experiment files.
%
experiment_file_interface([background_knowledge/2
			  ,metarules/2
			  ,positive_example/2
			  ,negative_example/2
			  ]).


%!	interface_reexports(+Exports) is det.
%
%	Collect the names of reexported interface predicates.
%
%	Exports is a list of compounds, P/A as R, where P/A the
%	predicate symbol and arity of an interface predicate, as defined
%	by interface_predicate/1, and R its renaming term used in
%	reexport/2.
%
%	In practice, P itself is always the renaming term, for example,
%	"metarules/2 as metarules".
%
%	This probably sounds bizzare but the purpose is to replace any
%	previous definitions of the export predicates and ensure that
%	only the latest ones are exported. As stated at the
%	documentation at the start of this module, that's a bit of a
%	hack.
%
interface_reexports(Es):-
	experiment_file_interface(Is)
	,findall(F/A as F
		,member(F/A,Is)
		,Es).
