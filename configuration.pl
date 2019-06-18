:-module(configuration, [experiment_file/2
			,metarule/2
			,metarule/3
			,metarule/4
			,metarule_language/2
			]).

:-reexport(lib(program_reduction/reduction_configuration)).

% Body literals of H(2,2) metarules.
:-dynamic m/1
         ,m/2
         ,m/3.

% Allows experiment files to define their own, special metarules.
:-multifile metarule/2
           ,metarule/3
           ,metarule/4.

%!	experiment_file(?Path,?Module) is semidet.
%
%	The Path and Module name of an experiment file.
%
experiment_file('data/examples/tiny_kinship.pl',tiny_kinship).


%!	metarule(?Id,?P,?Q) is semidet.
%!	metarule(?Id,?P,?Q,?R) is semidet.
%
%	An encapsulated metarule.
%
%	@tbd This representation does not define constraints. For the
%	time being this doesn't seem to be necessary but a complete
%	representation will need to include constraints.
%
metarule(unit,P):- m(P,_X,_Y).
metarule(projection,P,Q):- m(P,X,X), m(Q,X).
metarule(identity,P,Q):- m(P,X,Y), m(Q,X,Y).
metarule(inverse,P,Q):- m(P,X,Y), m(Q,Y,X).
metarule(chain,P,Q,R):- m(P,X,Y), m(Q,X,Z), m(R,Z,Y).
metarule(tailrec,P,Q,P):- m(P,X,Y), m(Q,X,Z), m(P,Z,Y).
metarule(precon,P,Q,R):- m(P,X,Y), m(Q,X), m(R,X,Y).
metarule(postcon,P,Q,R):- m(P,X,Y), m(Q,X,Y), m(R,Y).
metarule(switch,P,Q,R):- m(P,X,Y), m(Q,X,Z), m(R,Y,Z).


%!	metarule_language(?Min,?Max) is semidet.
%
%	The minimume and maximum number of body literals in metarules.
%
%	Used to constraint the search for metarules in
%	louise:metarule_expansion/2.
%
metarule_language(1,3).


/* % Alternative defintiion of metarule/n.
% This one is more convenient in some ways but less readable I think.

%!	metarule(?Id,?Existential,?Universal) is semidet.
%
%	An encapsulated metarule.
%
%	@tbd This representation does not define constraints. For the
%	time being this doesn't seem to be necessary but a complete
%	representation will need to include constraints.
%
metarule(chain,[P,Q,R],[X,Y,Z]):- m(P,X,Y),m(Q,X,Z),m(R,Z,Y).
metarule(identity,[P,Q],[X,Y]):- m(P,X,Y), m(Q,X,Y).
metarule(tailrec,[P,Q],[X,Y,Z]):-m(P,X,Y),m(Q,X,Z),m(P,Z,Y).
*/
