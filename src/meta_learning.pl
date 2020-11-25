:-module(meta_learning, [learn_meta/1
			,learn_meta/2
			,learn_meta/5
                        ]).

:-use_module(configuration).
:-use_module(src(metarule_extraction)).
:-use_module(lib(lifting/lifting)).

/** <module> Learning while learning new metarules.

*/

%:-debug(new_metarules).

%!	learn_meta(+Targets) is det.
%
%	Meta-learn a definition of one or more learning Targets.
%
learn_meta(Ts):-
	learn_meta(Ts,Ps)
	,print_clauses(Ps).



%!	learn_meta(+Targets,-Definition) is det.
%
%	Meta-learn a definition of one or more learning Targets.
%
learn_meta(Ts,_Ps):-
	(   \+ ground(Ts)
	->  throw('learn/2: non-ground target symbol!')
	;   fail
	).
learn_meta(Ts,Ps):-
	tp_safe_experiment_data(Ts,Pos,Neg,BK,MS)
	,learn_meta(Pos,Neg,BK,MS,Ps).



%!	learn_meta(+Pos,+Neg,+BK,+Metarules,-Program) is det.
%
%	Learn a Top program by specialising a general set of Metarules.
%
%	Learning predicate for "meta-learning", a new approach that
%	begins with a set of most-general metarules and progressively
%	refines them until a correct hypothesis is constructed.
%	Most-general metarules alleviate the burden of specifying
%	metarules for a MIL problem. Currently, the approach works well
%	but it is very inefficient- use with caution when processing
%	large numbers of examples. Learned hypotheses also have a
%	tendency to over-generalise. This is a new approach and is
%	still being worked on.
%
learn_meta(Pos,Neg,BK,MS,Ps):-
	debug(learn,'Encapsulating problem',[])
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,debug(learn,'Constructing Top program...',[])
	,meta_top_program(Pos_,Neg_,BK_,MS_,Ms)
	,debug(learn,'Reducing Top program...',[])
	,reduced_top_program(Pos_,BK_,MS_,Ms,Rs)
	,examples_targets(Pos,Ss)
	,debug(learn,'Excapsulating hypothesis',[])
	,excapsulated_clauses(Ss,Rs,Ps).


%!	meta_top_program(+Pos,+Neg,+BK,+MS,-Top) is det.
%
%	Construct a Top program while learning new metarules.
%
meta_top_program(Pos,Neg,BK,MS,Ts):-
        S = write_program(Pos,BK,Refs)
        ,G = (debug(top_program,'Constructing Top program...',[])
	     ,meta_top_program(Pos,Neg,MS,Ss)
	     ,applied_metarules(Ss,_,Ts)
	     ,debug_clauses(top_program,'Applied metarules',Ts)
	     )
	,C = erase_program_clauses(Refs)
	,setup_call_cleanup(S,G,C)
	,!.
meta_top_program(_Pos,_Neg,_BK,_MS,[]):-
% If Top program construction fails return an empty program.
	debug(top_program,'INSUFFICIENT DATA FOR MEANINGFUL ANSWER',[]).


%!	meta_top_program(+Pos,+MS,-Top,-Metarules) is det.
%
%	Meta-learning Generalisation and Specialisation step.
%
meta_top_program(Pos,Neg,MS,Ss):-
	findall(Sub-M_n
	       ,(member(M,MS)
		,copy_term(M,M_)
		,member(Ep,Pos)
		,debug(new_metarules,'',[])
		,debug_clauses(new_metarules,'New example',[Ep])
		,once(meta_grounding(Ep,Neg,M_,Sub,M_n))
		%,meta_grounding(Ep,Neg,M_,Sub,M_n)
		,numbervars(M_n)
		)
	       ,Ss_)
	,sort(Ss_,Ss_s)
	,maplist(varnumbers,Ss_s,Ss)
	,debug_clauses(new_metarules,'New metasubstitutions',Ss).


%!	meta_grounding(+E,+Neg,+Metarule,+Metasub,-Specialised) is
%!	nondet.
%
%	Ground a most-general metarule and return its Specialisation.
%
meta_grounding(Ep,Neg,M,Sub,M_n):-
	metasubstitution(Ep,M,Sub)
	,constraints(Sub)
	,debug_clauses(new_metarules,'Ground instance',[M])
	,new_metarule(M,Sub,M_n)
	,debug_clauses(new_metarules,'New metarule',[M_n])
	,\+((member(En,Neg)
	    ,metasubstitution(En,M_n,Sub)
	    )
	   )
	,debug(new_metarules,'Entails 0 negative examples',[]).


%!	metasubstitution(+Example,+Metarule,-Metasubstitution) is
%!	nondet.
%
%	Perform one Metasubstutition of Metarule initialised to Example.
%
%	Example is either a positive example or a negative example. A
%	positive example is a ground definite unit clause, while a
%	negative example is a ground definite goal (i.e. a clause of the
%	form :-Example).
%
metasubstitution(:-E,M,Sub):-
	louise:bind_head_literal(E,M,(Sub:-(E,Ls)))
	,debug_clauses(metasubstitution,'Trying metasubstitution:',Ls)
	,user:call(Ls)
	,debug(metasubstitution,'Succeeded',[]).
metasubstitution(E,M,Sub):-
	louise:bind_head_literal(E,M,(Sub:-(E,Ls)))
	,debug_clauses(metasubstitution,'Trying metasubstitution:',Ls)
	,once(list_tree(Ls_,Ls))
	,prove_body_literals(E,Ls_,Ls_).


%!	prove_body_literals(+Head,+Body,-Metasubstitution) is nondet.
%
%	Prove a set of Body literals in a Metasubstitution.
%
%	@tbd Document this properly.
%
prove_body_literals(_H,[true],_Gs):-
	!.
prove_body_literals(H,Bs,Gs):-
	H =.. [m,_P|Ts]
	,counts(Ts,[],Cs)
	,debug_clauses(grounding,'Grounding clause',[[H|Gs]])
	,prove_body_literals(Bs,Gs,[H],Cs).

%!	prove_body_literals(?Literals,?Clause,+Current,+Constants) is
%!	nondet.
%
%	Business end of prove_body_literals/3.
%
%	Literals is the set of literals in the body of a metarule.
%	Current is the body literal that will be ground in the current
%	step by resolution with the BK. Constants is the constants
%	buffer, a list of the constatns used to ground the members of
%	Literals in previous steps associated with the number of times
%	each constant has been used to ground a variable in Literals.
%
%	Constants is used to keep track of how many times a constant is
%	used. We want to produce a grounding such that when the grounded
%	clause is lifted, to extract the structure of a specialised
%	metarule, the resulting metarule has no free variables. For this
%	to be the case, each constant in the ground clause must be used
%	at least twice.
%
%	Clause is the instance of the most-general metarule to be
%	grounded. This is only useful for debugging and should be
%	removed in later stages of development.
%
prove_body_literals([],_Gs,_Ls,Cs):-
	forall(member(_C-N,Cs)
	      ,N > 1).
prove_body_literals([Lk|Bs],Gs,Ls,Cs):-
	grounding_constraints(Cs,Gs)
	,variable_instantiations(Lk,Cs,Is)
	,debug_clauses(grounding,'Variable instantiations',[Is])
	,debug_clauses(grounding,'Grounding literal',[Lk])
	,call(Lk)
	,new_literal(Lk,Ls)
	,debug_clauses(grounding,'Ground literal',[Lk])
	,counts(Is,Cs,Cs_)
	,debug_clauses(grounding,'Constant counts',[Cs_])
	,prove_body_literals(Bs,Gs,[Lk|Ls],Cs_).


%!	grounding_constraints(+Constants,+Literals) is det.
%
%	Apply clause grounding constraints to a set of Literals.
%
%	Constants is the constants buffer, the list of constants used
%	so-far in grounding an instance of a most-general metarule and
%	their associated counts, i.e. the number of times each constant
%	is used in grounding a variable.
%
%	Literals is the partially ground instance of a most-general
%	metarule that is currently being ground.
%
%	grounding_constraints/2 fails if there are more elements of
%	Constants with a count of 1 than there are free existentially
%	quantified first-order variables in Literals.
%
%	In other words, this checks that we haven't gathered up too many
%	one-use constants to ground a most-general metarule to a
%	metarule with free variables.
%
grounding_constraints(Cs,Ls):-
	findall(N
	       ,(member(L,Ls)
		,L =.. [m,_P|As]
		,term_variables(As,Vs)
		,length(Vs,N)
		)
	       ,Ns)
	,findall(1
		,member(_-1,Cs)
		,Ss)
	,maplist(sumlist,[Ns,Ss],[F,C])
	,C =< F.


%!	variable_instantiations(+Literal,+Costants,?Variables) is
%!	nondet.
%
%	Instantiate the Variables in a Literal to a set of Constants.
%
variable_instantiations(L,Cs,Vs):-
	L =.. [m,_P|As]
	,term_variables(As,Vs)
	,member(C-_N,Cs)
	,member(C,Vs).


%!	new_literal(+Literal,+Last,+All) is det.
%
%	True when Literal is a new ground literal.
%
%	Literal is the current ground literal. Last is the literal to
%	the left of this literal, ground in the previous step. All is
%	the list of all literals ground so far.
%
%	This performs a test to ensure that we are not adding the same
%	literal twice to a clause, to avoid generating tautologies and
%	redundancies.
%
new_literal(Lk,Ls):-
	copy_term([Lk|Ls],[Lk_|Ls_])
	,length([Lk_|Ls_],N)
	,setof(L
	     ,(member(L,[Lk_|Ls_])
	      ,numbervars(L)
	      )
	     ,Ls_s)
	,length(Ls_s,N).


%!      counts(+Terms,?Counts,-Updated) is det.
%
%       Count or update the Counts of a list of Terms.
%
counts(Ts,Cs,Cs_u):-
        sort(0,@=<,Ts,Ts_)
        ,counts_(Ts_,Cs,Cs_)
        ,findall(C-N
                ,order_by([asc(N)]
                         ,member(C-N,Cs_)
                         )
                ,Cs_u).

%!      counts(+Terms,?Counts,+Acc,-Updated) is det.
%
%       Business end of counts/3
%
counts_([],Cs,Cs).
counts_([T1|Ts],[],Bind):-
% Empty counts- initialise.
        !
        ,counts_(Ts,[T1-1],Bind).
counts_([T|Ts],[T-C|Cs],Bind):-
% Increment count of T.
        !
        ,succ(C,C_)
        ,counts_(Ts,[T-C_|Cs],Bind).
counts_([Ti|Ts],[Tk-C|Cs],Bind):-
% Find current count of T and update.
        Ti \== Tk
        ,selectchk(Ti-Ci,Cs,Cs_)
        ,!
        ,succ(Ci,Ci_)
        ,counts_(Ts,[Ti-Ci_,Tk-C|Cs_],Bind).
counts_([Ti|Ts],[Tk-C|Cs],Bind):-
% Start a count for new term.
        Ti \== Tk
        ,counts_(Ts,[Ti-1,Tk-C|Cs],Bind).


%!	new_metarule(+Metarule,+Ground_Metasub,-New) is det.
%
%	Rename a metarule.
%
%	Renames all metarules that are specialisations of a most-general
%	metarule after that most-general metarule.
%
%	Needed because we want to sort metarules to remove duplicates
%	and that is easier to do if they all have the same name.
%
new_metarule(M,Sub_g,Sub:-Ls):-
	lifted_program([M],[Sub:-Ls])
	,Sub_g =.. [m,Id|_Ps_g]
	,Sub =.. [m,Id|_Ps].
