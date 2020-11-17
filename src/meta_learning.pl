:-module(meta_learning, [learn_meta/1
			,learn_meta/2
			,learn_meta/5
                        ]).

:-use_module(configuration).
:-use_module(src(metarule_extraction)).
:-use_module(lib(lifting/lifting)).

/** <module> Learning while learning new metarules.

*/


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
	     ,generalise_meta(Pos,MS,Ss_Gen,MS_)
	     ,specialise(Ss_Gen,Neg,Ss_Spec)
	     ,debug_clauses(top_program,'Specialised Top program',Ss_Spec)
	     ,applied_metarules(Ss_Spec,MS_,Ts)
	     ,debug_clauses(top_program,'Applied metarules',Ts)
	     )
	,C = erase_program_clauses(Refs)
	,setup_call_cleanup(S,G,C)
	,!.


%!	generalise_meta(+Pos,+MS,-Top,-Metarules) is det.
%
%	Generalisation step for meta-Top program construction.
%
generalise_meta(Pos,MS,Ss_Pos,MS_):-
	findall(Sub-M_n
	     ,(member(M,MS)
	      ,copy_term(M,M_)
	      ,member(Ep,Pos)
	      ,metasubstitution(Ep,M_,Sub)
	      ,constraints(Sub)
	      ,debug(new_metarules,'',[])
	      ,debug_clauses(new_metarules,'Ground metarule',[M_])
	      ,new_metarule(M_,Sub,M_n)
	      ,debug_clauses(new_metarules,'New metarule',[M_n])
	      ,numbervars(M_n)
	      )
	     ,Ss_Pos_)
	,sort(Ss_Pos_,Ss_Pos_s)
	,maplist(varnumbers,Ss_Pos_s,Ss_Pos)
	,MS_ = nil.


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
	,prove_body_literals(E,Ls_,E:-Ls).


%!	prove_body_literals(+Head,+Body,-Metasubstitution) is nondet.
%
%	Prove a set of Body literals in a Metasubstitution.
%
prove_body_literals(H,Bs,C):-
	H =.. [m,_P|Ts]
	,counts(Ts,[],Cs)
	,prove_body_literals(H,Bs,C,Cs).

prove_body_literals(_L,[],_,Cs):-
	forall(member(_C-N,Cs)
	      ,N > 1).
prove_body_literals(Li,[Lk|Bs],C,Cs):-
	variable_instantiations(Lk,Cs,Is)
	,call(Lk)
	,Lk \== Li
	,counts(Is,Cs,Cs_)
	,prove_body_literals(Lk,Bs,C,Cs_).


%!	variable_instantiations(+Literal,+Costants,?Variables) is
%!	nondet.
%
%	Instantiate the Variables in a Literal to a set of Constants.
%
variable_instantiations(L,Cs,Vs):-
	term_variables(L,[_P|Vs])
	,member(C-_N,Cs)
	,member(C,Vs).


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
