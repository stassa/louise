:-module(graph_generator, [write_dataset/0
                          ,write_dataset/2
                          ,generate_ambiguities/3
                          ,generate_false_negatives/3
                          ,generate_false_positives/3
                          ]).

:-use_module(lib(sampling/sampling)).
:-use_module(graph_generator_configuration).
:-use_module(src(auxiliaries)).

/** <module> Generator for connected/2 problems with mislabelled examples.

*/


%!      write_dataset is det.
%
%       Write a generated dataset to file.
%
%       As write_dataset/2 but mislabelling probability and type of
%       mislabelled examples are taken from the generator configuration.
%
write_dataset:-
        graph_generator_configuration:mislabelling_probability(P)
        ,graph_generator_configuration:mislabelling_type(T)
        ,write_dataset(T,P).



%!      write_dataset(+Type, +Probability) is det.
%
%       Write a generated dataset to file.
%
%       Type is one of: [ambiguities, false_positives, false_negatives].
%       It determines what kind of dataset will be generated.
%
%       Probability is the probability of mislabelling, i.e. the
%       proportion of positive or negative examples, or both, that will
%       be given erronneous labels in the generated dataset.
%
write_dataset(L,P):-
        graph_generator_configuration:background_knowledge(BK)
        ,graph_generator_configuration:metarules(IDs)
        ,graph_generator_configuration:target_prefix_arity(T,A)
        ,graph_generator_configuration:output_root(Root)
        ,init_output_dir
        ,atomic_list_concat([T,L],'_',Bn)
        ,generate_examples(L,P,Bn,Pos,Neg)
        ,file_name_extension(Bn,'.pl',Fn)
        ,directory_file_path(Root,Fn,Path)
        ,exported_predicates(BK,Es)
        ,Set = open(Path,write,S,[alias(path)])
        ,G = (write_module_header(S,Bn,Es)
             ,write_bk_declaration(S,Bn/A,BK)
             ,write_metarules_declaration(S,Bn/A,IDs)
             ,write_positive_examples(S,Bn/A,Pos)
             ,write_negative_examples(S,Bn/A,Neg)
             ,write_bk_definitions(S,BK)
             )
        ,C = close(S)
        ,setup_call_cleanup(Set,G,C).


%!	init_output_dir is det.
%
%	Initialise the dataset output directory.
%
init_output_dir:-
	graph_generator_configuration:output_root(R)
	,(   \+ exists_directory(R)
	 ->   make_directory(R)
	 ;   true
	 ).


%!      generate_examples(+Type,+P,+Target,-Pos,-Neg) is det.
%
%       Generate examples of the given Type.
%
%       Type is one of: [ambiguities, false_positives, false_negatives].
%       It determines what kind of dataset will be generated.
%
%       P is the proportion of mislabelled examples in the generated
%       dataset.
%
%       Target is the predicate symbol _but not the arity_ of the target
%       predicate that corresponds to each Type. This is created by
%       prepending the atom specifided in the first argument of
%       thegenerator configuration option target_prefix_arity/2 to Type
%       and appending the number in the second argument of that option
%       as its arity, separated by "/".
%
%       Pos and Neg are lists of positive and negative examples of
%       Target, as expected by Louise's experiment file format. Unlike
%       the usual generators, they are atoms of the form:
%       ==
%       positive_example(T/A,T(... Args ...)).
%       ...
%       negative_example(T/A,T(... Args ...)).
%       ==
%
%       Where A is the arity of the target predicate.
%
generate_examples(no_noise,_P,T,Pos,Neg):-
        !
        ,positive_examples(Pos_)
        ,negative_examples(Neg_)
        ,maplist(rename_examples,[T,T],[Pos_,Neg_],[Pos,Neg]).
generate_examples(ambiguities,P,T,Pos,Neg):-
        !
        ,generate_ambiguities(P,Pos_,Neg_)
        ,maplist(rename_examples,[T,T],[Pos_,Neg_],[Pos,Neg]).
generate_examples(false_negatives,P,T,Pos,Neg):-
        !
        ,generate_false_negatives(P,Pos_,Neg_)
        ,maplist(rename_examples,[T,T],[Pos_,Neg_],[Pos,Neg]).
generate_examples(false_positives,P,T,Pos,Neg):-
        generate_false_positives(P,Pos_,Neg_)
        ,maplist(rename_examples,[T,T],[Pos_,Neg_],[Pos,Neg]).


%!      rename_examples(+Symbol,+Examples,-Renamed) is det.
%
%       Change the symbol of Examples to a new Symbol.
%
%       This predicate is used to change atoms of connected/2, the base
%       theory for datasets in this generator, to atoms of the target
%       predicates for each generated dataset.
%
%       @see generate_examples/5.
%
rename_examples(T,Es,Es_T):-
        findall(E_
               ,(member(E,Es)
                ,E =.. [_|As]
                ,E_ =.. [T|As]
                )
               ,Es_T).


%!      exported_predicates(+BK,-Exported) is det.
%
%       Collect predicates Exported by the experiment file module.
%
%       BK is the set of predicate symbols and arities of predicates
%       declared as background knowledge in background_knowledge/1 in
%       graph_generator_configuration.pl.
%
%       Exported is a list including the predicates in BK and the
%       predicate symbols of predicates in the transitive closure of
%       predicates in BK and sorted to the standard order of terms. In
%       other words, Exported is the set of predicate symbols
%       and arities of the background knowledge and the predicates
%       called by the background knowledge.
%
%       The latter kind of predicate is necessary to be exported
%       by the experiment file module because louise expects to find its
%       definition in the module user. write_bk_definitions/2, the
%       generator predicate that writes the background knowledge to the
%       generated experiment file, ensures that those predicates'
%       definitions are added to the generated experiment file.
%
exported_predicates(BK,Es):-
        closure(BK,graph_generator,Ps)
        ,findall(F/A
                ,(member([C|_Cs],Ps)
                 ,(   C = (H :- _B)
                  ->  functor(H,F,A)
                  ;   functor(C,F,A)
                  )
                 ,\+ memberchk(F/A, BK)
                 )
                ,Ss)
        ,append(BK,Ss,Es_)
        ,sort(Es_,Es).


%!      write_module_header(+Stream,+Module,+Exports) is det.
%
%       Write the module declaration for a generated dataset.
%
%       Stream is an open stream to the experiment file where the
%       current generated dataset is to be written.
%
%       Module is the module name of the experiment file.
%
%       Exports is a list of predicate symbols and arities of the
%       experiment file interface predicates and background knowledge
%       definitions exported by the experiment file module.
%
write_module_header(S,Bn,BK):-
        append([background_knowledge/2
               ,metarules/2
               ,positive_example/2
               ,negative_example/2
               ]
              ,BK
              ,Es)
        ,format(S,':-module(~w, ~w).~n~n',[Bn,Es]).


%!      write_bk_definition(+Stream,+Target,+Background) is det.
%
%       Write a background knowledge declaration for a dataset.
%
%       Stream is an open stream to the experiment file where the
%       current generated dataset is to be written.
%
%       Target is the predicate symbol and arity of the learning target
%       for the generated dataset.
%
%       Background is the list of predicate symbols and arities for the
%       background definitions in the experiment file.
%
write_bk_declaration(S,T,BK):-
        format(S,'background_knowledge(~w, ~w).~n~n',[T,BK]).


%!      write_metarules_declaration(+Stream,+Target,+IDs) is det.
%
%       Stream is an open stream to the experiment file where the
%       current generated dataset is to be written.
%
%       Target is the predicate symbol and arity of the learning target
%       for the generated dataset.
%
%       IDs is a list of the metarule IDs for the MIL problem in the
%       experiment file.
%
write_metarules_declaration(S,T,IDs):-
        format(S,'metarules(~w,~w).~n~n',[T,IDs]).


%!      write_positive_examples(+Stream,+Target,+Examples) is det.
%
%       Write positive examples of Target to Stream
%
%       Stream is an open stream to the experiment file where the
%       current generated dataset is to be written.
%
%       Target is the predicate symbol and arity of the learning target
%       for the generated dataset.
%
%       Examples is a list of positive examples of Target.
%
%       This predicate writes each positive example to the experiment
%       file Stream as an atom of the form:
%       ==
%       positive_example(T/A,T(... Args ...)).
%       ==
%
%       Where T/A is the predicate symbol and arity in Target.
%
write_positive_examples(S,T,Pos):-
        findall(A
               ,(member(E,Pos)
                ,format(atom(A),'positive_example(~w,~w)',[T,E]))
               ,Pos_)
        ,write_terms(S,Pos_)
        ,format(S,'~n',[]).


%!      write_negative_examples(+Stream,+Target,+Examples) is det.
%
%       Write negative examples of Target to Stream
%
%       Stream is an open stream to the experiment file where the
%       current generated dataset is to be written.
%
%       Target is the predicate symbol and arity of the learning target
%       for the generated dataset.
%
%       Examples is a list of negative examples of Target.
%
%       This predicate writes each negative example to the experiment
%       file Stream as an atom of the form:
%       ==
%       negative_example(T/A,T(... Args ...)).
%       ==
%
%       Where T/A is the predicate symbol and arity in Target.
%
write_negative_examples(S,T,Neg):-
        findall(A
               ,(member(E,Neg)
                ,format(atom(A),'negative_example(~w,~w)',[T,E]))
               ,Neg_)
        ,write_terms(S,Neg_)
        ,format(S,'~n',[]).


%!      write_bk_definitions(+Stream,+Background) is det.
%
%       Write definitions of Background predicates to Stream.
%
%       Stream is an open stream to the experiment file where the
%       current generated dataset is to be written.
%
%       Background is a list of symbols and arities of the background
%       predicates in the MIL problem for that dataset.
%
%       This predicate collects the definitions of predicates in
%       Background defined in the current module (i.e.
%       graph_generator.pl) and the predicates in their closure and
%       writes each definition to the experiment file in Stream.
%
write_bk_definitions(S,BK):-
        closure(BK,graph_generator,Bs)
        ,forall(member(Cs,Bs)
               ,(numbervars(Cs)
                ,write_terms(S,Cs)
                ,nl(S)
                )
               ).


%!	write_terms(+Stream,+Atoms) is det.
%
%	Write a list of atoms to a Stream.
%
write_terms(S,As):-
	forall(member(A,As)
	      ,(write_term(S,A,[fullstop(true)
			       ,nl(true)
			       ,numbervars(true)
			       ]
			  )
	       )
	      ).


%!      generate_ambiguities(+Probability,-Positive,-Negative) is det.
%
%       Generate a set of ambiguous Positive and Negative examples.
%
%       Probability is a float between 0.0 and 1.0 that determines the
%       proportion of ambiguous examples in Positive and Negative.
%
%       When Probability is 0.0, Positive and Negative are lists of
%       positive and negative examples, respectively, generated by
%       positive_examples/1 and negative_examples/1.
%
%       When Probability is more than 0.0, a proportion of positive
%       examples equal to Probability are added to Negatives and an
%       equal proportion of negative examples are added to Positive,
%       thereby creating "ambiguities". "Ambiguities" in this sense are
%       examples that are labelled as both positive and negative.
%
%       Ambiguities are selected at random and without replacement by a
%       call to p_list_samples/3.
%
%       The following is a useful diagnostic query for this predicate:
%       ==
%       ?- generate_ambiguities(0.05,_Pos,_Neg), intersection(_Pos,_Neg,_Is),
%        maplist(length,[_Pos,_Neg,_Is],[N,M,K]), print_clauses('Ambiguous:',_Is).
%	Ambiguous:
%	connected(a,f).
%	connected(h,j).
%	connected(i,l).
%	connected(k,i).
%	connected(l,k).
%	connected(f,c).
%	connected(h,b).
%	connected(h,l).
%	connected(l,h).
%	N = 112,
%	M = 79,
%	K = 9.
%       ==
%
generate_ambiguities(0,Pos,Neg):-
        !
        ,positive_examples(Pos)
        ,negative_examples(Neg).
generate_ambiguities(P,Pos,Neg):-
        positive_examples(Pos_Es)
        ,negative_examples(Neg_Es)
        ,maplist(p_list_samples,[P,P],[Pos_Es,Neg_Es],[Pos_amb,Neg_amb])
        ,maplist(append,[Pos_Es,Neg_Es],[Neg_amb,Pos_amb],[Pos_,Neg_])
        % Sort to reorder. There should be no duplicates at this point.
        ,maplist(sort,[Pos_,Neg_],[Pos,Neg]).



%!      generate_false_negatives(+P,-Positive,-Negative) is det.
%
%       Generate Positive and Negative examples with false negatives.
%
%       "False negative" examples are positive examples mislabelled as
%       negative.
%
%       Probability is a float between 0.0 and 1.0 that determines the
%       proportion of positive examples mislabelled as negative.
%
%       When Probability is 0.0, Positive and Negative are lists of
%       positive and negative examples, respectively, generated by
%       positive_examples/1 and negative_examples/1.
%
%       When Probability is more than 0.0, a proportion of positive
%       examples equal to Probability is removed from the set of
%       positive examples and added to Negative. This reduces the number
%       of examples in Positive!
%
%       The following is a useful diagnostic query for this predicate:
%       ==
%	?- generate_false_negatives(0.1,_Pos,_Neg)
%	,setof(connected(A,B),_Neg^(member(connected(A,B),_Neg)
%	,graph_generator:connected(A,B)),_FNs)
%	,print_clauses('False negatives:',_FNs)
%	,intersection(_Pos,_Neg,_Is)
%	,maplist(length,[_Pos,_Neg,_FNs,_Is],[N,M,K,J]).
%	False negatives:
%	connected(a,j).
%	connected(a,l).
%	connected(c,a).
%	connected(f,h).
%	connected(g,a).
%	connected(i,j).
%	connected(l,b).
%	connected(l,d).
%	connected(m,b).
%	connected(m,e).
%	connected(n,j).
%	N = 97,
%	M = 85,
%	K = 11,
%	J = 0.
%       ==
%
generate_false_negatives(0,Pos,Neg):-
        !
        ,positive_examples(Pos)
        ,negative_examples(Neg).
generate_false_negatives(P,Pos,Neg):-
        positive_examples(True_Pos)
        ,negative_examples(True_Neg)
        ,p_list_partitions(P,True_Pos,False_Neg,Pos)
        ,append(True_Neg,False_Neg,Neg_)
        % Sort to reorder- there should be no duplicates.
        ,sort(Neg_,Neg).



%!      generate_false_positives(+P,-Positive,-Negative) is det.
%
%       Generate Positive and Negative examples with false positives.
%
generate_false_positives(0,Pos,Neg):-
        !
        ,positive_examples(Pos)
        ,negative_examples(Neg).
generate_false_positives(P,Pos,Neg):-
        positive_examples(True_Pos)
        ,negative_examples(True_Neg)
        ,p_list_partitions(P,True_Neg,False_Pos,Neg)
        ,append(True_Pos,False_Pos,Pos_)
        ,sort(Pos_,Pos).


%!      positive_examples(-Positive) is det.
%
%       Generate a set of Positive examples.
%
positive_examples(Pos):-
        setof(E
             ,positive_example(E)
             ,Pos).


%!      negative_examples(-Negatives) is det.
%
%       Generate a set of Negative examples.
%
negative_examples(Neg):-
        setof(E
             ,negative_example(E)
             ,Neg).


%!      positive_example(-Example) is nondet.
%
%       Generate each positive Example of connected/2.
%
positive_example(connected(A,B)):-
	connected(A,B)
	,\+ cycle(A,B).


%!      negative_example(-Example) is nondet.
%
%       Generate each negative example of connected/2.
%
negative_example(connected(A,B)):-
	node(A)
	,node(B)
	,\+ once(connected(A,B))
	,\+ cycle(A,B).


%========== Target theory. ====================


%!	cycle(?Node,?Node) is nondet.
%
%	A node with an edge or path connecting it to itself.
%
cycle(A,A):-
	node(A).


%!	node(?Node) is nondet.
%
%	A coloured Node in a graph.
%
node(A):-
	blue(A).
node(A):-
	red(A).


%!	connected(?Node1,?Node2) is nondet.
%
%	True when Node2 can be reached via a path starting at Node1.
%
%	Target theory for connected/2 MIL problem, used to generate
%	positive and negative examples.
%
connected(A,B):- ancestor(A,B).
connected(A,B):- ancestor(B,A).
connected(A,B):- ancestor(C,A), ancestor(C,B).
connected(A,B):- ancestor(A,C), ancestor(B,C).


%========== Background knowledge. ====================

%!	ancestor(?Ancestor,?Descendant) is nondet.
%
%	Relates an Ancestor node to each of its Descendant nodes.
%
ancestor(X,Y):-
	parent(X,Y).
ancestor(X,Y):-
	parent(X,Z)
	,ancestor(Z,Y).


%!	parent(?Parent,?Child) is nondet.
%
%	Relates a Parent node to each of its Child nodes.
%
parent(X, Y):-
	blue_parent(X,Y).
parent(X, Y):-
	red_parent(X,Y).


%!	child(?Child, ?Parent) is nondet.
%
%	Relates a Child node to each of its Parent nodes.
%
child(X,Y):-
	parent(Y,X).


%!	blue_child(?Child, ?Parent) is nondet.
%
%	Relates a blue Child node to each of its Parent nodes.
%
blue_child(X,Y):-
	blue(X)
	,child(X,Y).


%!	red_child(?Child, ?Parent) is nondet.
%
%	Relates a red Child node to each of its Parent nodes.
%
red_child(X,Y):-
	red(X)
	,child(X,Y).


%!	blue_parent(?Parent,?Child) is nondet.
%
%	Relates a blue Parent node to each of its Child nodes.
%
blue_parent(a,c).
blue_parent(a,n).
blue_parent(b,i).
blue_parent(b,d).
blue_parent(c,j).
blue_parent(d,e).
blue_parent(f,g).
blue_parent(f,h).


%!	red_parent(?Parent,?Child) is nondet.
%
%	Relates a red Parent node to each of its Child nodes.
%
red_parent(k,c).
red_parent(k,n).
red_parent(l,i).
red_parent(l,d).
red_parent(i,j).
red_parent(m,e).
red_parent(n,g).
red_parent(n,h).


%!	blue(?Node) is nondet.
%
%	A Node coloured blue.
%
blue(a).
blue(b).
blue(c).
blue(d).
blue(e).
blue(f).
blue(g).
blue(h).


%!	red(?Node) is nondet.
%
%	A Node coloured red.
%
red(i).
red(j).
red(k).
red(l).
red(m).
red(n).
