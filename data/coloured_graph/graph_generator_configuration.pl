:-module(graph_generator_configuration, [background_knowledge/1
                                        ,mislabelling_probability/1
                                        ,mislabelling_type/1
                                        ,metarules/1
                                        ,output_root/1
                                        ,target_prefix_arity/2
                                        ]).


%!      background_knowledge(-Predicates) is semidet.
%
%       Symbols of background knowledgePredicates for connected/2.
%
%       Used to add background knowledge declarations to experiment
%       files for generated conneced/2 MIL problerms.
%
background_knowledge([ancestor/2
                     %,parent/2
                     %,child/2
                     %,blue_child/2
                     %,red_child/2
                     %,blue_parent/2
                     %,red_parent/2
                     %,blue/1
                     %,red/1
                     ]).


%!      metarules(-Ids) is semidet.
%
%       IDs of metarules for connected/2 problem.
%
%       Used to add metarule declarations to experiment files for
%       generated conneced/2 MIL problerms.
%
metarules([identity,inverse,switch,swap]).
%metarules([chain,tailrec,precon]).
%metarules([chain,tailrec,switch,precon]).


%!      mislabelling_probability(?Probability) is semidet.
%
%       Proportion of examples that are mislabelled in dataset.
%
mislabelling_probability(0.2).


%!      mislabelling_type(?Type) is semidet.
%
%       Type of mislabelled examples in generated dataset.
%
%       One of: [no_noise, ambiguities, false_positives,
%       false_negatives].
%
mislabelling_type(no_noise).
%mislabelling_type(ambiguities).
%mislabelling_type(false_positives).
%mislabelling_type(false_negatives).


%!      output_root(?Directory) is semidet.
%
%       Output directory for generated experiment files.
%
output_root('data/coloured_graph/output').


%!      target_prefix_arity(?Prefix,?Arity) is semidet.
%
%       Prefix and Arity for the generated dataset's target predicate.
%
%       The file will be suffixed with a description of the kind of
%       mislabelled examples in the experiment file.
%
target_prefix_arity('graph', 2).
