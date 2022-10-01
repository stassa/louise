:-module(toil, [learn_meta/1
	       ,learn_meta/2
	       ,learn_meta/5
	       ,learn_metarules/1
	       ,learn_metarules/2
	       ,learn_metarules/5
	       ]).

:-use_module(project_root(configuration)).
:-use_module(src(auxiliaries)).
:-use_module(subsystems(minimal_program)).
:-use_module(lib(lifting/lifting)).
:-use_module(src(mil_problem)).
:-use_module(src(louise)).
:-use_module(lib(sampling/sampling)).

/** <module> Third Order Inductive Learner.

This module implements TOIL, a MIL system that learns metarules from
data.

==
%learned_metarules_printing(pretty).

?- learn_metarules(ancestor/2).
(Meta-dyadic-1) ∃.P,P,P ∀.x,y,z: P(x,y)← P(x,z),P(z,y)
(Meta-dyadic-2) ∃.P,P,P ∀.x,y,z: P(x,y)← P(z,y),P(x,z)
(Meta-dyadic-3) ∃.P,P,Q ∀.x,y,z: P(x,y)← P(x,z),Q(y,z)
(Meta-dyadic-4) ∃.P,P,Q ∀.x,y,z: P(x,y)← P(x,z),Q(z,y)
(Meta-dyadic-5) ∃.P,P,Q ∀.x,y,z: P(x,y)← P(z,y),Q(x,z)
(Meta-dyadic-6) ∃.P,P,Q ∀.x,y,z: P(x,y)← P(z,y),Q(z,x)
(Meta-dyadic-7) ∃.P,Q,P ∀.x,y,z: P(x,y)← Q(x,z),P(z,y)
(Meta-dyadic-8) ∃.P,Q,P ∀.x,y,z: P(x,y)← Q(y,z),P(x,z)
(Meta-dyadic-9) ∃.P,Q,P ∀.x,y,z: P(x,y)← Q(z,x),P(z,y)
(Meta-dyadic-10) ∃.P,Q,P ∀.x,y,z: P(x,y)← Q(z,y),P(x,z)
(Meta-dyadic-11) ∃.P,Q,Q ∀.x,y,z: P(x,y)← Q(x,z),Q(z,y)
(Meta-dyadic-12) ∃.P,Q,Q ∀.x,y,z: P(x,y)← Q(z,y),Q(x,z)
(Meta-monadic-13) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)

%learned_metarules_printing(prolog).

?- learn_metarules(ancestor/2).
m(meta_dyadic_1,P,P,P):-m(P,X,Y),m(P,X,Z),m(P,Z,Y)
m(meta_dyadic_2,P,P,P):-m(P,X,Y),m(P,Z,Y),m(P,X,Z)
m(meta_dyadic_3,P,P,Q):-m(P,X,Y),m(P,X,Z),m(Q,Y,Z)
m(meta_dyadic_4,P,P,Q):-m(P,X,Y),m(P,X,Z),m(Q,Z,Y)
m(meta_dyadic_5,P,P,Q):-m(P,X,Y),m(P,Z,Y),m(Q,X,Z)
m(meta_dyadic_6,P,P,Q):-m(P,X,Y),m(P,Z,Y),m(Q,Z,X)
m(meta_dyadic_7,P,Q,P):-m(P,X,Y),m(Q,X,Z),m(P,Z,Y)
m(meta_dyadic_8,P,Q,P):-m(P,X,Y),m(Q,Y,Z),m(P,X,Z)
m(meta_dyadic_9,P,Q,P):-m(P,X,Y),m(Q,Z,X),m(P,Z,Y)
m(meta_dyadic_10,P,Q,P):-m(P,X,Y),m(Q,Z,Y),m(P,X,Z)
m(meta_dyadic_11,P,Q,Q):-m(P,X,Y),m(Q,X,Z),m(Q,Z,Y)
m(meta_dyadic_12,P,Q,Q):-m(P,X,Y),m(Q,Z,Y),m(Q,X,Z)
m(meta_monadic_13,P,Q):-m(P,X,Y),m(Q,X,Y)
==

A metarule taxonomy
-------------------

Metarules are the second-order clauses used as inductive bias in
Meta-Interpretive Learning (MIL) systems, such as Louise or Metagol. MIL
systems learn by specialising metarules by SLD resolution to derive the
first-order clauses of a hypothesis.

TOIL is a MIL system and it also works by specialising metarules by
SLD resolution, however the metarules specialised by TOIL are
generalisations of the metarules in ordinary MIL.

We distinguish three taxa of metarules. We illustrate them with an
example:

==
% Example 1: a taxonomy of metarules.

% User-defined second-order "sort" metarules:
P(x,y):- Q(x,y).
P(x,y):- Q(y,x).
P(x,y):- Q(x,z), R(z,y).
P(x,y):- Q(x,z), R(y,z).
P(x,y):- Q(z,x), R(z,y).
...

% Maximally general second-order "matrix" metarules:
P(x,y):- Q(z,u).
P(x,y):- Q(z,u), R(v,w).

% Third-order "punch" metarules:
P:- Q.
P:- Q, R.
==

"Sort" metarules are second-order datalog clauses where each first-order
variable is shared between at least two literals and where each variable
in a head literal is shared with a body literal.

"Matrix" metarules are generalisations of sort metarules where each
variable appears only once in the entire clause. Matrix metarules are
the most general second-order metarules in a language class (defined as
a set of metarules with a certain number of literals, and arities).

"Punch" metarules are generalisations of matrix metarules where each
literal is replaced by a variable. Punch metarules are third-order
clauses with variables quantified over the set of atoms (in the
first-order logic sense of atomic formulae, rather than the Prolog
sense).

How TOIL learns metarules
-------------------------

Note that in Example 1, showing a taxonomy of metarules, the matrix
metarules are generalisations of the sort metarules and the punch
metarules generalisations of the matrix metarules. This relation between
the three taxa of metarules is exploited by TOIL to specalise punch or
matrix metarules into sort metarules.

TOIL uses the Top Program Construction (TPC) algorithm, also used in
Louise and its other subsystems, to learn sort metarules from examples,
background knowledge and sets of either matrix or punch metarules.

The TPC algorithm learns by refuting positive examples by SLD-resolution
with metarules and background knowledge. If refutation fails, thus
proving an example, the substitutions of the second-order variables in
each metarule that successfully resolved with the example are kept and
used to construct a first-order clause (this clause entails the refuted
example, in conjunction with the background knowledge). TOIL also keeps
the substitutions of the first-order variables derived during
SLD-resolution, which gives a fully-ground first order clause. The
predicate symbols and constants in this fully-ground first order clause
are then replaced by variables to produce a second-order clause.


Using TOIL to learn metarules
-----------------------------

To learn sort metarules by TOIL, the user must still choose a set of
matrix or punch metarules, alongside the background knowledge and
examples of a learning target. However, selecting punch or matrix
metarules reduces the burden on the user to "guess" at the structure of
the target theory.

Learning sort metarules from matrix metarules frees the user from the
need to closely tailor the metarules in a MIL problem to the expected
solution. Matrix metarules can be used with TOIL when the user has some
intuition about the numbers of literals and arities in clauses of the
solution to a MIL problem.

Learning sort metarules from punch metarules needs the user to set only
a lower and upper limit of literals in the learned sort metarules. These
numbers correspond to the number of literals in punch metarules, for
example the punch metarules "P:-Q", "P:-Q,R" and "P:-Q,R,S" can be
specialised to any sort metarules with two, three, or four literals.

Although all the examples shown here use punch metarules to learn sort
metarules whose literals have always arity 2, punch metarules can be
specialised to metarules with literals of arbitrary arities.

Toil can be used in two "modes", implemented by the two families of
predicates defined in this module, learn_metarules/[1,2,5] and
learn_meta/[1,2,5].

In the first mode, TOIL is used to suggest metarules. TOIL is trained
with matrix or punch metarules and outputs a list of metarules. The user
selects the metarules he or she wants to use and manually adds them to
an experiment file, then learns a program with one of Louise's learning
predicates.

In the second mode, TOIL is trained with matrix or punch metarules and
imediatelly passes the learned metarules to one of Louise's learning
predicates, then outputs a first-order hypothesis. These two modes are
described below.

First mode: learning new metarules
----------------------------------

Predicates learn_metarules/[1,2,5] learn new metarules by specialising
sets of punch or matrix metarules. Currently, only one kind of
metarules can be used by TOIL at a time, either only punch or only
matrix metarules.

Below is an example of using learn_metarules/1 to specialise the matrix
metarule Meta-dyadic and learn a new sort metarule from the examples and
background knowledge in data/examples/anbn.pl:

==
% Example 2: specialising matrix metarules with learn_metarules/1.

?- list_config.
depth_limits(2,1)
example_clauses(call)
experiment_file(data/examples/anbn.pl,anbn)
generalise_learned_metarules(false)
learned_metarules_printing(pretty)
learner(louise)
learning_predicate(learn_dynamic/1)
max_invented(1)
metarule_learning_limits(metasubstitutions(1))
minimal_program_size(2,inf)
recursive_reduction(false)
reduce_learned_metarules(false)
reduction(plotkins)
resolutions(5000)
theorem_prover(resolution)
unfold_invented(false)
true.

?- list_mil_problem('S'/2).
Positive examples
-----------------
'S'([a,b],[]).
'S'([a,a,b,b],[]).
'S'([a,a,a,b,b,b],[]).

Negative examples
-----------------
[]

Background knowledge
--------------------
A/2:
'A'([a|A],A).

B/2:
'B'([b|A],A).

Metarules
---------
(Meta-dyadic) ∃.P,Q,R ∀.x,y,z,u,v,w: P(x,y)← Q(z,u),R(v,w)
true.

?- learn_metarules('S'/2).
(Meta-dyadic-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
true.
==

Note that the Meta-dyadic-1 metarule learned in Example 2 corresponds to
the Chain metarule that is defined by hand in the anbn.pl experiment
file. Chain is a specialisation of Meta-dyadic, the most-general
metarule in the M(l=3,a=2) language, of metarules with exactly three
literals each of arity exactly 2.

The following is an example of using learn_metarules/1 this time with a
punch metarule, again to learn a sort metarule from the data in
data/examples/anbn.pl:

==
% Example 3: specialising punch metarules with learn_metarules/1.

?- list_mil_problem('S'/2).
Positive examples
-----------------
'S'([a,b],[]).
'S'([a,a,b,b],[]).
'S'([a,a,a,b,b,b],[]).

Negative examples
-----------------
[]

Background knowledge
--------------------
A/2:
'A'([a|A],A).

B/2:
'B'([b|A],A).

Metarules
---------
(TOM-2) ∃.P,Q: P ← Q
(TOM-3) ∃.P,Q,R: P ← Q,R
true.

?- learn_metarules('S'/2).
(Hom-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
true.
==

Note that the specialised metarule HOM-1 again corresponds to Chain.


Second mode: learning with learned metarules
--------------------------------------------

In the previous section we have shown examples of using TOIL to output
metarules in Louise's high-level, user-friendly format, where metarules
are represented as atoms. TOIL can also ouptut metarules in the internal
format used by Louise's learning predicates such as learn/5 or
learn_dynamic/5. This is controlled with a configuration option:

==
% Example 4: outputting learned metarules as Prolog clauses.

% configuration:learned_metarules_printing(prolog).

?- learn_metarules('S'/2).
m(meta_dyadic_1,P,Q,R):-m(P,X,Y),m(Q,X,Z),m(R,Z,Y)
true.
==

The learn_meta/[1,2,5] family of predicates, defined in this module, can
be used to learn a set of sort metarules by TOIL and immediately pass
them to the learning predicate set in the configuration: The following
is an example of calling learn_meta/1 with the learning predicate set
to learn_dynamic/1, to perform predicate invention:

==
% Example 5: using learn_meta/1.

% configuration:learning_predicate(learn_dynamic/1).

?- learn_meta('S'/2).
'$1'(A,B):-'S'(A,C),'B'(C,B).
'S'(A,B):-'A'(A,C),'$1'(C,B).
'S'(A,B):-'A'(A,C),'B'(C,B).
true.
==

Limitations of TOIL
-------------------

TOIL is still a prototype and so has some limitations.

1. Predicate invention

Currently TOIL is implemented using a special-purpose version of the TPC
algorithm that does not perform predicate invention. This limits the
capability of TOIL to learn metarules with arbitrary numbers of
literals. However, metarules learned by TOIL can still be used to learn
programs that can't be learned without predicate invention. In examples
2-5 above, we have shown how TOIL learns the Chain metarule from
examples of the a^nb^n language in data/examples/anbn.pl. The Chain
metarule suffices for TPC to learn a full grammar of a^nb^n by inventing
a new predicate, consisting of a single clause that is an instance of
Chain.

2. Over-generation

TOIL tends to over-generate metarules, as can be seen in the following
example. The example only lists the configuration options that are
relevant to TOIL:

==
% Example 6: over-generation of metarules

?- list_config.
% ...  other configs
generalise_learned_metarules(false)
learned_metarules_printing(pretty)
metarule_learning_limits(none)
reduce_learned_metarules(false)
% ... other configs

?- learn_metarules('S'/2).
(Hom-1) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Hom-2) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(z,y),R(x,z)
true.
==

In particular, note the value "false" set for the option
metarule_learning_limits/1. This can be used to restrict the
over-generation of metarules by TOIL. See the structured documentation
for metarule_learning_limits/1 for more details.

3. Overfitting

Alongside over-generation, TOIL can also _overfit_ to examples and so
learn metarules over-specialised to a set of examples (and to the
background knowledge). The following example illustrates this:

==
% Example 7: Overfitting to the examples

?- learn_meta(ancestor/2).
ancestor(stathis,stassa).
ancestor(stefanos,stassa).
ancestor(alexandra,stassa).
ancestor(paraskevi,stassa).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),parent(B,C).
true.
==

In Example 7, the last clause defines an ancestor as the ancestor of
someone's children. This means that, for example, a person's mother and
father are ancestors of each other- a very special case of ancstry
(though not unheard of in science fiction).

Overfitting can be controlled by providing more negative training
examples, but this means that more examples are needed overall.
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
%	Meta-learn a Definition of one or more learning Targets.
%
learn_meta(Ts,_Ps):-
	(   \+ ground(Ts)
	->  throw('learn_meta/2: non-ground target symbol!')
	;   fail
	).
learn_meta(Ts,Ps):-
	tp_safe_experiment_data(Ts,Pos,Neg,BK,MS)
	,learn_meta(Pos,Neg,BK,MS,Ps).



%!	learn_meta(+Pos,+Neg,+BK,+Meta,-Program) is det.
%
%	Learn a Program while learning a set of new metarules.
%
%	Combines learning of new metarules, as in learn_metarules/[1,2,5],
%	with a call to another learning predicate in Louise.
%
%	@tbd Currently only learn/5 can be the learning predicate.
%
learn_meta([],_Neg,_BK,_MS,_Ts):-
	throw('learn_meta/5: No positive examples found. Cannot train.').
learn_meta(Pos,Neg,BK,MS,_Ts):-
	(   var(Pos)
	->  throw('learn_meta/5: unbound positive examples list!')
	;   var(Neg)
	->  throw('learn_meta/5: unbound negative examples list!')
	;   var(BK)
	->  throw('learn_meta/5: unbound background symbols list!')
	;   var(MS)
	->  throw('learn_meta/5: unbound metarule IDs list!')
	;   fail
	).
learn_meta(Pos,Neg,BK,MS_G,Ps):-
	debug(learn,'Encapsulating problem...',[])
	,encapsulated_problem(Pos,Neg,BK,MS_G,[Pos_,Neg_,BK_,MS_])
	,debug(learn,'Learning new metarules...',[])
	,learn_metarules(Pos_,Neg_,BK_,MS_,MS_n)
	,debug(learn,'Calling learning predicate...',[])
	,(   configuration:learning_predicate(F/A)
	    ,\+ F/A == learn_meta/A
	 ->  learning_query(Pos_,Neg_,BK_,MS_n,Ps)
	 ;   learn(Pos_,Neg_,BK_,MS_n,Ps)
	 ).


%!	learn_metarules(+Targets) is det.
%
%	Derive specialised metarules from a list of learning Targets.
%
%	New metarules are printed at the top-level. The printing can be
%	controlled by setting the value of the configuration option
%	new_metarules_printing/1 to "pretty" or "prolog".
%
%	Option "pretty" invokes the preint_quantified_metarules/1
%	pretty-printer to print learned metarules in a human-readable
%	format with quantifiers. Use this option when you want to
%	inspect the metarules learned with this predicate (and its
%	higher-arity brethren).
%
%	Option "prolog" invokes the less pretty printer
%	print_metarules/1 that prints learned metarules in their
%	encapsulated form as Prolog terms, but with variables renamed
%	according to their existential or universal quantification to
%	make them easier to read. Metarules printed to the top-level
%	with option "prolog" can be passed to a top-program construction
%	predicate like top_program/5 to learn a hypothesis. Use this
%	option when you want to inspect the metarules learned with this
%	predicate before passing them to a top-program construction
%	predicate.
%
learn_metarules(Ts):-
	learn_metarules(Ts,MS)
	,print_metarules(MS).



%!	learn_metarules(+Targets,-Metarules) is det.
%
%	Derive specialised Metarules from a list of learning Targets.
%
learn_metarules(Ts,MS):-
	tp_safe_experiment_data(Ts,Pos,Neg,BK,MS_G)
	,learn_metarules(Pos,Neg,BK,MS_G,MS).


%!	learn_metarules(+Pos,+Neg,+BK,+Templates,-Metarules) is det.
%
%	Derive specialised Metarules from the elements of a MIL problem.
%
%	Templates is a list of punch or matrix metarules. Metarules is
%	a set of specialisations of the metarules in Templates.
%
learn_metarules(Pos,Neg,BK,MS,MS_f):-
	configuration:reduce_learned_metarules(B)
	,Ci = c(1)
	,debug(learn_metarules,'Encapsulating problem...',[])
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
	,debug(learn_metarules,'Specialising metarule templates...',[])
	,specialised_metarules_(Pos_,Neg_,BK_,MS_,MS_n)
	,(   B == true
	 ->  debug(learn_metarules,'Reducing learned metarules...',[])
	    ,program_reduction(MS_n,MS_r,_MS_d)
	 ;   B == false
	 ->  MS_r = MS_n
	 )
	,maplist(renamed_metarule(Ci),MS_r,MS_f)
	,debug_msg_metarules(learned_metarules,'Learned metarules:',MS_f).


%!	specialised_metarules_(+Pos,+Neg,+BK,+General,-Special) is det.
%
%	Setup helper for specialised_metarules/5.
%
%	Responsible for collecting the Herbrand signature, writing the
%	elements of the MIL problem to the dynamic database to take
%	advantage of Prolog's SLD resolution (rather than a
%	meta-interpreter) and then cleaning up on exit, including via
%	exception. Also sets up a call to specialised_metarules/7 with
%	sampling from Pos if the configuration option
%	metarule_learning_limits/1 is set to sampling(R) (where R some
%	sampling rate).
%
specialised_metarules_(Pos,Neg,BK,MS,MS_):-
	configuration:metarule_learning_limits(L)
	,examples_targets(Pos,Ts)
	,herbrand_signature(Ts,Ss)
        ,S = write_problem(user,[Pos,BK],Refs)
	,(   L = sampling(R)
	 ->  pk_list_samples(R,Pos,Pos_)
	    ,G = specialised_metarules(none,Pos_,Neg,MS,Ss,[],MS_)
	 ;   G = specialised_metarules(L,Pos,Neg,MS,Ss,[],MS_)
	 )
	,C = erase_program_clauses(Refs)
	,setup_call_cleanup(S,G,C)
	,!.
specialised_metarules_(_Pos,_Neg,_BK,_MS,[]):-
% If Top program construction fails return an empty program.
	debug(learn_metarules,'INSUFFICIENT DATA FOR MEANINGFUL ANSWER',[]).


%!	specialised_metarules_(+Limits,+Pos,+Neg,+General,+Signature,+Acc,-Special)
%!	is det.
%
%	Specialise a list of most General metarules.
%
%	Each metarule in General is passed to specialised_metarule/6 for
%	specialisation with respect to the positive and negative
%	examples in Pos and Neg. Note that General may be a list of
%	numbers, denoting the number of literals (head and body) in a
%	higher-order metarule. Iterating over the number of literals
%	essentially implements a depth-first search with iterative
%	deepening over the length of a metarule (its number of
%	literals).
%
%	Limits is the value of metarule_learning_limit/1. This is used
%	to restrict the construction of new metarules.
%
%	Signature is the Herbrand signature, a list of encapsulated
%	second-order atoms m(P,V1,...,Vn) each of which unifies with
%	the head literal of a predicate in the BK or the positive
%	examples.
%
specialised_metarules(_L,_Pos,_Neg,[],_Ss,MS,MS):-
	!.
specialised_metarules(L,Pos,Neg,[M|MS],Ss,Acc,Bind):-
	debug_clauses(learn_metarules,'Specialising metarule template:',[M])
	,specialised_metarule(L,Pos,Neg,M,Ss,Acc,Acc_)
	,specialised_metarules(L,Pos,Neg,MS,Ss,Acc_,Bind).



%!	specialised_metarules(+Limits,+Pos,+Neg,+Template,+Signature,+Acc,-Special)
%!	is det.
%
%	Specialise one most General metarule.
%
%	Template is either a most-general metarule in a language class,
%	like meta-dyadic, the most general metarule in H(2,2) etc, or an
%	integer, denoting the number of literals in a higher-order
%	metarule.
%
%	Signature is the Herbrand signature.
%
%	Clauses of specialised_metarule/7 are seleced according to the
%	value of Limits, set in the configuration option
%	metarule_learning_limits/1. This constraints the number of
%	metarules that will be constructed. Option "none" allows
%	unconstrained construction. Option "coverset" implements a kind
%	of a coverset algorithm that removes from Pos all examples
%	entailed by the last metarule learned, then learns a new one
%	from the remaining examples. Option "sampling(R)" first samples
%	with a sampling rate of R from the positive examples (actually
%	done in specialised_metarules_/5). Option "metasubstitutions(N)"
%	only allows N metasubstitutions of Template to be derived.
%
specialised_metarule(coverset,[],_Neg,_M,_Ss,MS,MS):-
	!.
specialised_metarule(coverset,[E|Pos],Neg,M,Ss,Acc,Bind):-
	debug_clauses(learn_metarules,'New example:',[E])
	,meta_grounding(E,Neg,M,Ss,_Sub,M_n)
	,generalise_second_order(M_n,M_g)
	,encapsulated_metarule(M_g,C)
	,!
	,reduced_examples(Pos,C,Pos_)
	,maplist(length,[Pos,Pos_],[N,N_])
	,debug(learn_metarules,'Reduced ~w examples to ~w',[N,N_])
	,specialised_metarule(coverset,Pos_,Neg,M,Ss,[M_g|Acc],Bind).
specialised_metarule(coverset,[_E|Pos],Neg,M,Ss,Acc,Bind):-
	specialised_metarule(coverset,Pos,Neg,M,Ss,Acc,Bind).

specialised_metarule(N,Pos,Neg,M,Sig,Acc,MS_):-
	B = (member(Ep,Pos)
	    ,debug_clauses(learn_metarules,'New example:',[Ep])
	    ,meta_grounding(Ep,Neg,M,Sig,_Sub,M_n)
	    ,generalise_second_order(M_n,M_g)
	    ,debug_clauses(learn_metarules,'Generalised metarule:',[M_g])
	    ,numbervars(M_g)
	    )
	,(   N = none
	 ->  findall(M_g,B,Ss_)
	 ;   N = metasubstitutions(N_)
	    ,integer(N_)
	 ->  findnsols(N_,M_g,B,Ss_)
	 )
	,sort(Ss_,Ss_s)
	,maplist(varnumbers,Ss_s,MS)
	,flatten([Acc,MS],MS_)
	,debug_clauses(learn_metarules,'New metasubstitutions:',MS_).



%!	encapsulated_metarule(+Metarule,-Body) is det.
%
%	Extract the encapsulated Body literals of a Metarule.
%
encapsulated_metarule(_Sub:-(H,B),H:-B):-
	!.
encapsulated_metarule(_Sub:-L,L).


%!	renamed_metarule(+Counter,+Metarule,-Renamed) is det.
%
%	Rename a new Metarule.
%
%	Counter is a Prolog term c(I) where I is an integer, counting
%	the metarules renamed so-far. Metarule is an encapsulated
%	metarule with a the name of either a) a most-general metarule,
%	e.g. "meta_dyadic", "meta_monadic", etc, or b) the higher-order
%	metarule "hom". Renamed is the same encapsulated metarule with
%	its id replaced by <id>_I where <id> is the original name of the
%	metarule.
%
%	So for example, after renaming 3 instances of any three
%	metarules, if Metarule is an instance of meta-dyadic, this
%	fourth metarule will be renamed to "meta_dyadic_4".
%
%	The reason for this predicate's existence is that we want to be
%	able to sort learned metarules by skolemising them to ignore the
%	age of variables. However, if metarles that are instances of the
%	same template metarule are named apart, they cannot be
%	recognised as identical during sorting and so duplicates will
%	not be removed correctly. So we leave the instances of metarule
%	templates named after the templates until we have to output
%	them, at which point we rename them with this predicate.
%
%	@tbd To be honest, I'm not sure who is sorting learned metarules
%	after all. I think I've seen duplicates in the output, so it's
%	possible nothing is. Well then something should.
%
renamed_metarule(C,Sub:-M,Sub_:-M):-
	C = c(I)
	,Sub =.. [m,Id|Ps]
	,atomic_list_concat([Id,I],'_',Id_i)
	,Sub_ =.. [m,Id_i|Ps]
	,succ(I,I_)
	,nb_setarg(1,C,I_).


%!	generalise_second_order(+Metarule,-Generalised) is det.
%
%	Generalise the second-order variables in a Metarule.
%
%	Metarule is an encapsulated metarule. Generalised is the same
%	metarule with each second-order variables named apart, i.e.
%	replaced with a free variable.
%
%	For example, if Metarule is ['P(x,y):- Q(x,z), P(z,y)"],
%	Generalised would be [P(x,y):- Q(x,z), R(z,y)]. Note that the
%	two instances of P have been named apart and the last body
%	literal now has a free second order variable, R.
%
%	The motivation for this operation is to allow learning of
%	metarules that are a little more general in the sense that they
%	are not too tightly bound to the training examples and may be
%	better for representing unseen examples. Note that learning
%	predicates are capable of specialising metarules futher, by
%	binding their second-order variables together, for example
%	recursive clauses can be constructed as instances of the Chain
%	metarule having their first and last second-order variable
%	bound together. The opposite is not true, for example it's not
%	possible to generalise Tailrec so as to construct non-tail
%	recursive clauses as its instances by Top program construction
%	alone, hence the need for this prediate.
%
generalise_second_order(M,M):-
	configuration:generalise_learned_metarules(false)
	,!.
generalise_second_order(Sub:-M,Sub_g:-M_g):-
	configuration:generalise_learned_metarules(true)
	,!
	,Sub =.. [m,Id|Ps]
	,once(list_tree(Ms,M))
	,debug(learn_metarules,'Generalising second-order variables...',[])
	,generalise_second_order(Ps,Ms,[],Ps_,[],Ms_g)
	,once(list_tree(Ms_g,M_g))
	,Sub_g =.. [m,Id|Ps_].
generalise_second_order(_,_):-
	configuration:generalise_learned_metarules(O)
	,\+ memberchk(O,[true,false])
	,throw('Unknown generalise_learned_metarules/1 option':O).

%!	generalise_second_order(+SO,+Lits,Acc1,-New_SO,+Acc2,-New_Lits)
%!	is det.
%
%	Business end of generalise_second_order/2.
%
%	SO is a list of the second-order variables in a metarule and
%	Lits is the list of body literals in the metarule. New_SO and
%	New_Lits are the lists SO and Lits with second-order variables
%	replaced with new, free variables so that all second-order
%	variables are named apart.
%
%	Note that the instances of the second-order variables in body
%	literals do unify with the ones in the encapsulated
%	metasubstitution atom in the head of the encapsulated metarule.
%	The second order variables are "unique" in the context of the
%	encapsulated literals of the metarule.
%
generalise_second_order([P|Ps],[],[P1],[P1|[P|Ps]],Ls,Ls):-
% Metarule with no body.
	!.
generalise_second_order([],[],Ps_Acc,Ps,Ls_Acc,Ls):-
	maplist(reverse,[Ps_Acc,Ls_Acc],[Ps,Ls])
	,!.
generalise_second_order([P|Ps],[L|Ls],Ps_Acc,Ps_Bind,Ls_Acc,Ls_Bind):-
	!
	,L =.. [m,P|As]
	,L_ =.. [m,P_|As]
	,generalise_second_order(Ps,Ls,[P_|Ps_Acc],Ps_Bind,[L_|Ls_Acc],Ls_Bind).



%!	meta_grounding(+E,+Neg,+Template,-Metasubstitution,-Specialised)
%!	is nondet.
%
%	Ground a metarule Template and return its Specialisation.
%
%	E is a single positive example.
%
%	Neg is a list of negative examples.
%
%	Template can be a) a most-general metarule in a language class
%	or b) an integer denoting the number of literals in a
%	higher-order metarule.
%
%	Metasubstitution is a ground metasubstitution of the learned
%	metarule, Specialised.
%
%	Specialised is a specialisation of the metarule Template.
%
%	@tbd Note that Metasubstitution is output but never used. It
%	should be removed from the list of arguments.
%
meta_grounding(Ep,Neg,M,Ss,Sub,M_n):-
	% Forget the bindings of M found below
	copy_term(M,M_)
	,metasubstitution(Ep,M_,Ss,Sub:-M_g)
	,constraints(Sub)
	,debug_clauses(metarule_grounding,'Ground instance:',[M_g])
	,new_metarule(Sub:-M_g,M_n)
	,debug_clauses(metarule_grounding,'New metarule:',[M_n])
	,\+((member(En,Neg)
	    ,metasubstitution(En,M_n,Ss,Sub)
	    )
	   )
	,debug(metarule_grounding,'Entails 0 negative examples.',[]).


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
%	@tbd There is a bit of a too-high-level of abstraction here that
%	can be confusing when debugging or calling this predicate. In
%	particulare, the form of Metasubstitution changes depending on
%	the sign of Example: it can be either a ground metasubstitution
%	atom, when Example is a negative example, or a variable to be
%	bound to an encapsulated metarule Sub:- M, where Sub is a ground
%	metasubstitution atom and M the encapsulated literals of the
%	metarule.
%
%	@tbd Does this work with arbitrary definite clause examples?
%
metasubstitution(:-E,M,_Ss,Sub):-
% Sub is a ground metasubstitution atom, m(Id,P1,...,Pn)
	!
	,louise:bind_head_literal(E,M,(Sub:-(E,Ls)))
	,debug_clauses(metasubstitution,'Trying metasubstitution:',Ls)
	% Metarule without a body.
	% TODO: Make sure not needed anymore after next clause added.
	%,Ls \= true
	,user:call(Ls)
	,debug(metasubstitution,'Succeeded.',[]).
metasubstitution(E,M,_Ss,(Sub:-E)):-
% E is a positive example and M is an abduction metarule. See
% bind_head_literal/3. We don't need to carry on the "true" atom in the
% body and in fact this will make it fiddly to handle specialisations of
% this metarule further down the line.
	\+ integer(M)
	,louise:bind_head_literal(E,M,(Sub:-(E,true)))
	,!
	,debug_clauses(metasubstitution,'Abduce metasubstitution:',[Sub:-(E,true)]).
metasubstitution(E,M,_Ss,(Sub:-(E,Ls))):-
% M is an encapsulated metarule and Sub is yet free.
	\+ integer(M)
	,!
	,louise:bind_head_literal(E,M,(Sub:-(E,Ls)))
	,debug_clauses(metasubstitution,'Trying metasubstitution:',Ls)
	,once(list_tree(Ls_,Ls))
	,prove_body_literals(E,Ls_,Ls_)
	,debug(metasubstitution,'Succeeded.',[]).
metasubstitution(E,Max,Ss,(Sub:-Ls_)):-
% Max is an integer denoting the number of literals in a metarule to be
% learned from E.  Sub is yet free.
	%integer(Max)
	E =.. [m,_P|Ts]
	,counts(Ts,[],Cs)
	,prove_body_literals_ho(Max,Cs,Ss,[E],Ls)
	,literals_metasub(Ls,Sub)
	,once(list_tree(Ls,Ls_)).


%!	prove_body_literals(+Head,+Body,-Metasubstitution) is nondet.
%
%	Prove a set of Body literals in a Metasubstitution.
%
%	Top-level of a partial meta-interpreter performing resolution of
%	the body literals of a most-general metarule with the BK.
%
%	The meta-interpreter is "partial" in the sense that, unlike an
%	ordinary meta-interpreter, it hands over resolution to Prolog
%	(via call/1) and only manages initial instantiations of literals
%	and imposes constraints on their bindings (mostly by testing
%	after a literal has been ground by resolution, to be fair).
%
prove_body_literals(_H,[true],_Gs):-
	!.
prove_body_literals(H,Bs,Gs):-
	H =.. [m,_P|Ts]
	,counts(Ts,[],Cs)
	,debug_clauses(metarule_grounding,'Grounding clause:',[[H|Gs]])
	,prove_body_literals(Bs,Gs,[H],Cs).

%!	prove_body_literals(?Literals,?Clause,+Current,+Constants) is
%!	nondet.
%
%	Business end of prove_body_literals/3.
%
%	Literals is the set of literals in the body of a metarule.
%	Current is the body literal that will be ground in the current
%	step by resolution with the BK. Constants is the constants
%	buffer, a list of the constants used to ground the members of
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
	      ,N > 1)
	,debug_clauses(metarule_grounding,'Fully connected constants:',[Cs]).
prove_body_literals([Lk|Bs],Gs,Ls,Cs):-
	debug_clauses(metarule_grounding,'Checking grounding constraints:',[Gs])
	,grounding_constraints(Cs,Gs)
	,variable_instantiations(Lk,Cs,Is)
	,debug_clauses(metarule_grounding,'Variable instantiations:',[Is])
	,debug_clauses(metarule_grounding,'Grounding literal:',[Lk])
	,call(Lk)
	,new_literal(Lk,Ls)
	,debug_clauses(metarule_grounding,'Ground literal:',[Lk])
	,counts(Is,Cs,Cs_)
	,debug_clauses(metarule_grounding,'Constant counts:',[Cs_])
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


%!	new_literal(+Literal,+Literals) is det.
%
%	True when Literal is a new ground literal.
%
%	Literal is the current literal, the one ground in the current
%	step of meta-interpretation. Literals is the list of all
%	literals ground so far.
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


%!	new_metarule(+Ground,-Metarule) is det.
%
%	Lift a Ground clause to derive a Metarule.
%
%	Ground is a ground instance of an encapsulated most-general
%	metarule. Metarule is a specialisation of that most-general
%	metarule derived by replacing each constant and predicate symbol
%	in Ground with a variable.
%
new_metarule(Sub_g:-Ls_g,Sub:-Ls):-
	lifted_program([Sub_g:-Ls_g],[Sub:-Ls])
	,Sub_g =.. [m,Id|_Ps_g]
	,Sub =.. [m,Id|_Ps].


%!	herbrand_signature(+Targets,-Signature) is det.
%
%	Build the Herbrand Signature of a set of Targets.
%
%	The "Herbrand signature" of a set of predicates is a set of
%	second-order atoms P1(V1,...,Vn) where each Pi is an
%	existentially quantified variable ranging over the set of
%	predicate symbols and each Vi is an existentially or universally
%	quantified variable ranging over the set of constants.
%
%	Targets then is a list of predicate indicators to be used as
%	learning targets and Signature is their Herbrand signature.
%
%	Note that each memeber of Signature is encapsulated, i.e. each
%	second-order atom P(V1,...,Vn) is represented as a first-rder
%	atom m(P,V1,...,Vm).
%
%	The purpose of this predicate is to constrain the search for a
%	specialisation of a higher-order metarule to metarules that have
%	as a head literal an atom of a target predicate and as body
%	literals atoms of a target predicate or a predicate in the BK,
%	or in other words, atoms that unify with the atomic templates in
%	the Herbrand signature. Without such a constraint, the search
%	for a specialisation of a most general metarule would have to
%	search the space of all second-order definite clauses, which is
%	somewhat on the large side.
%
herbrand_signature(T,Ss):-
	\+ is_list(T)
	,herbrand_signature([T],Ss)
	,!.
herbrand_signature(Ts,Ss):-
	findall(Fi/Ai
		,(member(T,Ts)
		 ,experiment_file:background_knowledge(T,BK)
		 ,member(Fi/Ai,BK)
		 )
		,Bs)
	,append(Ts,Bs,Ps)
	,findall(P
		,(member(_Fk/Ak,Ps)
		 ,succ(Ak,Ak_)
		 ,functor(P,m,Ak_)
		 ,numbervars(P)
		 )
		,Ss_)
	,sort(Ss_, Ss_s)
	,varnumbers(Ss_s,Ss).


%!	prove_body_literals_ho(+N,+Constants,+Signature,+Acc,-Literals)
%!	is nondet.
%
%	Derive a specialisation of a higher-order metarule.
%
%	N is the number of literals (head and body) of the
%	specialisations of the higher-order metarule, P:-, that we are
%	going to consider in the current step of the search, in other
%	words it's a depth-bound for the iterative deepening search
%	performed by specialised_metarules/7 when a higher-order
%	metarule is specified.
%
%	Constants is the constants buffer, as in
%	grounding_constraints/2, a list of the constants in ground
%	literals derived so far and their instance counts.
%
%	Signature is the Herbrand signature, as returned by
%	herbrand_signature/2, a list of second-order atomic templates
%	that determine the arities of literals in the head and body of
%	the searched-for metarules.
%
%	Literals is the set of literals of a new metarule, a
%	fully-connected metarule that is a specialisation of the
%	higher-order metarule P:-.
%
%	As with prove_body_literals/4, this predicate implements a
%	partial meta-interpreter that hands resolution off to Prolog via
%	call/1 but imposes constraints and performs tests on the
%	derived atoms.
%
%	Conceptually, this predicate begins specialising the
%	higher-order metarule P:- by first binding its head literal, P,
%	to an atom of a positive example (this step is actually
%	performed by the last clause of metasubstitution/4, defined in
%	this module). The constants from the example, and their counts,
%	are placed on the constant buffer, then the number of literals
%	added to the specialisation of P:- so far is checked against N.
%	If the number of those literals is exactly N and there are no
%	free variables in the clause, the specialised metarule is
%	returned. Otherwise, the next atomic template is selected from
%	the Herbrand signature, partially instantiated to the constants
%	in the constants buffer and resolved with the BK. If resolution
%	succeeds and the uniqueness constraint imposed by new_literal/2
%	are not violated, the now fully-ground literal is added to the
%	set of literals of the specialised metarule and execution
%	continues with a new atomic template. Otherwise, a new atomic
%	template is selected from the Herbrand singature and execution
%	repeats.
%
%	The purpose of binding the length of literals to N is that all
%	specialisatiosn of P:-, of length N, are first tried before any
%	specialisations of length N+l are tried. This avoids over-long
%	and over-specialised metarules from being derived when more
%	general metarules can be derived instead.
%
%	@tbd Unlike in prove_body_literals/4, this predicate does not
%	start with a set of body literals in a most-general metarule.
%	Therefor, it can not know the number of variables in
%	literals that remain to be ground. Consequently, it cannot take
%	advantage of grounding_constraints/2, used in
%	prove_body_literals/4, to predict that the contents of the
%	constant buffer will lead to a not-fully-connected metarule. In
%	other words, this can do less prunning and may have to do more
%	work (as in backtracking more) to find a specialised metarule
%	that is fully connected.
%
prove_body_literals_ho(Max,Cs,_Ss,Acc,Bs):-
	length(Acc,Max)
	,forall(member(_C-K,Cs)
	      ,K > 1)
	,reverse(Acc,Bs)
	,debug_clauses(metarule_grounding,'Ground literals:',[Bs]).
prove_body_literals_ho(Max,Cs,Ss,Acc,Bind):-
	debug_clauses(metarule_grounding,'Accumulated literals:',[Acc])
	,length(Acc,N)
	,N =< Max
	,member(L,Ss)
	% Avoid grounding the Herbrand signature
	,copy_term(L,L_)
	,variable_instantiations(L_,Cs,Is)
	,debug_clauses(metarule_grounding,'Variable instantiations:',[Is])
	,debug_clauses(metarule_grounding,'Grounding literal:',[L_])
	,call(L_)
	,new_literal(L_,Acc)
	,debug_clauses(metarule_grounding,'Ground literal:',[L_])
	,counts(Is,Cs,Cs_)
	,debug_clauses(metarule_grounding,'Constant counts:',[Cs_])
	,prove_body_literals_ho(Max,Cs_,Ss,[L_|Acc],Bind).


%!	literals_metasub(+Literals,-Metasubstitution) is det.
%
%	Construct a Metasubstitution from the Literals of a metarule.
%
%	Literals is the set of encapsulated head and body literals of a
%	metarule, learned with prove_body_literals_ho/5.
%	Metasubstitution is an encapsulated metasubstitution atom
%	holding the predicate symbols of Literals.
%
literals_metasub(Ls,Sub):-
	findall(P
	       ,(member(L,Ls)
		,L =.. [m,P|_As]
		)
	       ,Ps)
	,Sub =.. [m,hom|Ps].
