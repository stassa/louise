:-module(executors,[solver_executor/4
                   ,solve_environment/5
                   ,solved_example/5
                   ,executor/6
                   ,executor/7
                   ,grid_slam/7
                   ]).

:-use_module(controller_freak_configuration).
:-use_module(lib(grid_master/src/action_generator)).
:-use_module(lib(grid_master/src/actions),[]).
:-use_module(lib(grid_master/src/map)).
:-use_module(lib(grid_master/src/map_display)).
:-use_module(lib(grid_master/data/environments/basic_environment/maze_observations)).

:- controller_freak_configuration:controller(P1,_,_)
   ,controller_freak_configuration:environment(P2,_)
   ,use_module(P1)
   ,use_module(P2).

/** <module> Execution predicates for Solvers and Finite State Controllers.

*/

%!      solve_executor(+Solver,+Map,+Display,-SLAM).
%
%       Execute a solver using a SLAMming map to solve a real Map.
%
%       This predicate first carries out a "survey" mission: searching
%       the map exhuastively with a controller, followed by seeking the
%       start and end tiles to complete the mapping of the unseen
%       environment with grid-SLAM. Once a map is created in this way it
%       is used to generate the clauses of an action model for the
%       Solver, then the Solver is executed to solve the real Map, but
%       using the model derived from the SLAMming map. If all succeeds
%       the SLAMming map is returned at the end. Each of the three steps
%       of the process: searching, seeking and solving, can be displayed
%       separately by setting the controller_freak_configuration option
%       debug_environment/3.
%
%       Solver is a list of clauses of a logic program representing a
%       solver.
%
%       Map is a map/3 term.
%
%       Display is one of: [tiles,blessed,none], denoting how the Solver
%       solving the Map is to be displayed. This is required for the
%       environment initialisation.
%
%       SLAM is the SLAMming map derived during execution of the Solver
%       and its attendant controllers.
%
%       @tbd This predicate incorporates multiple tests of execution and
%       is more appripriately considered as being at the "mission" or
%       experiment level, above execution. To be honest it's a bit of a
%       hack to allow a complete SLAMming map to be used with the
%       Solver. The non-hack version would entail a special-uprose
%       controller that can do the whole mapping on its own. But I
%       suppose for the time being it fits into this module.
%
solver_executor(Ss,Map,Disp,M1):-
        controller_freak_configuration:environment(_,Env)
        %,grid_master_configuration:maps_module(Maps)
        % Initialise the environment for a "Search" mission.
        ,Env:environment_init(Map,Disp,Ds,Fs-s,Q0,O0,Gs_0-nil)
        ,executor(Fs,Q0,O0,Gs_0,As_0,[X0/Y0,M0])
        ,clear_marks([X0/Y0,M0])
        ,debug_environment(search,'\nSearching map',Disp,As_0,Fs)
        % Initialise the environment for a "Seek" mission.
        ,Env:environment_init(Map,Disp,Ds,Fs_1-s,Q0,O0,Gs_1-e)
        ,executor(Fs_1,Q0,O0,Gs_1,As_1,[X0/Y0,M0],[X1/Y1,M1])
        ,clear_marks([X1/Y1,M1])
        ,debug_environment(seek,'\nSeeking start and exit tiles',Disp,As_1,Fs_1)
        % Solve the environment using the SLAMming map and a solver.
        ,length(As_0,N)
        ,solve_environment(Ss,M1,0,N,As_S)
        ,once(debug_environment(solve,'\nSolving map:',Disp,As_S,Fs_1))
        %Stops whatever silly backtracking somewhere or other ggnrnananrnang.
        ,!.


%!      debug_environment(+Subject,+Message,+Display,+Actions,+Fluents)
%!      is det.
%
%       Debug the solution of an environment by a solver or controller.
%
%       Subject is an atom matched to the first argument of the
%       controller_freak_configuration option debug_environment/3 to
%       decide whether, and how, to debug the solved environment.
%
%       Message is a message string to be printed before debugging the
%       solved environment.
%
%       Display is one of: [blessed,print,none], denoting how the solved
%       environment is to be displayed.
%
%       Fluents is a set of fluents related to the solved environment.
%       Those are not accessible to the solver, controller, or executor.
%
debug_environment(S,M,Disp,As,Fs):-
        controller_freak_configuration:environment(_,Env)
        ,controller_freak_configuration:debug_environment(S,W,T)
        ,debug(S,'Actions:~w',[As])
        ,writeln(M)
        ,(   T == true
         ->  time( Env:display_environment(W,id,Disp,As,Fs) )
         ;   T == false
        ->   Env:display_environment(W,id,Disp,As,Fs)
         )
        ,length(As,N)
        ,debug(S,'~w Actions',[N])
        ,!.
debug_environment(_S,_M,_Disp,_As,_Fs).



%!      solve_environment(+Solver,+Map,+Min,+Max,+Actions) is det.
%
%       Solve an environment with a Solver.
%
%       Thin shell around solved_example/5 to return the list of Actions
%       in a solved example.
%
%       Solver is a list of clauses, a logic program representing a grid
%       map solver.
%
%       Map is a map/3 term, the map of the environment to be solved.
%
%       Min and Max are integers, the minimum and maximum length of a
%       sequence of actions generated by the Solver while solving the
%       given Map. This implies that the solver actions are in the
%       controller_sequences representation. It is not always known in
%       advance Min and Max but a good heuristic is Min = 0, Max = W x
%       H, where W, H the width and height of the grid in Map.
%
%       Actions is a list of atoms, the actions returned by the
%       controller. Actions constitutes a plan to solve the environment
%       in Map. This assumes that Map is fully observable.
%
solve_environment(Ss,Map,J,K,As):-
        grid_master_configuration:action_representation(R)
        ,solved_example(Ss,Map,J,K,E)
        % Need better way to extract actions from examples.
        ,E =.. [_S,S1,_S2]
        %,S1 = [_Id,_XYs,_Ts,_Q0,_Qs,_Os,As,_Qs_]
        ,fluents_actions(R,S1,As)
        ,!.


%!      fluents_actions(?Representation,?Fluents,?Actions) is semidet.
%
%       Extract a list of Actions from a list of Fluents.
%
%       Representation is one of the known action_representation/1
%       values: [stack-based, stack_less,lookaround,
%       controller_sequences].
%
%       Fluents is a list of fluents, taken as argument of a dyadic
%       action or solver predicate.
%
%       Actions is a fluent in Fluents representing a list of Actions.
%       Actions may be a list of atomic action symbols, or the special
%       atom 'nil', meaning the current Representation does not record
%       lists of actions. For example, that is the case with the
%       stack_less Representation.
%
%       @tbd This so needs a better way to extract fluents.
%
fluents_actions(stack_based,[_Id,_Xs/_Ys,_T1,_Os,As],As).
fluents_actions(stack_less,[_Id,_X/_Y,_T],[]).
fluents_actions(lookaround,[_Id,_Xs/_Ys,_T1,_Os,As],As).
fluents_actions(controller_sequences,[_Id,_Xs/_Ys,_T1,_Q0,_Qs,_Os,As,_Qs_],As).


%!      solved_example(+Solver,+Map,+Min,+Max,-Example) is nondet.
%
%       Solve an Example with a Solver.
%
%       Business end of solve_environment/5.
%
%       Solver is a list of clauses, a logic program representing a grid
%       map solver.
%
%       Map is a map/3 term, the map of the environment to be solved.
%
%       Min and Max are integers, the minimum and maximum length of a
%       sequence of actions generated by the Solver while solving the
%       given Map. This implies that the solver actions are in the
%       controller_sequences representation. It is not always known in
%       advance Min and Max but a good heuristic is Min = 0, Max = W x
%       H, where W, H the width and height of the grid in Map.
%
%       Example is an instance of the Solver instantiated according to
%       the Solver's solution of the given Map.
%
solved_example(Ss,Map,J,K,E):-
        grid_master_configuration:maps_module(Maps)
        ,map_term(Id,_Ds,_,Map)
        % Careful; this is passed to the with_primitives/3 call below.
        ,G_in = (experiment_file:test_initialisation(Id,Q0,Q1,[XYs,XYe,Ts,Te,_,_])
                ,experiment_file:solver_test_instance(s/2,E,[Id,J,K,XYs,XYe,Ts,Te,Q0,Q1])
                ,call(user:E)
                )
        ,S = (assert(Maps:Map,Ref)
             ,assert_program(user,Ss,Rs_S)
             ,table(user:s/2 as variant)
             )
        ,G_out = (with_primitives(Map,user,G_in)
                 )
        ,C = (erase_program_clauses([Ref|Rs_S])
             ,untable(user:s/2)
             )
        ,setup_call_cleanup(S,G_out,C).



%!      executor(+Initial,+Q0,+O,+Goal,-Actions,-Model) is nondet.
%
%       Execute a controller in an Initial state to reach a Goal state.
%
%       Initial is a set of fluents, not observed by the controller,
%       that determine the state of the hidden environment where the
%       controller operates. "State" here refers to the environment
%       state, not the controller state.
%
%       Q0 and O are the starting state and initial observation of the
%       controller in the environment behind the Initial state.
%
%       Goal is a set of fluents, the same as in Initial, but with
%       different values, that defines a goal state that the controller
%       much reach, starting at the given Initial state, Q0 and O. To
%       make things more interesting, Goal should not include the X/Y
%       coordinates of a destination on a map, only the tile at the
%       destination.
%
%       Actions is a list of action labels, the actions taken by the
%       controller to reach the Goal state from the Initial state.
%       Actions are in the order in which they were taken during
%       execution fo the controller.
%
%       Model is a list [X/Y,Map], where X/Y is a pair of coordinates,
%       and Map a map/3 term, of the map created by a controller
%       exploring an environment. X/Y are the coordinates of the
%       controller in Map at the location where it stopped exploring.
%
%       Clauses of executor/6 are selected according to the
%       controller_freak_configuration option executor/1, that
%       determines the execution strategy for controllers. Known
%       strategies are as follows:
%       * backtracking: magickally teleport back in time.
%       * reversing: painstakingly reverse course.
%       * backtracking_slam: as 'bakctracking' but also builds a Model.
%       * reversing_slam: as 'reversing' but also builds a Model.
%
%       When the value of the executor/1 option is 'backtracking' or
%       'reversing' Model is the empty list. Otherwise, grid_slam/7 is
%       used to create a map from a controller's observations as it
%       explores an environment. Further controller_freak_configuration
%       options controll how grid_slam/7 is used and whether the map
%       is displayed while it is being derived for debugging. See
%       debug_map/2, mark_slamming_map/1 and overlay_precedence/2.
%
executor(Fs,Q0,O,Gs,As,Ms):-
        controller_freak_configuration:executor(E)
        ,G = executor_loop(E,Fs,Q0,O,Gs,As,Ms)
        ,setup_executor(G).

%!      setup_executor(+Goal) is det.
%
%       Setup tabling for an executor, if required.
%
%       Executors are tabled or not according to the value of
%       controller_freak_configuration option table_executor/1.
%
setup_executor(G):-
        controller_freak_configuration:table_executor(true)
        ,S = (table(executor_bck/7)
             ,table(executor_rev/8)
             ,table(executor_bck_slam/9)
             ,table(executor_rev_slam/10)
             ,table(observation_action/4)
             ,table(observation_actions/4)
             ,table(oscillation_safe/2)
             ,table(environment_action/4)
             )
        ,C = (untable(executor_bck/7)
             ,untable(executor_rev/8)
             ,untable(executor_bck_slam/9)
             ,untable(executor_rev_slam/10)
             ,untable(observation_action/4)
             ,untable(observation_actions/4)
             ,untable(oscillation_safe/2)
             ,untable(environment_action/4)
             )
        ,setup_call_cleanup(S,G,C).
setup_executor(G):-
        controller_freak_configuration:table_executor(false)
        ,call(G).


%!      executor_loop(+Class,+Init,+Q0,+O0,+Goal,-Actions,-Map) is
%!      det.
%
%       Start an executor loop.
%
%       Arguments are inherited from executor/6. The executor to run is
%       selected by controller_freak_configuration option executor/1.
%
executor_loop(backtracking,Fs,Q0,O,Gs,As,[]):-
        environment_observation(Fs,O)
        ,executor_bck(Fs,Q0,O,_,As,Gs,[])
        ,!.
executor_loop(reversing,Fs,Q0,O0,Gs,As,[]):-
        environment_observation(Fs,O0)
        ,tuple(T0,Q0,O0,_A1,_Q1)
        ,executor_rev(Fs,trv:T0,[nil],[nil],Gs,As).
executor_loop(backtracking_slam,Fs,Q0,O,Gs,As,Ms_):-
        environment_observation(Fs,O)
        ,slam_update([],O,_A,Ms)
        ,executor_bck_slam(Fs,Q0,O,_,As,Ms,Gs,[],Ms_)
        ,clear_marks(Ms_)
        ,!.
executor_loop(reversing_slam,Fs,Q0,O0,Gs,As,Ms):-
        environment_observation(Fs,O0)
        ,tuple(T0,Q0,O0,_A1,_Q1)
        ,executor_rev_slam(Fs,trv:T0,[nil],[nil],[],Gs,As,Ms)
        ,clear_marks(Ms).



%!      executor(+Initial,+Q0,+O,+Goal,-Actions,+Model,-New) is nondet.
%
%       Execute a controller in an Initial state to reach a Goal state.
%
%       As executor/6 but allows an initial Model to be passed in and
%       updated by grid-based SLAM.
%
executor(Fs,Q0,O,Gs,As,Ms,Ms_):-
        controller_freak_configuration:executor(E)
        ,G = executor_loop(E,Fs,Q0,O,Gs,As,Ms,Ms_)
        ,setup_executor(G).


%!      executor_loop(+Class,+Init,+Q0,+O0,+Goal,-Actions,+Map,-New) is
%!      det.
%
%       Start a slamming executor loop.
%
%       As executor_loop/7 but allows an existing Map to be passed in
%       and updated with grid-based SLAM.
%
executor_loop(backtracking_slam,Fs,Q0,O,Gs,As,Ms,Ms_):-
% Fails immediately.
        environment_observation(Fs,O)
        ,slam_update([],O,_A,Ms)
        ,executor_bck_slam(Fs,Q0,O,_,As,Ms,Gs,[],Ms_).
executor_loop(reversing_slam,Fs,Q0,O0,Gs,As,Ms,Ms_):-
        environment_observation(Fs,O0)
        ,tuple(T0,Q0,O0,_A1,_Q1)
        ,executor_rev_slam(Fs,trv:T0,[nil],[nil],Ms,Gs,As,Ms_).


%!      environment_observation(+State,-Observation) is det.
%
%       Obtain one Observation at the current world-State.
%
%       State is a list of unobservable fluents that determine the
%       current world-state.
%
%       Observation is an observation label derived from the given
%       world-State.
%
%       Used to seed executor/7 with an observation in an initial
%       world-state.
%
%       Note: Also clears the screen when displaying the map with
%       blessed library.
%
environment_observation(Fs,O):-
        controller_freak_configuration:environment(_,M)
        ,M:new_observation(Fs,O).


%!      observation_action(+Q0,+O,-A,-Q1) is nondet.
%
%       Mapping between states, observations and actions.
%
observation_action(Q0,O,A,Q1):-
        controller_freak_configuration:controller(_,M,S)
        ,T =.. [S,Q0,O,A,Q1]
        ,call(M:T).


%!      oscillation_safe(+Action1,+Action2) is det.
%
%       True when Action1 and Action2 are not opposites.
%
%       Action1 and Action2 are action labels returned by a controller.
%
%       Used to avoid oscillations in controller execution. Whether
%       Action1 and Action2 are opposites is controller knowledge
%       expressed in a predicate opposite_actions/2, declared in the
%       controller module.
%
%/*
oscillation_safe(A0,A1):-
        maplist(ground,[A0,A1])
        ,!
        ,debug(oscillations,'oscillating(~w,~w)?',[A0,A1])
        ,\+ opposite(A0,A1)
        ,debug(oscillations,'No!',[]).
oscillation_safe(_Ai,_Ak).
%*/
/*
oscillation_safe(A0,A1):-
        maplist(ground,[A0,A1])
        ,!
        ,debug(oscillations,'oscillating(~w,~w)?',[A0,A1])
        ,(   \+ opposite(A0,A1)
         ->  debug(oscillations,'No!',[])
         ;   debug(oscillations,'Yes!',[])
             ,fail
             ,!
         ).
oscillation_safe(_Ai,_Ak).
*/


%!      environment_action(+State,+Action,+Observation,-New) is nondet.
%
%       Apply an Action in an environment State, moving to a New state.
%
%       State is a set of fluents passed to the current environment,
%       defined in environment/1. Fluents in State represent the hidden
%       state of the environment, i.e. that part of the environment
%       that is not observable by a controller.
%
%       Action is an action to take in the current environment.
%
%       Observation is an observation generated by the environment in
%       the given State when Action is taken.
%
%       New is the new state of the environment, as modified by Action.
%       New is a list of the same fluents as in State, but with values
%       modified according to Action.
%
environment_action(Fs,A,O,Fs_):-
        controller_freak_configuration:environment(_,M)
        ,M:environment(Fs,A,O,Fs_).


%!      opposite(+Action1,+Action2) is det.
%
%       True when Action1 and Action2 are opposites.
%
%       Action1 and Action2 are action labels defined by a controller.
%       Alternatively, they can be Action-State pairs, again defined by
%       a controller.
%
%       Used to avoid oscillations in controller execution. Whether
%       Action1 and Action2 are opposites is controller knowledge
%       expressed in a predicate opposite_actions/2, declared in the
%       controller module.
%
%       opposite/2 fails when Action1 and Action2 are not both ground.
%
opposite(A0-Q0,A1-Q1):-
        !
        ,controller_freak_configuration:controller(_,M,_S)
        ,M:opposite_actions(A0-Q0,A1-Q1).
opposite(A0,A1):-
        controller_freak_configuration:controller(_,M,_S)
        ,M:opposite_actions(A0-_Q0,A1-_Q1).



%!      executor_bck(+Init,+Q0,+O0,+Action,+Acc,+Goal,-Actions)
%!      is nondet.
%
%       Backtracking executor for Finite State Controllers.
%
%       Init and Goal are the lists of fluents representing the initial
%       and goal world-states, respectively.
%
%       Q0 and O are the last controller state and observation pair,
%       after taking an action and switching to a new state (i.e. Q0).
%
%       Action is the last Action-State pair, taken after observing O in
%       Q0.
%
%       Acc is the accumulator of actions, eventually bound to Actions.
%
%       Actions is a list of action labels, tracing the controller's
%       actions, without state or observation labels. Use this to debug
%       or visualise the controller's behaviour.
%
%       This predicate implements a classic backtracking executor that
%       "jumps back in time", if the environment allows it.
%
%       @tbd Unlike reversing executors this executor queries controller
%       tuples directly, with a call to observation_action/4, rather
%       than collecting all possible tuples and so does not need to
%       pass around tuples directly. Or, dunno, it could? Maybe?
%
executor_bck(Gs,_Q0,_O,_A,As,Gs,As):-
        %throw('You are here'),
        debug(environment,'~w',[Gs]),
        !.
executor_bck(Fs,Q0,O,A,[A_|Acc],Gs,As):-
        observation_action(Q0,O,A_,Q1)
        ,oscillation_safe(A,A_)
        ,environment_action(Fs,A_,O_,Fs_)
        ,debug(environment,'~w -> ~w',[Fs, Fs_])
        ,debug(executor,'~w -> ~w',[(Q0,O,A_,Q1),O_])
        ,executor_bck(Fs_,Q1,O_,A_,Acc,Gs,As).



%!      executor_rev(+Fluents,+Tuple,+Stack,+Tuples,+Goals,-Acc) is
%!      nondet.
%
%       Reversing executor.
%
%       Init and Goal are the lists of fluents representing the initial
%       and goal states, respectively, of the environment in which the
%       controller operates. Fluents are never accessed by an executor
%       an donly passed to the environment, along with controller
%       actions, to receive an observation.
%
%       Tuple is a pair S:T, where S is an _executor_ state (yes, yes I
%       know, too many damn states) and T is a partially instantiated
%       tuple of the form T(Q,O,A,Q1). Executor states are one of:
%       [trv, rev, alt] and determine how the stack is managed. All
%       tuples discussed below are of the same form as Tuple.
%
%       Stack is a stack of ground Tuple's, the stack that drives the
%       executor's, and the executed controller's, behaviour. When Stack
%       is empty, execution terminates.
%
%       Tuples is a list of ground tuples collected at the end of each
%       recursive step.
%
%       Goals is a list of fluents defining a goal state in the current
%       environment. When Fluents unifies with Goals, execution
%       terminates.
%
%       Discussion
%       ----------
%
%       The Stack in this executor is used to implement a sort of
%       backtracking in-place, without magickal jump-backs to unvisited
%       branches, which may not be avialable in an environment. Instead,
%       this executor reverses course when a dead end is found, where no
%       more actions are possible. This setting assumes that only
%       reversible actions are taken, otherwise the controller may get
%       well and truly stuck in an unrecoverable situation.
%
%       What actions are reversible, and how, is determined by the
%       controller, which must declare a predicate reversible_action/2,
%       of the form reversible_action(A1-Q1,A2-Q2), where A1-Q1 is an
%       action and next-state label, and A2-Q2 are the same, but
%       reversed. For example, a grid-navigating controller may
%       broadcast the following opposite actions:
%       ==
%       opposite_actions(up-q0,down-q2).
%       opposite_actions(down-q2,up-q0).
%       opposite_actions(left-q3,right-q1).
%       opposite_actions(right-q1,left-q3).
%       ==
%
%       Whether an action should be reversed, or not, and how, is
%       determined by the executor's own state.
%
%       The executor state is reflected in a Tuple's form and determines
%       how the stack is updaed. A tuple can be a traversing tuple, of
%       the form trv:T, an alternative tuple, of the form alt:T, or a
%       reversing tuple, of the form rev:T. Accordingly, the executor
%       can be in state "trv", traversing, "alt", for alternatives, or
%       "rev" for "reversing".
%
%       A quick intution about the way the stack is updated depending on
%       executor states is as follows:
%       * At each step of execution all controller tuples matching the
%         current controller state and observation are pushed to the
%         stack, then the first tuple is popped from the stack and its
%         action sent to the environment to obtain a new observation.
%       * In state "trv", the reverse of each action taken is pushed on
%         the stack as a member of a new "rev" tuple.
%       * In state "rev", no tuple is pushed on the stack.
%       * In state "alt" each tuple popped from the stack puts the
%         executor in state "trv".
%
%       This way the controller traverses a map until it reaches a dead
%       end, and begins reversing its course. When the controller
%       reaches the last place where it had an alterantive, it stops
%       reversing and starts traversing and pushing alternatives to the
%       stack again.
%
%       More technically, the reversing executor's loop is as follows:
%       a) Push all possible tuples to the stack.
%       b) Pop a tuple, T, from the stack
%       c) Ground T by interaction with the environment.
%       d) If T is not a reversing tuple, push its reverse on the stack.
%       e) Repeat until Stack is empty or Fluents unifies with Goals.
%
executor_rev(Gs,_T1,_Ss,Acc,Gs,As):-
        cleanup_actions(Acc,As)
        ,!.
executor_rev(_Fs,_T1,[],Acc,_Gs,As):-
        cleanup_actions(Acc,As)
        ,!.
executor_rev(Fs,T1,Ss,[T0|Acc],Gs,As):-
        push_tuples(T0,T1,Ss,Ss_1)
        ,pop_tuple(Ss_1,T2,Ss_2)
        ,state_transition(Fs,T2,Fs_,T3)
        ,! % er, just in case?
        ,debug(executor,'~w -> ~w',[T2,T3])
        ,push_reverse(T2,Ss_2,Ss_r)
        ,executor_rev(Fs_,T3,Ss_r,[T2,T0|Acc],Gs,As).


%!      push_tuples(+T0,+T1,+Stack,-Updated) is det.
%
%       Push all possible tuples to the Stack.
%
%       T0 and T1 are tuples, of the form S:T, where S is a controller
%       state, and T is a tuple T(Q0,O,A,Q1). The tuples in T0 and T1
%       are chained so that their variables are bound as in
%       T0(Q0,O,A1,Q1), T1(Q1,O1,A2,Q2).
%
%       Stack is the current executor stack, a list of tuples.
%
%       Updated is Stack updated with all instances of T1 returned by
%       the current controller, according to the binding to T1, and
%       ensuring that an action label A2 in T1 will not be the same as
%       the action label A in T0, to avoid oscillations.
%
%       Note that T0 can also be the atom 'nil' a hack to avoid passing
%       in an empty stack at the start of execution because I like my
%       terminating conditions to be above my recursive clauses. In that
%       case Stack should also be the singleton list [nil] and T can
%       only be of the form trv:T.
%
%       When T0 is of the form rev:T, Stack and Updated are the same.
%       That is to say, when the executor is in reversing state it does
%       not reverse its actions, because that would be reversing the
%       reverse; possibly of the reverse, of the reverse, etc, causing
%       (long-term) looping forever.
%
push_tuples(nil,trv:T1,[nil],Ts_s):-
        !
        ,tuple(T1,Q1,O1,A1,Q2)
        ,findall(alt:T1
                ,(observation_action(Q1,O1,A1,Q2)
                 )
                ,Ts_)
        ,sort(Ts_,Ts_s).
push_tuples(rev:_T0,_T1,Ss,Ss):-
        !.
push_tuples(_:T0,_:T1,Ss,Ss_):-
        tuple(T0,_Q0,_O0,A0,Q1)
        ,tuple(T1,Q1,O1,A1,Q2)
        ,findall(alt:T1
                ,(observation_action(Q1,O1,A1,Q2)
                 ,oscillation_safe(A0-Q1,A1-Q2)
                 )
                ,Ts_)
        ,sort(Ts_,Ts_s)
        ,append(Ts_s,Ss,Ss_).


%!      tuple(?Tuple,?Q0,?O,?A,?Q1) is det.
%
%       Construct or deconstruct a Tuple.
%
%       @tbd This should really, really be in grid_master.
%
tuple(T,Q0,O,A,Q1):-
        controller_freak_configuration:controller(_,_M,S)
        ,T =.. [S,Q0,O,A,Q1].


%!      pop_tuple(+Stack,-Tuple,-Updated) is det.
%
%       Pop a Tuple from the Stack.
%
%       Stack is a stack of tuples of the form S:T, where S is an
%       executor state, one of [trv, rev, alt], and tuple is a
%       controller tuple of the form T(Q0,O,A,Q1).
%
%       Tuple is the tuple popped from the stack and is always of the
%       form trv:T or rev:T.
%
%       Updated is the Stack minus the popped Tuple.
%
%       Note that Tuple may have previously been associated to a
%       different executor state in the stack. In particular, an alt:T
%       tuple will be popped as a trv:T tuple, whereas a rev:T tuple
%       will always be popped as a rev:T tuple.
%
%pop_tuple([trv:T1|Ss],trv:T1,Ss). %Not?
pop_tuple([alt:T1|Ss],trv:T1,Ss):-
        !.
pop_tuple([rev:T1|Ss],rev:T1,Ss):-
        !.


%!      state_transition(+Fluents,+TupleIn,-Updated,-TupleOut) is det.
%
%       Send a tuple to the environment to update it.
%
%       Fluents is a list of fluents that represent the current
%       environment.
%
%       TupleIn is the "input tuple", of the form S:T0 where S is an
%       executor state, one of [trv, rev, alt] and T0 is a controller
%       tuple T0(Q0,O,A,Q1) generated by querying the current
%       controller.
%
%       Updated is the list of Fluents possibly modified according to
%       the action label in TupleIn.
%
%       TupleOut is a partially instantiated tuple of the form S:T1,
%       where S is the same controller state as in TupleIn, and T1 is a
%       controller tuple T1(Q1,O1,A2,Q2), where Q1 is the controller
%       state in TupleIn, and O1 is the observation label returned by
%       the environment after A1 in TupleIn is sent to the environment.
%
state_transition(Fs,D:T1,Fs_,D:T2):-
        tuple(T1,_Q1,_O1,A2,Q2)
        ,environment_action(Fs,A2,O2,Fs_)
        ,tuple(T2,Q2,O2,_A3,_Q3).


%!      push_reverse(+Tuple,+Stack,-Updated) is det.
%
%       Update the Stack with a reversing Tuple.
%
push_reverse(trv:T,Ss,[rev:Tr|Ss]):-
        !
        ,tuple(T,Q,O,A,Q1)
        ,opposite(A-Q1,Ar-Qr)
        ,tuple(Tr,Q,O,Ar,Qr).
push_reverse(rev:_T,Ss,Ss).


%!      cleanup_actions(+Actions,-Output) is det.
%
%       Clean up Actions collected by an executor.
%
%       Actions is a list of action terms returned by an executor, in
%       particular a revresing executor where actions may be in
%       different forms.
%
%       Output is the list of actions in Actions modified according to
%       teh setting of the controller_freak_configuration option
%       output_actions/1.
%
%       Actions in a reversing executor's accumulator of actions will be
%       in the form S:T where S is an executor state and T a tuple. We
%       might want to return the entire S:T term, just the tuple, T, or
%       the action label in T. This is managed by output_actions/1.
%
cleanup_actions(As,As_):-
        controller_freak_configuration:output_actions(executor_states)
        ,!
        ,reverse(As,As_).
cleanup_actions(As,As_):-
        controller_freak_configuration:output_actions(F)
        ,!
        ,cleanup_actions(F,As,As_,[]).

%!      cleanup_actions(+Format,+Actions,-Cleaned,+Acc) is det.
%
%       Business end of cleanup_actions/2.
%
%       Essentially reverse/2 but with some cleanup potential.
%
cleanup_actions(_,[nil],As,As):-
        !.
cleanup_actions(labels,[_:T|As],Bind,Acc):-
        !
        ,tuple(T,_Q0,_O,A,_Q1)
        ,cleanup_actions(labels,As,Bind,[A|Acc]).
cleanup_actions(tuples,[_:T|As],Bind,Acc):-
        cleanup_actions(tuples,As,Bind,[T|Acc]).



%!      executor_bck_slam(+Init,+Q0,+O0,+A,+As_Acc,+Ms_Acc,+Goal,-As,-Ms)
%!      is nondet.
%
%       Backtracking executor building a Map as it goes.
%
%       Ms is a list [X/Y,Map], where X/Y is a pair of coordinates, and
%       Map a map/3 term, of the map created by a controller exploring
%       an environment. X/Y are the coordinates of the controller in
%       Map, when it stopped exploring. X/Y is marked with the exit
%       tile, 'e', in Map.
%
%       This backtracking executor only takes actions that do not move
%       the controller on a cell in the slamming map that is marked as
%       traversed. This is to avoid long-term loops in maps with open
%       areas (plazas). The check is made as part of the slamming map
%       updates, with slam_update/4.
%
executor_bck_slam(Gs,_Q0,_O,_A,As,[X/Y,M],Gs,As,[X/Y,M]):-
        debug(environment,'~w',[Gs])
        ,mark(X/Y,e,M)
        ,!.
executor_bck_slam(Fs,Q0,O,A,[A_|Acc],Ms,Gs,As,Ms_Bind):-
        observation_action(Q0,O,A_,Q1)
        ,oscillation_safe(A,A_)
        ,environment_action(Fs,A_,O_,Fs_)
        ,debug(environment,'~w -> ~w',[Fs, Fs_])
        ,debug(executor,'~w -> ~w',[(Q0,O,A_,Q1),O_])
        ,slam_update(Ms,O_,A_,Ms_)
        ,executor_bck_slam(Fs_,Q1,O_,A_,Acc,Ms_,Gs,As,Ms_Bind).


%!      slam_update(+Ms,+Observation,+Action,-Updated) is det.
%
%       Update the map being built by Slam.
%
%       Also checks that the next location to be visited on the map is
%       not marked as visited, to avoid long-distance looping.
%
slam_update([],O,A,[X1/Y1,M1]):-
        grid_slam(id(dyn1),_XY0,O,A,1,X1/Y1,M1)
        ,mark(X1/Y1,s,M1)
        ,map_logging(slam_update,M1).
slam_update([X0/Y0,M0],O,A,[X1/Y1,M1]):-
        grid_slam(M0,X0/Y0,O,A,1,X1/Y1,M1)
        ,check(trv,X1/Y1,M1)
        ,mark(X1/Y1,@,M1)
        ,map_logging(slam_update,M1).


%!      mark(+Coordinates,+TIle,+Map) is det.
%
%       Mark the position of the agent on a Map.
%
%       Coordinates is a pair X/Y of the coordinates to mark.
%
%       Tile is the tile to mark at X/Y.
%
%       Map, well, duh.
%
%       Tile is overlayed to whatever is at X/Y, or not, according to
%       the precedence list in overlay_precedence/2 for 'mark'.
%
mark(X/Y,T1,map(Id,Ds,M)):-
        controller_freak_configuration:mark_slamming_map(true)
        ,controller_freak_configuration:overlay_precedence(mark,Ps)
        ,!
        ,map_location(X/Y,T0,M,Ds,true)
        ,overlay(T1,T0,Ps,T)
        ,nb_write_to_location(X/Y,T,M,Ds,true)
        ,map_logging(mark,map(Id,Ds,M)).
mark(_XY,_T,M):-
        map_logging(mark,M).


%!      map_logging(+Subject,+Map) is det.
%
%       Print a map for a given Subject.
%
%       The format in which the map is to be printed is taken from the
%       configuration option debug_map/2.
%
map_logging(S,M):-
        controller_freak_configuration:debug_map(S,W)
        ,print_map(W,M)
        ,nl
        ,!.
map_logging(_S,_M).


%!      check(+State,+Location,+Map) is det.
%
%       Check whether a Location on a Map is marked, or not.
%
%       State is an executor state label, one of: [trv, rev, alt].
%
%       Location is a pair X/Y, of coordinates on a slamming map.
%
%       Map is the slamming map being derived by exploration.
%
%       If State is rev, this predicate succeeds automatically. If State
%       is trv or alt, this predicate fails if X/Y is marked with a
%       traversing tile, currently '@', on Map.
%
check(rev,_XY,_M):-
        !.
check(_Rev,X/Y,map(_Id,Ds,M)):-
        once( map_location(X/Y,T,M,Ds,true) )
        ,T \== '@'.



%!      executor_rev_slam(+Init,+Tuple,+Stack,+Ts,+Ms,-Gs,-Ts,-SLAM)
%!      is det.
%
%       Reversing executor that bulds a model of an environment.
%
%       Init, Tuple, Stack are as in executor_rev/8. Ts and Gs are the
%       same as Tuples and Goals in executor_rev/8, accumulators for the
%       output actions (as tuples, labels or executor states) and
%       environment goal state.
%
%       Ms is the accumulator of slamming states, and SLAM is the
%       slamming state. SLAM Is a list [X/Y,Map], where X/Y is a pair of
%       coordinates, and Map a map/3 term, of the map created by a
%       controller exploring an environment. X/Y are the coordinates of
%       the location in Map where the controller was when it stopped
%       exploring. X/Y is marked with 'e' on Map.
%
%       This reversing executor updates the stack with all possible
%       actions that do not move the controller on a cell in the
%       slamming map that is marked as traversed. This is to avoid
%       long-term loops in maps with open areas (plazas).
%
executor_rev_slam(Gs,_T1,_Ss,Acc,[X/Y,M],Gs,As,[X/Y,M]):-
        cleanup_actions(Acc,As)
        ,mark(X/Y,e,M)
        ,!.
executor_rev_slam(_Fs,_T1,[],Acc,[X/Y,M],_Gs,As,[X/Y,M]):-
        cleanup_actions(Acc,As)
        ,mark(X/Y,e,M)
        ,!.
executor_rev_slam(Fs,T1,Ss,[T0|Acc],Ms,Gs,As,Ms_Acc):-
        push_tuples(Ms,T0,T1,Ss,Ss_1)
        ,pop_tuple(Ss_1,T2,Ss_2)
        ,state_transition(Fs,T2,Fs_,T3)
        ,! % er, just in case?
        ,debug(executor,'~w -> ~w',[T2,T3])
        ,push_reverse(T2,Ss_2,Ss_r)
        ,slam_update_rev(Ms,T2,T3,Ms_)
        ,executor_rev_slam(Fs_,T3,Ss_r,[T2,T0|Acc],Ms_,Gs,As,Ms_Acc).


%!      slam_update_rev(+Model,+TupleIn,+TupleOut,-Updated) is det.
%
%       Update the model being built by a reversing executor.
%
%       Model is a list [X/Y,Map], where X/Y is a pair of
%       coordinates, and Map a map/3 term, of the map created by a
%       controller exploring an environment. X/Y are the coordinates of
%       the location in Map where the controller was when it stopped
%       exploring. Model will be empty immediately after the first
%       action taken by the controller, i.e. at the start of execution.
%
%       TupleIn and TupleOut are as in state_transition/4, the input and
%       output tuples of a controller state transition. The important
%       thing to keep in mind is that TupleIn includes the last action
%       taken and TupleOut the last observation label after that action
%       was taken.
%
%       Updated is Model updated according to the action and observation
%       labels in TupleIn and TupleOut.
%
%       The initial and final location on the Model map are marked with
%       's' and 'e', respectively.
%
slam_update_rev([],_:T0,_:T1,[X1/Y1,M1]):-
        tuple(T0,_Q0,_O0,A0,Q1)
        ,tuple(T1,Q1,O1,_A1,_Q2)
        ,grid_slam(id(dyn1),_XY0,O1,A0,1,X1/Y1,M1)
        % Look back to find starting location.
        ,opposite(A0-Q1,Ar-_Qr)
        ,action_look(Ar,M1,X1/Y1,X0/Y0)
        % And mark it.
        ,mark(X0/Y0,s,M1)
        ,mark(X1/Y1,@,M1)
        ,map_logging(slam_update,M1).
slam_update_rev([X0/Y0,M0],_:T0,_:T1,[X1/Y1,M1]):-
        tuple(T0,_Q0,_O0,A0,Q1)
        ,tuple(T1,Q1,O1,_A1,_Q2)
        ,grid_slam(M0,X0/Y0,O1,A0,1,X1/Y1,M1)
        ,mark(X1/Y1,@,M1)
        ,map_logging(slam_update,M1).


%!      push_tuples(+SLAM,+T0,+T1,+Stack,-Updated) is det.
%
%       Push all possible tuples to the stack.
%
%       As push_tuples/4, but also checks that a tuple's action label
%       will not lead to a location marked as visited on a slamming map.
%       The motivation for this is to avoid long-distance loops.
%
push_tuples(_Ms,rev:_T0,_T1,Ss,Ss):-
        !.
push_tuples([],nil,trv:T1,[nil],Ts_s):-
        !
        ,tuple(T1,Q1,O1,A1,Q2)
        ,findall(alt:T1
                ,(observation_action(Q1,O1,A1,Q2)
                 )
                ,Ts_)
        ,sort(Ts_,Ts_s).
push_tuples([X0/Y0,M],nil,S1:T1,[nil],Ss_):-
% Start of execution with an existing SLAMming map.
        debug(push_tuples,'In push-tuples',[])
        ,tuple(T1,Q1,O1,A1,Q2)
        ,findall(alt:T1
               ,(observation_action(Q1,O1,A1,Q2)
                ,action_look(A1,M,X0/Y0,X1/Y1)
                ,check(S1,X1/Y1,M)
                )
               ,Ts_)
        ,sort(Ts_,Ss_)
        ,debug(observation_actions,'Pushed tuples: ~w',[Ss_]).
push_tuples([X0/Y0,M],_S0:T0,S1:T1,Ss,Ss_):-
        debug(push_tuples,'In push-tuples',[])
        ,tuple(T0,_Q0,_O0,A0,Q1)
        ,tuple(T1,Q1,O1,A1,Q2)
        ,findall(alt:T1
               ,(observation_action(Q1,O1,A1,Q2)
                ,oscillation_safe(A0-Q1,A1-Q2)
                ,action_look(A1,M,X0/Y0,X1/Y1)
                ,check(S1,X1/Y1,M)
                )
               ,Ts_)
        ,sort(Ts_,Ts_s)
        ,append(Ts_s,Ss,Ss_)
        ,debug(observation_actions,'Pushed tuples: ~w',[Ss_]).



%!      grid_slam(+Map1,+Start,+Observation,+Action,-End,-Map2) is det.
%
%       Grid-world Simultaneous Localisation and Mapping.
%
%       Builds a map of a grid world as it is being explored by a
%       model-free controller.
%
%       Map1 is either a term id(Id) if mapping has just started (i.e.
%       if the agent has just woken up in the grid world surrounded by
%       the fog of grids, or a map/3 term otherwise. A map/3 term passed
%       in to this argument should be the result of multiple chained
%       invocations of this predicate by a controller's executor.
%
%       Start is a pair X/Y, the current position of the agent _in Map1_
%       as far as the agent's model can tell.
%
%       Observation is the label of the last observation made by the
%       agent.
%
%       Action is the label of the last action taken by the agent before
%       Observation was made. That means Observation and Action may
%       belong to different controller tuples.
%
%       End is a pair X/Y, the position of the agent _in Map2_ after
%       action is taken, as far as the agent can tell.
%
%       Map2 is Map1 (or the new map created for a given Id at the
%       start of mapping) modified according to Observation and Action.
%       That is, the map is updated with a new map element matching
%       Observation placed in the End position in which Action leaves
%       the agent.
%
grid_slam(id(Id),_XY0,O,_A,R,1/1,map(Id,Ds,E)):-
        radius_dimensions(R,Ds)
        ,observation_element(O,R,E)
        ,!.
grid_slam(M0,X0/Y0,O,A,R,X1/Y1,M1):-
        %print_map(both,M0),nl,
        (   action_look(A,M0,X0/Y0,X1/Y1)
            ,\+ map_edge(M0,X1/Y1)
            ,M1 = M0
        ->  true
        ;   controller_freak_configuration:overlay_precedence(extended_map,Ps_ex)
           ,extended_map(A,M0,Ps_ex,Me)
            %,print_map(coordinates,Me),nl
            ,M1 = Me
        )
        %,print_map(coordinates,M1),nl
        ,observation_element(O,R,E)
        ,once( step_action(A,X0/Y0,M1,X1/Y1) )
        ,element_origin(X1/Y1,M1,Xo/Yo)
        ,radius_dimensions(R,Ds)
        ,controller_freak_configuration:overlay_precedence(grid_slam,Ps_g)
        ,overlay_map(map(el,Ds,E),Xo/Yo,M1,Ps_g)
        %,print_map(both,M1),nl
        .


%!      action_look(+Action,+Map,+Start,-End) is det.
%
%       Mapping between an Action and a look predicate's outcome.
%
%       Action is an action label.
%
%       Map is the map being built by the agent as it moves through a
%       grid world.
%
%       Start and End are pairs X/Y of the coordinates of the agent's
%       current location in Map (i.e. not the grid world itself, as
%       such) and of the location one step to the direction determined
%       by Action.
%
%       Used to test whether taking Action will move the agent outside
%       the limits of the dynamic Map.
%
action_look(A,Mt,X0/Y0,X1/Y1):-
        map_term(_Id,Ds,M,Mt)
        ,action_look(A,S)
        ,L =.. [S,X0/Y0,M,Ds,X1/Y1,_T]
        ,call(action_generator:L).

%!      action_look(?Action,?Symbols) is semidet.
%
%       Mapping between an Action label and a look predicate Symbol.
%
action_look(up,look_up).
action_look(right,look_right).
action_look(down,look_down).
action_look(left,look_left).


%!      map_edge(+Map,+Coordinates) is det.
%
%       True when Coordinates are at the edge of Map.
%
map_edge(_Mt,nil/nil):-
        !.
map_edge(_Mt,0/_Y):-
        !.
map_edge(_Mt,_X/0):-
        !.
map_edge(Mt,X/Y):-
        map_term(_Id,W-H,_M,Mt)
        ,(   X =:= W - 1
         ;   Y =:= H - 1
         ).


%!      step_action(+Action,+Start,+Map,-End) is nondet.
%
%       Try taking one step Action in the Map being built.
%
%       Action is an action label.
%
%       Map is the map being built from observations made by a
%       controller exploring an environment.
%
%       Start and End are pairs X/Y, the start and end location of
%       Action.
%
step_action(down,X0/Y0,_M,X0/Y0):-
        debug(step_action,'step_action/4 (down)',[]),
        !.
step_action(left,X0/Y0,_M,X0/Y0):-
        debug(step_action,'step_action/4 (left)',[]),
        !.
step_action(A,X0/Y0,M,X1/Y1):-
        debug(step_action,'step_action/4 (any: ~w)',[A]),
        grid_master_configuration:action_representation(R)
        %,M = map(_Id,Ds,Ms) ,map_display:print_map(both,Ds,Ms), nl
        %,action_generator:step(R,M,X0/Y0,S)
        ,action_generator:action(R,[M,X0/Y0],S)
        ,S =.. [_S,S1,S2]
        ,fluents_actions(R,S1,[A|_As])
        ,S1 = [Id,X0/Y0|_]
        ,S2 = [Id,X1/Y1|_].


%!      element_origin(+Current,+Map,-Origin) is det.
%
%       Where to place the Origin of a new map Element.
%
%       Current is the current position of an agent in the map being
%       built. A map element corresponding to an observation label is to
%       be centered on the agent to update the map with.
%
%       Map is the map to be updated.
%
%       Origin is an X/Y pair denoting the location where the new map
%       element is to be placed in Map. That is either the cell
%       diagonally to the left of Current, or 0/0 if that cell is
%       outside the Map.
%
element_origin(X/Y,Mt,Xo/Yo):-
        map_term(_Id,Ds,M,Mt)
        ,actions:look_down_left(X/Y,M,Ds,Xo/Yo,T)
        ,T \== o
        ,!.
element_origin(_XY,_Mt,0/0).


%!      observation_element(+Label,+Dimensions,-Element) is det.
%
%       Map between an observation Label and a map Element.
%
%       Label is an atomic observation label.
%
%       Dimensions is a pair W-H, the dimensions of a map.
%
%       Element is a new map element as a row-major ordar array, with
%       the given Dimensions and coisting of the observable and
%       unobservable tiles represented by Label.
%
%       Example:
%       ==
%       ?- _Ds = 3-3
%       ,observation_labels(_Ls)
%       ,member(L,_Ls)
%       ,map:observation_element(L,_Ds,E)
%       ,print_map(tiles,_Ds,E).
%
%       x □ x
%       □ □ □
%       x □ x
%       L = pppp,
%       E = c(x,f,x,f,f,f,x,f,x) ;
%       x □ x
%       ■ □ □
%       x □ x
%       L = pppu,
%       E = c(x,f,x,w,f,f,x,f,x) ;
%       x □ x
%       □ □ □
%       x ■ x
%       L = ppup,
%       E = c(x,f,x,f,f,f,x,w,x) .
%       % ... more
%       ==
%
observation_element(O,R,E):-
        atom_chars(O,Cs)
        ,radius_dimensions(R,Ds)
        ,maze_observations:observations_grid(Cs,Ds,Gs)
        ,maze_observations:lists_to_arrays([Gs],[E]).


%!      radius_dimensions(+Radius,-Dimensions) is det.
%
%       Mapping between an agent's view Radius and map Dimensions.
%
%       Radius is the view radius of an agent in a grid map. It denotes
%       the number of cells that are observable by the agent in any one
%       direction (including diagonal).
%
%       Dimensions is a pair W-H denoting the dimensions of a map
%       element centered on the agent, extending one step to each
%       direction, up to the given Radius. In other words, it's the
%       agent's view of the grid map, centered on the agent.
%
radius_dimensions(1,3-3):-
        !.
radius_dimensions(R,D-D):-
% Er, is that right?
% Does this need +1 for the agent's location?
        D is R^2.



%!      extended_map(+Direction,+Map,+Precedence,-Extended).
%
%       Extended a Map towards the given Direction.
%
%       Direction is one of: [up, right, left, down], denoting the
%       direction towards which the map is to be extended.
%
%       Map is a map/3 term, the map to be extended.
%
%       Extended is the map/3 term representing the map in Map, extended
%       according to Direction, i.e. with a row or column added to it.
%
%       Precedece is a list of precedence terms passed to overlay_map/4
%       or offset_map/5.
%
extended_map(up,Mt,Ps,M_e):-
        !
        ,map_term(Id,Ds,_M,Mt)
        ,extended_dimensions(up,Ds,Ds_e)
        ,new_map_term(Id,Ds_e,x,M_e)
        ,overlay_map(Mt,0/0,M_e,Ps).
extended_map(right,Mt,Ps,M_e):-
        map_term(Id,Ds,_M,Mt)
        ,extended_dimensions(right,Ds,Ds_e)
        ,new_map_term(Id,Ds_e,x,M_e)
        ,overlay_map(Mt,0/0,M_e,Ps).
extended_map(down,Mt,Ps,M_e):-
        map_term(Id,Ds,_M,Mt)
        ,extended_dimensions(down,Ds,Ds_e)
        ,new_map_term(Id,Ds_e,x,M_e)
        ,offset_map(Mt,0/0,0/1,M_e,Ps).
extended_map(left,Mt,Ps,M_e):-
        map_term(Id,Ds,_M,Mt)
        ,extended_dimensions(left,Ds,Ds_e)
        ,new_map_term(Id,Ds_e,x,M_e)
        ,offset_map(Mt,0/0,1/0,M_e,Ps).


%!      extended_dimensions(+Direction,+Dimensions,-Extended) is det.
%
%       Extend a pair of Dimensions towards one Direction.
%
extended_dimensions(up,W-H,W-Hu):-
        succ(H,Hu).
extended_dimensions(right,W-H,Wr-H):-
        succ(W,Wr).
extended_dimensions(left,W-H,Wl-H):-
        succ(W,Wl).
extended_dimensions(down,W-H,W-Hd):-
        succ(H,Hd).


%!      new_map_term(+Id,+Dimensions,+Tile,-Map) is det.
%
%       Create a new map/3 term filled with a Tile.
%
new_map_term(Id,Ds,T,map(Id,Ds,M)):-
        map:new_map(Ds,T,M).



%!      offset_map(+Overlaying,+Origin,+Offset,+Underlying,+Precedence)
%!      is det.
%
%       Overlay a map on top of another, respecting tile Precedence.
%
%       As overlay_map/4 but allows for an Offset term to reposition the
%       conetnts of the Overlaying map.
%
%       Offset is a term X/Y, added to the coordinate of each cell in
%       Overlaying before that cell is written on top of Underlying.
%
offset_map(map(_Id1,Ds_1,M1),X0/Y0,Xo/Yo,map(_Id2,Ds_2,M2),Ps):-
        forall(map_location(X1/Y1,T1,M1,Ds_1,true)
              ,(maplist(sum,[X0,Y0],[X1,Y1],[X2,Y2])
               ,maplist(sum,[X2,Y2],[Xo,Yo],[X3,Y3])
               ,map_location(X2/Y2,T2,M2,Ds_2,true)
               ,overlay(T1,T2,Ps,T2_)
               ,nb_write_to_location(X3/Y3,T2_,M2,Ds_2,true)
               %,nl, map_display:print_map(tiles,Ds_2,M2)
               )
              ).



%!      overlay_map(+Overlaying,+Origin,+Underlying,+Precedence) is det.
%
%       Overlay a map on top of another, respecting tile Precedence.
%
%       Overlaying and Underlying are two map/3 terms. Overlaying is the
%       map to be overlaeyd on top of the other map. Underlying is the
%       map to be overlayed on top-of.
%
%       Origin is a pair X/Y, the coordinates, in Underlying, where the
%       Overlaying map is to begin being written.
%
%       Precedence is a list of key-value pairs T1 > T2 where T1 and T2
%       are tile symbols. Precedence may be empty and controls how tiles
%       are written from Overlaying on top of tiles in Underlying. If a
%       term T1 > T2 exists in Precedence, then a tile T1 can be written
%       over a tile T2. Otherwise, T1 cannot be written on top of T2.
%       This doesn't cause the overlaying operation to fail - only to
%       ignore the attempt to write T1 on top of T2 unless T1 > T2 is
%       there.
%
overlay_map(map(_Id1,Ds_1,M1),X0/Y0,map(_Id2,Ds_2,M2),Ps):-
        forall(map_location(X1/Y1,T1,M1,Ds_1,true)
              ,(maplist(sum,[X0,Y0],[X1,Y1],[X2,Y2])
               ,map_location(X2/Y2,T2,M2,Ds_2,true)
               ,overlay(T1,T2,Ps,T2_)
               ,nb_write_to_location(X2/Y2,T2_,M2,Ds_2,true)
               %,nl, map_display:print_map(tiles,Ds_2,M2)
               )
              ).

sum(X,Y,Z):- Z is X + Y.


%!      overlay(+Tile1,+TIle2,+Precedence,-Tile) is det.
%
%       Determine whether Tile1 can overwrite Tile2.
%
%       Tile1, Tile2 are two tiles. T1 is a tile in a map to be
%       overlayed on a second map, and T2 is a tile in the second map,
%       to be overlayed.
%
%       Precedence is a list of key-value pairs T1 > T2, that determines
%       whether T1 is allowed to overwrite T2, or not. If T1 > T2 is not
%       in Precedence, then T1 is allowed to overwrite T2 by default. If
%       T2 > T1 is in precedence then T1 is not allowed to overwrite T2,
%       but T2 allowed to overwrite T1.
%
%       Tile is the tile to be written, depending on whether T1 or T2
%       has precedence to overwrite, according to Precedence.
%
overlay(T1,T2,Ps,T2):-
        memberchk(T2>T1,Ps)
        ,!.
overlay(T1,_T2,_Ps,T1).


%!      clear_marks(+Model) is det.
%
%       Remove marks from a slamming map.
%
clear_marks([]):-
        !.
clear_marks([_XY,map(_Id,Ds,M)]):-
        forall(map_location(X/Y,@,M,Ds,true)
              ,nb_write_to_location(X/Y,f,M,Ds,true)
              ).
