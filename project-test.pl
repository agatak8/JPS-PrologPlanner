plan_wrapper(InitState, Goals, Limit, Plan, FinalState) :-
	consult('helpers.pl'),
	consult('debug.pl'),
	%is_between(0, MaxLimit, Limit),
	my_trace_rec(2, plan_wrapper, 1, 0, plan),
	intersect(Goals, InitState, InitAchievedGoals),
	plan(InitState, Goals, InitAchievedGoals, Limit, Plan, FinalState, 0),% !,
	my_trace_rec(3, plan_wrapper, 1, 0, plan, ['InitState'/InitState, 'Goals'/Goals, 'AchievedGoals'/InitAchievedGoals, 'Limit'/Limit, 'RecursionLevel'/0, 'FinalState'/FinalState, 'Plan'/Plan]).  

%plan_wrapper(InitState, Goals, Limit, Plan, FinalState) :-
%    NewLimit is Limit + 1,
%    plan_wrapper(InitState, Goals, NewLimit, Plan, FinalState).
	
plan(State, Goals, AchievedGoals, Limit, [], State, RecursionLevel) :-
    my_trace_rec(1, plan, 1, RecursionLevel, ['State'/State, 'Goals'/Goals, 'AchievedGoals'/AchievedGoals, 'Limit'/Limit, 'RecursionLevel'/RecursionLevel]),
	my_trace_rec(2, plan, 1, RecursionLevel, goals_achieved),
	goals_achieved(Goals, State),
    my_trace_rec(3, plan, 1, RecursionLevel, goals_achieved, ['result'/true]).
    
plan(_, _, _, Limit, _, _, RecursionLevel) :-
    Limit =< 0,
    write('Cele niespełnione i limit <= 0. Nastąpi nawrót. Poziom rekurencji: '),
    write_ln(RecursionLevel),
    fail.
	
plan(InitState, Goals, AchievedGoals, Limit, Plan, FinalState, RecursionLevel) :-
	my_trace_rec(1, plan, 2, RecursionLevel, ['InitState'/InitState, 'Goals'/Goals, 'AchievedGoals'/AchievedGoals, 'Limit'/Limit, 'RecursionLevel'/RecursionLevel]),
	Limit > 0,
	is_between(0, Limit, LimitPre),
	%is_between(0, Limit, LimitPre),
	my_trace_rec(2, plan, 2, RecursionLevel, choose_goal),
	choose_goal(Goal, Goals, RestGoals, InitState),
	my_trace_rec(3, plan, 2, RecursionLevel, choose_goal, ['Goal'/Goal, 'Goals'/Goals, 'RestGoals'/RestGoals, 'InitState'/InitState ]),
	my_trace_rec(2, plan, 2, RecursionLevel, achieves),
	achieves(Goal, Action),
	my_trace_rec(3, plan, 2, RecursionLevel, achieves, ['Goal'/Goal, 'Action'/Action]),
	my_trace_rec(2, plan, 2, RecursionLevel, requires),
	requires(Action, CondGoals, Conditions),
	my_trace_rec(3, plan, 2, RecursionLevel, requires, ['Action'/Action, 'CondGoals'/CondGoals, 'Conditions'/Conditions]),
	Rec2 is RecursionLevel + 1,
	my_trace_rec(2, plan, 2, RecursionLevel, plan),
	plan(InitState, CondGoals, AchievedGoals, LimitPre, PrePlan, State1, Rec2),
	my_trace_rec(3, plan, 2, RecursionLevel, plan, ['InitState'/InitState, 'Goals'/Goals, 'LimitPre'/LimitPre, 'RecursionLevel'/RecursionLevel, 'State1'/State1, 'PrePlan'/PrePlan]),
	my_trace_rec(2, plan, 2, RecursionLevel, inst_action),
	inst_action(Action, Conditions, State1, InstAction),
	my_trace_rec(3, plan, 2, RecursionLevel, inst_action, ['Action'/Action, 'Conditions'/Conditions, 'State1'/State1, 'InstAction'/InstAction]),
	my_trace_rec(2, plan, 2, RecursionLevel, check_action),
	check_action(InstAction, AchievedGoals),
	my_trace_rec(3, plan, 2, RecursionLevel, check_action, ['result'/true]),
	my_trace_rec(2, plan, 2, RecursionLevel, perform_action),
	perform_action(State1, InstAction, State2), !,
	my_trace_rec(3, plan, 2, RecursionLevel, perform_action, ['State1'/State1, 'InstAction'/InstAction, 'State2'/State2]),
	LimitPost is Limit - LimitPre - 1,
	my_trace_rec(2, plan, 2, RecursionLevel, plan),
	plan(State2, RestGoals, [Goal | AchievedGoals], LimitPost, PostPlan, FinalState, Rec2),
	my_trace_rec(3, plan, 2, RecursionLevel, plan, ['State2'/State2, 'RestGoals'/RestGoals, 'LimitPost'/LimitPost, 'RecursionLevel'/RecursionLevel, 'FinalState'/FinalState, 'PostPlan'/PostPlan]),
	conc(PrePlan, [InstAction | PostPlan ], Plan),
	my_trace_rec(4, plan, 2, RecursionLevel, ['InitState'/InitState, 'Goals'/Goals, 'Limit'/Limit, 'RecursionLevel'/RecursionLevel, 'FinalState'/FinalState, 'Plan'/Plan]).

goals_achieved([], _).

goals_achieved([HeadGoal | Rest], UnitedState) :-
	goal_achieved(HeadGoal, UnitedState),
	goals_achieved(Rest, UnitedState).
	
goal_achieved(on(A, B), UnitedState) :-
	non_slash(A),
	non_slash(B),
	member(on(A, B), UnitedState).

goal_achieved(clear(A), State) :-
	non_slash(A),
	member(clear(A), State).
	
goal_achieved(clear(A/Goal), State) :-
	nonvar(Goal),
	goal_achieved(Goal, State),
	member(clear(A), State).

goal_achieved(on(A, B/Goal2), UnitedState) :-
	non_slash(A),
	nonvar(Goal2),
	goal_achieved(Goal2, UnitedState),
	member(on(A, B), UnitedState).

% generalization only
goal_achieved(on(A/Goal, B), UnitedState) :- 
	nonvar(Goal),
	goal_achieved(Goal, UnitedState),
	member(on(A, B), UnitedState).
	
goal_achieved(on(A/Goal, B/Goal2), UnitedState) :-
	nonvar(Goal),
	nonvar(Goal2),
	goal_achieved(Goal, UnitedState),
	goal_achieved(Goal2, UnitedState),
	member(on(A, B), UnitedState).
	

choose_goal(Goal, [Goal | Goals], Goals, UnitedState) :-
	not(goal_achieved(Goal, UnitedState)).
	
choose_goal(Goal, [X | Goals], [X | TailRestGoals], UnitedState) :-
	choose_goal(Goal, Goals, TailRestGoals, UnitedState).
	
achieves(on(A, B), move(A, Y/(on(A, Y)), B)).

achieves(clear(B), move(A/on(A,B), B, C)).

requires(move(What, From, On), [clear(What), clear(On)], [on(What, From)]) :-
	non_slash(From),
	nonvar(What),
	nonvar(On).
	
requires(move(What, From/on(What, From), On), [clear(What), clear(On)], [on(What, From)]) :-
	nonvar(What),
	nonvar(On).

requires(move(What, From, On), [clear(What)], [clear(On), safe_diff(On, What)]) :-
	var(On).
	
check_action(_, []).

check_action(InstAction, [Goal|Rest]) :-
    my_trace_rec(1, check_action, 2, -1, ['InstAction'/InstAction, 'Goal'/Goal]),
    my_trace_rec(2, check_action, 2, -1, action_destroys_goal),
	not(action_destroys_goal(InstAction, Goal)),
	check_action(InstAction, Rest).

action_destroys_goal(move(What, From, _), on(What, From)) :-
    write_ln('chosen action would destroy goal').

action_destroys_goal(move(_, _, On), clear(On)) :- write_ln('chosen action would destroy goal').

inst_action(move(What, From, On), Conds, UnitedState, move(InstWhat, InstFrom, InstOn)) :-
	inst_test(What, UnitedState, InstWhat, move(What, From, On)),
	inst_test(From, UnitedState, InstFrom, move(InstWhat, From, On)),
	inst_test(On, UnitedState, InstOn, move(InstWhat, InstFrom, On)),
	conds_achieved(Conds, UnitedState).

conds_achieved([], _).

conds_achieved([safe_diff(A, B) | Rest], UnitedState) :-
    non_slash(A),
    non_slash(B),
    conds_achieved(Rest, UnitedState), !,
	safe_diff(A, B).
	
conds_achieved([safe_diff(A, B/W) | Rest], UnitedState) :-
    non_slash(A),
	goal_achieved(W, UnitedState),
	conds_achieved(Rest, UnitedState), !,
	safe_diff(A, B).

conds_achieved([safe_diff(A/W, B) | Rest], UnitedState) :-
    non_slash(B),
	goal_achieved(W, UnitedState),
	conds_achieved(Rest, UnitedState), !,
	safe_diff(A, B).

conds_achieved([safe_diff(A/W, B/W2) | Rest], UnitedState) :-
	goal_achieved(W, UnitedState),
	goal_achieved(W2, UnitedState),
	conds_achieved(Rest, UnitedState), !,
	safe_diff(A, B).

conds_achieved([HeadGoal | Rest], UnitedState) :-
	HeadGoal \= safe_diff(_, _),
	goal_achieved(HeadGoal, UnitedState),
	conds_achieved(Rest, UnitedState).

inst_test(A, US, UI, _) :-
    nonvar(A),
    inst_one(A, US, UI).
	
inst_test(A, US, UI, Action) :-
    var(A),
    prompt_user_input(A, US, UI, Action).

prompt_user_input(On, UnitedState, UserInput, move(What, _, On)) :-
    write("Stan: "),
    write_ln(UnitedState),
    write("Podaj gdzie chcesz postawić klocka "),
    write(What),
    write_ln(":"),
    read(UserInput),
    process_user_input(On, UnitedState, UserInput), !.

process_user_input(A, US, UserInput) :-
    UserInput \= 'cofnij'.

process_user_input(A, US, UserInput) :-
    UserInput \= 'cofnij',
    prompt_user_input(A, US, UserInput).
      
	
inst_one(A/B, UnitedState, A) :-
    % zeby nie generowac nowych zmiennych postaci Zmienna/Zmienna2 po nawrocie
    var(A),
    var(B),
	fail.
	
inst_one(A/B, UnitedState, A) :-
    nonvar(B),
	goal_achieved(B, UnitedState).
	
inst_one(A, UnitedState, A) :-
	% uncomment for testing 
	nonvar(A),
	non_slash(A).

perform_action(State1, move(What, From, On), [on(What, On), clear(From) | PartialRes]) :-
    member(clear(On), State1),
    member(on(What, From), State1),
    delete(State1, clear(On), PartialPartialRes),
    delete(PartialPartialRes, on(What, From), PartialRes).

