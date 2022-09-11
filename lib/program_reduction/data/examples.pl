:-module(examples, [example/2]).

/** <module> Examples to test program and clause reduction.

*/

/*
Little gotcha: when a set of clauses is given as a list,
identically-named variables will unify and numbervars will bind them to
the same constants. This will make some queries true that shouldn't be.
Make sure to number each variable according to the clause it belongs, to
standardise them apart and avoid this.

The same seems to go for constants, also, in some cases at least. I'm
not sure what is the best thing to do with _numeric_ constants.
*/

%!	example(?Id, ?Program) is det.
%
%	An example of a Program to be reduced.
%

example(recipe, [m(recipe,[egg_whisk,eggs,frying_pan,olive_oil,pepper,salt],[omelette])
                ,(m(_P1,A1,B1):-m(whisk_eggs,A1,C1),m(_Q1,C1,B1))
                ,(m(_P2,A2,B2):-m(heat_oil,A2,C2),m(_Q2,C2,B2))
                ,(m(_P3,A3,B3):-m(break_eggs,A3,C3),m(_Q3,C3,B3))
                ,(m(_Q4,A4,B4):-m(heat_oil,A4,C4),m(_R4,C4,B4))
                ,(m(_Q5,A5,B5):-m(whisk_eggs,A5,C5),m(_R5,C5,B5))
                ,(m(_R6,A6,B6):-m(fry_eggs,A6,C6),m(season,C6,B6))
                ,(m(recipe,A7,B7):-m(break_eggs,A7,C7),m(_P7,C7,B7))
                ,(m(recipe,A8,B8):-m(heat_oil,A8,C8),m(_P8,C8,B8))
                ,(m(break_eggs,Xs1,Ys1):- m(replace,[eggs],Xs1,[egg_whites,egg_yolks],Ys1))
                ,(m(whisk_eggs,Xs2,Ys2):- m(replace,[egg_whisk,egg_whites,egg_yolks],Xs2,[whisked_eggs],Ys2))
                ,(m(heat_oil,Xs3,Ys3):- m(replace,[frying_pan,olive_oil],Xs3,[frying_oil],Ys3))
                ,(m(fry_eggs,Xs4,Ys4):- m(replace,[frying_oil,whisked_eggs],Xs4,[frying_eggs],Ys4))
                ,(m(season,Xs5,Ys5):- m(replace,[frying_eggs,pepper,salt],Xs5,[omelette],Ys5))
                ,(m(replace,Xs6,Is6,Ys6,Os6):- m(ground,Xs6) ,m(ground,Is6) ,m(ground,Ys6) ,m(ord_subset,Xs6,Is6) ,m(ord_subtract,Is6,Xs6,Zs_) ,m(ord_union,Ys6,Zs_,Os6))
                ,(m(ground,X7):-ground(X7))
                ,(m(ord_subset,X8,Y8):-ord_subset(X8,Y8))
                ,(m(ord_subtract,X9,Y9,Z9):- ord_subtract(X9,Y9,Z9))
                ,(m(ord_union,X10,Y10,Z10) :- ord_union(X10,Y10,Z10))
                ]).


% Forgot to standardise apart but still get correct reduction.
example('kin_a', [(m(fm,parent))
		 ,(m(P1,X1,Y1) :- m(_Q1,X1,Y1), m(fm,P1))
		 ,(m(P2,X2,Y2) :- m(_Q2,Y2,X2), m(fm,P2))
		 ,(m(child,X3,Y3) :- m(father,Y3,X3))
		 ,(m(child,X4,Y4) :- m(mother,Y4,X4))
		 ,(m(parent,X5,Y5) :- m(father,X5,Y5))
		 ,(m(parent,X6,Y6) :- m(mother,X6,Y6))
		 ]).

example('kin_b', [(m(fm,child))
		 ,(m(P1,X1,Y1) :- m(_Q1,X1,Y1), m(fm,P1))
		 ,(m(P2,X2,Y2) :- m(_Q2,Y2,X2), m(fm,P2))
		 ,(m(child,X3,Y3) :- m(father,Y3,X3))
		 ,(m(child,X4,Y4) :- m(mother,Y4,X4))
		 ,(m(parent,X5,Y5) :- m(father,X5,Y5))
		 ,(m(parent,X6,Y6) :- m(mother,X6,Y6))
		 ]).


% Example from CProgol 4.4.
% Just to know we're on the same page.
example('red', [(m(_P1,X1,Y1) :- m(_Q1,Y1,X1))
	       ,(m(_P2,X2,Y2) :- m(_Q2,X2,Y2))
	       ,(m(_P3,X3,Y3) :- m(_Q3,X3,Z3), m(_R3,Z3,Y3))
	       ]).

% 14 encapsulated metarules
% Variables standardised apart
example('0b', [(m(_P1,X1,Y1) :- m(_Q1,X1,Y1)) % serial
	      ,(m(_P2,X2,Y2) :- m(_Q2,Y2,X2)) % inverse
	      ,(m(_P3,X3,Y3) :- m(_Q3,X3,Y3), m(_R3,X3,Y3))
	      ,(m(_P4,X4,Y4) :- m(_Q4,X4,Y4), m(_R4,Y4,X4))
	      ,(m(_P5,X5,Y5) :- m(_Q5,X5,Z5), m(_R5,Y5,Z5)) % stack
	      ,(m(_P6,X6,Y6) :- m(_Q6,X6,Z6), m(_R6,Z6,Y6)) % chain
	      ,(m(_P7,X7,Y7) :- m(_Q7,Y7,X7), m(_R7,X7,Y7))
	      ,(m(_P8,X8,Y8) :- m(_Q8,Y8,X8), m(_R8,Y8,X8))
	      ,(m(_P9,X9,Y9) :- m(_Q9,Y9,Z9), m(_R9,X9,Z9))
	      ,(m(_P10,X10,Y10) :- m(_Q10,Y10,Z10), m(_R10,Z10,X10))
	      ,(m(_P11,X11,Y11) :- m(_Q11,Z11,X11), m(_R11,Y11,Z11))
	      ,(m(_P12,X12,Y12) :- m(_Q12,Z12,X12), m(_R12,Z12,Y12))
	      ,(m(_P13,X13,Y13) :- m(_Q13,Z13,Y13), m(_R13,X13,Z13))
	      ,(m(_P14,X14,Y14) :- m(_Q14,Z14,Y14), m(_R14,Z14,X14))
	      ]).

% Same as 0b but with variables named similar to the top-level printing
% for easier reading.
example('0b_ab', [(m(_A1,B1,C1) :- m(_D1,B1,C1)) % serial
		 ,(m(_A2,B2,C2) :- m(_D2,C2,B2)) % inverse
		 ,(m(_A3,B3,C3) :- m(_D3,B3,C3), m(_E3,B3,C3))
		 ,(m(_A4,B4,C4) :- m(_D4,B4,C4), m(_E4,C4,B4))
		 ,(m(_A5,B5,C5) :- m(_D5,B5,E5), m(_F5,C5,E5)) % stack
		 ,(m(_A6,B6,C6) :- m(_D6,B6,E6), m(_F6,E6,C6)) % chain
		 ,(m(_A7,B7,C7) :- m(_D7,C7,B7), m(_E7,B7,C7))
		 ,(m(_A8,B8,C8) :- m(_D8,C8,B8), m(_E8,C8,B8))
		 ,(m(_A9,B9,C9) :- m(_D9,C9,E9), m(_F9,B9,E9))
		 ,(m(_A10,B10,C10) :- m(_D10,C10,E10), m(_F10,E10,B10))
		 ,(m(_A11,B11,C11) :- m(_D11,E11,B11), m(_F11,C11,E11))
		 ,(m(_A12,B12,C12) :- m(_D12,E12,B12), m(_F12,E12,C12))
		 ,(m(_A13,B13,C13) :- m(_D13,E13,C13), m(_F13,B13,E13))
		 ,(m(_A14,B14,C14) :- m(_D14,E14,C14), m(_F14,E14,B14))
	      ]).

% Just the chain and stack metarules.
% No rule is found redundant- CProgol agrees btw.
example('0b_stack_chain', [(m(_A5,B5,C5) :- m(_D5,B5,E5), m(_F5,C5,E5)) % stack
			  ,(m(_A6,B6,C6) :- m(_D6,B6,E6), m(_F6,E6,C6)) % chain
			  ]).

% First two of 0b only
% Order should not matter.
example('0b1', [(m(_P1,X1,Y1) :- m(_Q1,X1,Y1))
	      ,(m(_P2,X2,Y2) :- m(_Q2,Y2,X2))
	       ]).

example('0b2', [(m(_P2,X2,Y2) :- m(_Q2,Y2,X2))
	      ,(m(_P1,X1,Y1) :- m(_Q1,X1,Y1))
	       ]).

% "Reified" metarules, standardised apart
% Order is messed up - eg. chain is now 4th. Needs to be done properly.
example('0d', [(p(X1,Y1) :- q(X1,Y1))
	      ,(p(X2,Y2) :- q(Y2,X2))
	      ,(p(X3,Y3) :- q(X3,Y3),r(X3,Y3))
	      ,(p(X4,Y4) :- q(X4,Y4),r(Y4,X4))
	      ,(p(X5,Y5) :- q(X5,Z5),r(Z5,Y5))
	      ,(p(X6,Y6) :- q(X6,Z6),r(Y6,Z6))
	      ,(p(X7,Y7) :- q(Y7,X7),r(X7,Y7))
	      ,(p(X8,Y8) :- q(Y8,X8),r(Y8,X8))
	      ,(p(X9,Y9) :- q(Y9,Z9),r(X9,Z9))
	      ,(p(X10,Y10) :- q(Y10,Z10),r(Z10,X10))
	      ,(p(X11,Y11) :- q(Z11,X11),r(Y11,Z11))
	      ,(p(X12,Y12) :- q(Z12,X12),r(Z12,Y12))
	      ,(p(X13,Y13) :- q(Z13,Y13),r(X13,Z13))
	      ,(p(X14,Y14) :- q(Z14,Y14),r(Z14,X14))
	      ]).

% DCG version of first two m/3 clauses.
example('0e', [(m(_P1, X1, Y1, [true|Z1], U1):-m(_Q1, X1, Y1, Z1, U1))
	      ,(m(_P2, X2, Y2, [true|Z2], U2):-m(_Q2, Y2, X2, Z2, U2))
	      ]).


% Pairwise reductions
% This one is the same as 0b2, but repeated here to have them all nicely
% together.
% The first clause (serial metarule) is dropped
example('0f0', [(m(_P1,X1,Y1) :- m(_Q1,X1,Y1))
	       ,(m(_A2,B2,C2) :- m(_D2,C2,B2)) % inverse
	       ]).

% The new clause (3d) is dropped
example('0f1', [(m(_A2,B2,C2) :- m(_D2,C2,B2)) % inverse
	       ,(m(_A3,B3,C3) :- m(_D3,B3,C3), m(_F3,B3,C3))
	       ]).

% The new clause (4th) is dropped.
example('0f2', [(m(_A2,B2,C2) :- m(_D2,C2,B2)) % inverse
	       ,(m(_A4,B4,C4) :- m(_D4,B4,C4), m(_F4,C4,B4))
	       ]).

% Both clauses are added to the reduction
% That is probably wrong already.
example('0f3', [(m(_A2,B2,C2) :- m(_D2,C2,B2)) % inverse
	       ,(m(_A5,B5,C5) :- m(_D5,B5,E5), m(_F5,C5,E5))
	      ]).

% The previously added clause (5th) is dropped
% So the inverse (2nd) and chain (6th) are now in the reduction.
example('0f4', [(m(_A2,B2,C2) :- m(_D2,C2,B2)) % inverse
	       ,(m(_A5,B5,C5) :- m(_D5,B5,E5), m(_F5,C5,E5))
	       ,(m(_A6,B6,C6) :- m(_D6,B6,E6), m(_F6,E6,C6)) % chain
	      ]).

% New clause (7th) is dropped
% Inverse and chain are in the reduction.
example('0f5', [(m(_A2,B2,C2) :- m(_D2,C2,B2)) % inverse
	       ,(m(_A6,B6,C6) :- m(_D6,B6,E6), m(_F6,E6,C6)) % chain
	       ,(m(_A7,B7,C7) :- m(_D7,C7,B7), m(_F7,B7,C7))
	      ]).

% With derivation_depth(50), resolutions(100) chain and inverse are kept
% only and 8th is dropped.
% This is correct- but note that it depends on the depth and
% resolutions. Different settings make it give incorrect results again,
% e.g. depth 10 and resolutions 100 drops inverse. It's a bit of a
% headache.
example('0f6', [(m(_A2,B2,C2) :- m(_D2,C2,B2)) % inverse
	       ,(m(_A6,B6,C6) :- m(_D6,B6,E6), m(_F6,E6,C6)) % chain
	       ,(m(_A8,B8,C8) :- m(_D8,C8,B8), m(_F8,C8,B8))
	      ]).

% After fixing the clause/2 backtracking issue, processing doesn't go
% infinite anymore. However, in 0f6 the inverse _can_ still be dropped
% from the reduction - depending on settings. So I can't be bothered to
% continue with subsequent pairwise reductions. Your best bet is to make
% sure you have as many metarules as possible in the original program I
% guess...
example('0f7', [(m(_A6,B6,C6) :- m(_D6,B6,E6), m(_F6,E6,C6)) % chain
	       ,(m(_A8,B8,C8) :- m(_D8,C8,B8), m(_F8,C8,B8))
	       ,(m(_A9,B9,C9) :- m(_D9,C9,E9), m(_F9,B9,E9))
	       ]).

% 0f7 reordered to put chain second, just in case order matters
% It doesn't
example('0f7_b', [(m(_A8,B8,C8) :- m(_D8,C8,B8), m(_F8,C8,B8))
		 ,(m(_A6,B6,C6) :- m(_D6,B6,E6), m(_F6,E6,C6)) % chain
		 ,(m(_A9,B9,C9) :- m(_D9,C9,E9), m(_F9,B9,E9))
		 ]).

% With derivation_depth(50), resolutions(100) this correctly drops 9th.
example('0f7_c', [(m(_A2,B2,C2) :- m(_D2,C2,B2)) % inverse
		 ,(m(_A6,B6,C6) :- m(_D6,B6,E6), m(_F6,E6,C6)) % chain
		 ,(m(_A9,B9,C9) :- m(_D9,C9,E9), m(_F9,B9,E9))
		 ]).

example('1a1', [(p(X1) :- q(X1))
	      ,(p(X2) :- q(X2), r(X2))
	       ]).

example('1a2', [(p(X2) :- q(X2), r(X2))
	       ,(p(X1) :- q(X1))
	       ]).

example('1b1', [(p(X1) :- q(X1))
	       ,(p(X2) :- q(X2), r(X2))
	       ,(p(X3) :- q(X3), r(X3), r(X3))
	       ]).

example('1b2', [(p(X3) :- q(X3), r(X3), r(X3))
	       ,(p(X2) :- q(X2), r(X2))
	       ,(p(X1) :- q(X1))
	       ]).

example('1c', [ (p(X1) :- q(X1))
	      ,(p(a2) :- q(_Y2))
	      ,(p(_X3) :- q(a3))
	      ,(p(a4) :- q(b4))
	      ,(p(a5) :- q(a5))
	      ,(p(X6) :- q(X6), r(X6))
	      ,(p(X7) :- q(_Y7), r(X7))
	      ,(p(_X8) :- q(Y8), r(Y8))
	      ,(p(_X9) :- q(_Y9), r(_Z9))
	      ,(p(a10) :- q(_Y10), r(_Z10))
	      ,(p(_X11) :- q(a), r(_Z11))
	      ,(p(_X12) :- q(_Y12), r(a12))
	      ,(p(a13) :- q(b13), r(_Z13))
	      ,(p(a14) :- q(_Y14), r(b14))
	      ,(p(_X15) :- q(a15), r(b15))
	      ,(p(a16) :- q(b16), r(c16))
	      ]).

example('2a1', [(lessthan(X1,Y1) :- successor(X1,Y1))
	       ,(lessthan(X2,Y2) :- successor(X2,Z2), successor(Z2,Y2))
	       ,(lessthan(X3,Y3) :- successor(X3,Z3), lessthan(Z3,Y3))
		% ^^ Watch it. Goes infinite!
	       ]).

example('2a2', [(lessthan(X1,Y1) :- successor(X1,Y1))
	       ,(lessthan(X3,Y3) :- successor(X3,Z3), lessthan(Z3,Y3))
	       ,(lessthan(X2,Y2) :- successor(X2,Z2), successor(Z2,Y2))
	       ]).

example('2a3', [(lessthan(X2,Y2) :- successor(X2,Z2), successor(Z2,Y2))
	       ,(lessthan(X1,Y1) :- successor(X1,Y1))
	       ,(lessthan(X3,Y3) :- successor(X3,Z3), lessthan(Z3,Y3))
	       ]).

example('2a4', [(lessthan(X2,Y2) :- successor(X2,Z2), successor(Z2,Y2))
	       ,(lessthan(X3,Y3) :- successor(X3,Z3), lessthan(Z3,Y3))
	       ,(lessthan(X1,Y1) :- successor(X1,Y1))
	       ]).

example('2a5', [(lessthan(X3,Y3) :- successor(X3,Z3), lessthan(Z3,Y3))
	       ,(lessthan(X1,Y1) :- successor(X1,Y1))
	       ,(lessthan(X2,Y2) :- successor(X2,Z2), successor(Z2,Y2))
	       ]).

example('2a6', [(lessthan(X3,Y3) :- successor(X3,Z3), lessthan(Z3,Y3))
	       ,(lessthan(X2,Y2) :- successor(X2,Z2), successor(Z2,Y2))
	       ,(lessthan(X1,Y1) :- successor(X1,Y1))
	       ]).

example('2c', [(lessthan(X1,Y1) :- successor(X1,Y1))
	      ,(lessthan(X2,Y2) :- successor(X2,Z2), successor(Z2,Y2))
	      ,(lessthan(X3,Y3) :- successor(X3,Z3), lessthan(Z3,Y3))
	      ,(lessthan(U,V) :- successor(U,W), successor(W,X), successor(X,V))
	      ,(lessthan(X4,Y4) :- successor(Y4,X4))
	      ]).

example(3, [(q(X1) :- p(X1))
	   ]).

example(4, [(q(X1) :- p(X1))
	   ,(q(X2) :- a(X2))
	   ]).

example('4a', [(q(X2) :- a(X2))
	      ,(q(X1) :- p(X1))
	      ]).

% ===== Examples of contextual redundancy from the Fonseca paper =====
%
% Identity
%
% "The clause C is contextual redundant in S U {C} if S entails C" (S is
% a set of hypothesis clauses generated so-far)
example('5a', [(p(X1) :- a(X1, _Y1))
	      ,(p(X2) :- a(X2, _Y2))
	      ]).

% Commutativity
%
% In this case, we want the constants to be shared.
example('5b', [(p(X1) :- a(d,X1),a(c,X1))
	      ,(p(X2) :- a(c,X2), a(d,X2))
	      ]).

% Transitivity
%
% gt is the greater-than relation. In the Fonseca paper, the example is
% that p(X,Y,Z) <- X > Y, Y > Z is transitive redundant in a set S
% containing p(X, Y, Z) <- X > Y and p(X, Y, Z) <- Y > Z. However, in
% Swi Prolog (and, I believe, any Prolog) trying to assert body literals
% that are built-ins, like >/2, raises an error, because we're trying to
% redefine a system predicate. I think the transitivity of the gt/2
% relation is still obvious to the interpreter, though.
example('5c1', [(p(X1,Y1,_Z1) :- gt(X1, Y1))
	       ,(p(X2,_Y2,Z2) :- gt(X2, Z2))
	       ,(p(X3,Y3,Z3) :- gt(X3, Y3), gt(X3, Z3))
	       ]).

example('5c2', [(p(X3,Y3,Z3) :- gt(X3, Y3), gt(X3, Z3))
	       ,(p(X2,_Y2,Z2) :- gt(X2, Z2))
	       ,(p(X1,Y1,_Z1) :- gt(X1, Y1))
	       ]).

% Direct equivalence
%
% Informally, two clauses, C and D are directly equivalent redundant if
% they each have a single body literal not shared with the other and
% those literals are direct equivalents to each other.
example('5d1', [(p(X1) :- gt(X1, 1))
	       ,(p(X2) :- lte(1, X2)) % lte/2 is =< in the original example
	       ]).

example('5d2', [(p(X2) :- lte(1, X2))
	       ,(p(X1) :- gt(X1, 1))
	       ]).

% Direct entailment
%
% Informally, two clauses C and D are directly entailed redundant if a
% literal in the body of C would be directly entailed by some literals
% in the body of D. If I understand this correctly.
example('5e1', [(p(X1) :- gte(X1, 1), lte(X1, 1))
	       ,(p(X2) :- eq(1, X2)) % eq/2 is equality, in the original
	       ]).

example('5e2', [(p(X2) :- eq(1, X2))
	       ,(p(X1) :- gte(X1, 1), lte(X1, 1))
	       ]).

example(6,[(pltkn([], _, R1, R1, Hr1, Hr1):- cut)
	  ,(pltkn([C2|Cs2], H02, Acc_H02, Bind_H02, Acc_Hr2, Bind_Hr2):-
	   slct(C2, H02, H02_)
	   ,gnrlis(H02_, C2)
	   ,cut
	   ,pltkn(Cs2, H02_, Acc_H02, Bind_H02, [C2|Acc_Hr2], Bind_Hr2)
	   )
	  ,(pltkn([C3|Cs3], H03, Acc_H03, Bind_H03, Acc_Hr3, Bind_Hr3):-
	   pltkin(Cs3, H03, [C3|Acc_H03], Bind_H03, Acc_Hr3, Bind_Hr3)
	   )
	  ]).

% ==== Some common Prolog predicates

% append/3, reverse/2, member/2, takeout/3 perm/2 and intersection/2
% from: https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_7.html
example(7, [(append([],L1,L1))
	   ,(append([H|T],L2,[H|L3])  :-  append(T,L2,L3))
	   ]).
example(8, [(reverse([X1|Y1],Z1,W1) :- reverse(Y1,[X1|Z1],W1))
	   ,(reverse([],X2,X2))
	   ]).

example(9, [(list_member(X1,[X1|_R]))
	   ,(list_member(X2,[_Y|R2]) :- list_member(X2,R2))
	   ]).

example(10, [(takeout(X1,[X1|R1],R1))
	    ,(takeout(X2,[F|R2],[F|S]) :- takeout(X2,R2,S))
	    ]).

example(11, [(perm([X|Y],Z) :- perm(Y,W), takeout(X,Z,W))
	    ,(perm([],[]))
	    ]).

example(12, [(set_intersection([X1|Y1],M1,[X1|Z1]):-
	     list_member(X1,M1)
	     ,set_intersection(Y1,M1,Z1))
	    ,(set_intersection([X2|Y2],M2,Z2):-
	     negation
	     ,list_member(X2,M2)
	     ,set_intersection(Y2,M2,Z2))
	    ,(set_intersection([],_M3,[]))
	    ]).
