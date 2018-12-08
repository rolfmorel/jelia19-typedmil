:- use_module('metagol-typed').
:- use_module(library(system)).
metagol:max_clauses(3).

metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
my_succ1(X,Y):-Y is X+1,Y<5000.
my_succ2(X,Y):-Y is X+1,Y<5000.
my_succ3(X,Y):-Y is X+1,Y<5000.
my_succ4(X,Y):-Y is X+1,Y<5000.
my_succ5(X,Y):-Y is X+1,Y<5000.
my_succ6(X,Y):-Y is X+1,Y<5000.
my_succ7(X,Y):-Y is X+1,Y<5000.
my_succ8(X,Y):-Y is X+1,Y<5000.
my_succ9(X,Y):-Y is X+1,Y<5000.
my_succ10(X,Y):-Y is X+1,Y<5000.
my_succ11(X,Y):-Y is X+1,Y<5000.
my_succ12(X,Y):-Y is X+1,Y<5000.
my_succ13(X,Y):-Y is X+1,Y<5000.
my_succ14(X,Y):-Y is X+1,Y<5000.
my_succ15(X,Y):-Y is X+1,Y<5000.
sucker16(X,Y):-Y is X+1,Y<5000.
sucker17(X,Y):-Y is X+1,Y<5000.
sucker18(X,Y):-Y is X+1,Y<5000.
sucker19(X,Y):-Y is X+1,Y<5000.
sucker20(X,Y):-Y is X+1,Y<5000.
prim(my_succ1,[int,int]).
prim(my_succ2,[int,int]).
prim(my_succ3,[int,int]).
prim(my_succ4,[int,int]).
prim(my_succ5,[int,int]).
prim(my_succ6,[int,int]).
prim(my_succ7,[int,int]).
prim(my_succ8,[int,int]).
prim(my_succ9,[int,int]).
prim(my_succ10,[int,int]).
prim(my_succ11,[int,int]).
prim(my_succ12,[int,int]).
prim(my_succ13,[int,int]).
prim(my_succ14,[int,int]).
prim(my_succ15,[int,int]).
prim(sucker16,[bottom,bottom]).
prim(sucker17,[bottom,bottom]).
prim(sucker18,[bottom,bottom]).
prim(sucker19,[bottom,bottom]).
prim(sucker20,[bottom,bottom]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  catch(call_with_time_limit(MaxTime, (learntyped([p(1,0)],[],[int,int],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  format('%prog,~w\n',[H]),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,num_well_typed,15\n"),
  format("%data,total_preds,20\n"),
  format("%data,types_enabled,True\n").
