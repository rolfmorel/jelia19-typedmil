:- use_module('metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).

tail([_|T],T).

prim(tail/2).
prim(reverse/2).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
my_last0(A,B):-last(A,B).
my_max_list1(A,B):-max_list(A,B).
my_last2(A,B):-last(A,B).
my_tail3([_|TL],TL).
my_succ4(A,B):-succ(A,B).
my_pred5(A,B):-succ(B,A).
my_sumlist6(A,B):-sumlist(A,B).
my_tail7([_|TL],TL).
my_pred8(A,B):-succ(B,A).
my_succ9(A,B):-succ(A,B).
my_pred10(A,B):-succ(B,A).
my_max_list11(A,B):-max_list(A,B).
my_min_list12(A,B):-min_list(A,B).
my_max_list13(A,B):-max_list(A,B).
my_succ14(A,B):-succ(A,B).
my_len15(A,B):-length(A,B).
my_min_list16(A,B):-min_list(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_min_list19(A,B):-min_list(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_pred21(A,B):-succ(B,A).
prim(my_last0/2).
prim(my_max_list1/2).
prim(my_last2/2).
prim(my_tail3/2).
prim(my_succ4/2).
prim(my_pred5/2).
prim(my_sumlist6/2).
prim(my_tail7/2).
prim(my_pred8/2).
prim(my_succ9/2).
prim(my_pred10/2).
prim(my_max_list11/2).
prim(my_min_list12/2).
prim(my_max_list13/2).
prim(my_succ14/2).
prim(my_len15/2).
prim(my_min_list16/2).
prim(my_sumlist17/2).
prim(my_sumlist18/2).
prim(my_min_list19/2).
prim(my_sumlist20/2).
prim(my_pred21/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  catch(call_with_time_limit(MaxTime, (learn(Pos,[],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([['e','p','m'],['o','k','i'],['t','v','u']],[['e','p'],['o','k'],['t','v']]).
p([['p','i','d'],['s','x','r','i']],[['p','i'],['s','x','r']]).
