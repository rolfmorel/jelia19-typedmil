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
my_sumlist0(A,B):-sumlist(A,B).
my_max_list1(A,B):-max_list(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_min_list3(A,B):-min_list(A,B).
my_succ4(A,B):-succ(A,B).
my_pred5(A,B):-succ(B,A).
my_pred6(A,B):-succ(B,A).
my_last7(A,B):-last(A,B).
my_last8(A,B):-last(A,B).
my_min_list9(A,B):-min_list(A,B).
my_succ10(A,B):-succ(A,B).
my_head11([H|_],H).
my_min_list12(A,B):-min_list(A,B).
prim(my_sumlist0/2).
prim(my_max_list1/2).
prim(my_sumlist2/2).
prim(my_min_list3/2).
prim(my_succ4/2).
prim(my_pred5/2).
prim(my_pred6/2).
prim(my_last7/2).
prim(my_last8/2).
prim(my_min_list9/2).
prim(my_succ10/2).
prim(my_head11/2).
prim(my_min_list12/2).
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
p([['f','g','v'],['b','b','d','r'],['y','a','p','u'],['u','d','x','v']],[['f','g'],['b','b','d'],['y','a','p'],['u','d','x']]).
p([['o','m','c','u'],['t','j','k','x'],['w','c','k']],[['o','m','c'],['t','j','k'],['w','c']]).
