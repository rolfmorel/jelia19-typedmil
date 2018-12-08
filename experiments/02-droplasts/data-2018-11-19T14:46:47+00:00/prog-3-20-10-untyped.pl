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
my_tail0([_|TL],TL).
my_min_list1(A,B):-min_list(A,B).
my_max_list2(A,B):-max_list(A,B).
my_head3([H|_],H).
my_pred4(A,B):-succ(B,A).
my_succ5(A,B):-succ(A,B).
my_min_list6(A,B):-min_list(A,B).
my_head7([H|_],H).
my_sumlist8(A,B):-sumlist(A,B).
my_max_list9(A,B):-max_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_pred11(A,B):-succ(B,A).
my_pred12(A,B):-succ(B,A).
my_head13([H|_],H).
my_min_list14(A,B):-min_list(A,B).
my_succ15(A,B):-succ(A,B).
my_last16(A,B):-last(A,B).
my_len17(A,B):-length(A,B).
my_last18(A,B):-last(A,B).
my_head19([H|_],H).
prim(my_tail0/2).
prim(my_min_list1/2).
prim(my_max_list2/2).
prim(my_head3/2).
prim(my_pred4/2).
prim(my_succ5/2).
prim(my_min_list6/2).
prim(my_head7/2).
prim(my_sumlist8/2).
prim(my_max_list9/2).
prim(my_max_list10/2).
prim(my_pred11/2).
prim(my_pred12/2).
prim(my_head13/2).
prim(my_min_list14/2).
prim(my_succ15/2).
prim(my_last16/2).
prim(my_len17/2).
prim(my_last18/2).
prim(my_head19/2).
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
p([['k','l','n','x'],['o','b','d'],['o','j','i'],['d','b','x','a']],[['k','l','n'],['o','b'],['o','j'],['d','b','x']]).
p([['g','o','l'],['r','b','l','y']],[['g','o'],['r','b','l']]).
