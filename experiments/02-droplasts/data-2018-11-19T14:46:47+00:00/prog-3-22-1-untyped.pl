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
my_head0([H|_],H).
my_max_list1(A,B):-max_list(A,B).
my_reverse2(A,B):-reverse(A,B).
my_max_list3(A,B):-max_list(A,B).
my_head4([H|_],H).
my_len5(A,B):-length(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_tail7([_|TL],TL).
my_max_list8(A,B):-max_list(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_pred10(A,B):-succ(B,A).
my_reverse11(A,B):-reverse(A,B).
my_max_list12(A,B):-max_list(A,B).
my_pred13(A,B):-succ(B,A).
my_pred14(A,B):-succ(B,A).
my_succ15(A,B):-succ(A,B).
my_pred16(A,B):-succ(B,A).
my_tail17([_|TL],TL).
my_last18(A,B):-last(A,B).
my_last19(A,B):-last(A,B).
my_min_list20(A,B):-min_list(A,B).
my_succ21(A,B):-succ(A,B).
prim(my_head0/2).
prim(my_max_list1/2).
prim(my_reverse2/2).
prim(my_max_list3/2).
prim(my_head4/2).
prim(my_len5/2).
prim(my_sumlist6/2).
prim(my_tail7/2).
prim(my_max_list8/2).
prim(my_sumlist9/2).
prim(my_pred10/2).
prim(my_reverse11/2).
prim(my_max_list12/2).
prim(my_pred13/2).
prim(my_pred14/2).
prim(my_succ15/2).
prim(my_pred16/2).
prim(my_tail17/2).
prim(my_last18/2).
prim(my_last19/2).
prim(my_min_list20/2).
prim(my_succ21/2).
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
p([['p','l','f'],['r','d','y'],['r','t','b','l']],[['p','l'],['r','d'],['r','t','b']]).
p([['o','h','e'],['m','e','s','r'],['i','x','n'],['b','b','h']],[['o','h'],['m','e','s'],['i','x'],['b','b']]).
