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
my_len1(A,B):-length(A,B).
my_min_list2(A,B):-min_list(A,B).
my_last3(A,B):-last(A,B).
my_max_list4(A,B):-max_list(A,B).
my_pred5(A,B):-succ(B,A).
my_last6(A,B):-last(A,B).
my_head7([H|_],H).
my_succ8(A,B):-succ(A,B).
my_head9([H|_],H).
my_sumlist10(A,B):-sumlist(A,B).
my_min_list11(A,B):-min_list(A,B).
my_tail12([_|TL],TL).
my_last13(A,B):-last(A,B).
my_tail14([_|TL],TL).
my_last15(A,B):-last(A,B).
my_reverse16(A,B):-reverse(A,B).
my_pred17(A,B):-succ(B,A).
my_max_list18(A,B):-max_list(A,B).
my_pred19(A,B):-succ(B,A).
my_succ20(A,B):-succ(A,B).
my_pred21(A,B):-succ(B,A).
my_max_list22(A,B):-max_list(A,B).
my_succ23(A,B):-succ(A,B).
my_head24([H|_],H).
my_succ25(A,B):-succ(A,B).
prim(my_last0/2).
prim(my_len1/2).
prim(my_min_list2/2).
prim(my_last3/2).
prim(my_max_list4/2).
prim(my_pred5/2).
prim(my_last6/2).
prim(my_head7/2).
prim(my_succ8/2).
prim(my_head9/2).
prim(my_sumlist10/2).
prim(my_min_list11/2).
prim(my_tail12/2).
prim(my_last13/2).
prim(my_tail14/2).
prim(my_last15/2).
prim(my_reverse16/2).
prim(my_pred17/2).
prim(my_max_list18/2).
prim(my_pred19/2).
prim(my_succ20/2).
prim(my_pred21/2).
prim(my_max_list22/2).
prim(my_succ23/2).
prim(my_head24/2).
prim(my_succ25/2).
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
p([['o','w','m','i'],['c','y','n','w'],['g','b','l','k'],['n','b','x']],[['o','w','m'],['c','y','n'],['g','b','l'],['n','b']]).
p([['x','h','y','t'],['m','l','r','c']],[['x','h','y'],['m','l','r']]).
