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
my_pred0(A,B):-succ(B,A).
my_min_list1(A,B):-min_list(A,B).
my_len2(A,B):-length(A,B).
my_pred3(A,B):-succ(B,A).
my_succ4(A,B):-succ(A,B).
my_last5(A,B):-last(A,B).
my_max_list6(A,B):-max_list(A,B).
my_min_list7(A,B):-min_list(A,B).
my_pred8(A,B):-succ(B,A).
my_max_list9(A,B):-max_list(A,B).
my_succ10(A,B):-succ(A,B).
my_max_list11(A,B):-max_list(A,B).
my_tail12([_|TL],TL).
my_tail13([_|TL],TL).
my_head14([H|_],H).
my_last15(A,B):-last(A,B).
my_reverse16(A,B):-reverse(A,B).
my_head17([H|_],H).
my_succ18(A,B):-succ(A,B).
my_min_list19(A,B):-min_list(A,B).
my_pred20(A,B):-succ(B,A).
my_sumlist21(A,B):-sumlist(A,B).
prim(my_pred0/2).
prim(my_min_list1/2).
prim(my_len2/2).
prim(my_pred3/2).
prim(my_succ4/2).
prim(my_last5/2).
prim(my_max_list6/2).
prim(my_min_list7/2).
prim(my_pred8/2).
prim(my_max_list9/2).
prim(my_succ10/2).
prim(my_max_list11/2).
prim(my_tail12/2).
prim(my_tail13/2).
prim(my_head14/2).
prim(my_last15/2).
prim(my_reverse16/2).
prim(my_head17/2).
prim(my_succ18/2).
prim(my_min_list19/2).
prim(my_pred20/2).
prim(my_sumlist21/2).
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
p([['e','j','d','y'],['j','m','b'],['c','r','c','g'],['x','c','k']],[['e','j','d'],['j','m'],['c','r','c'],['x','c']]).
p([['c','h','k','d'],['x','u','k','h']],[['c','h','k'],['x','u','k']]).
