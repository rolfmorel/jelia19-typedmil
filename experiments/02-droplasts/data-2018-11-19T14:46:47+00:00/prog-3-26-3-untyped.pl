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
my_last1(A,B):-last(A,B).
my_max_list2(A,B):-max_list(A,B).
my_last3(A,B):-last(A,B).
my_pred4(A,B):-succ(B,A).
my_head5([H|_],H).
my_tail6([_|TL],TL).
my_len7(A,B):-length(A,B).
my_reverse8(A,B):-reverse(A,B).
my_last9(A,B):-last(A,B).
my_min_list10(A,B):-min_list(A,B).
my_reverse11(A,B):-reverse(A,B).
my_head12([H|_],H).
my_sumlist13(A,B):-sumlist(A,B).
my_reverse14(A,B):-reverse(A,B).
my_reverse15(A,B):-reverse(A,B).
my_succ16(A,B):-succ(A,B).
my_pred17(A,B):-succ(B,A).
my_max_list18(A,B):-max_list(A,B).
my_min_list19(A,B):-min_list(A,B).
my_pred20(A,B):-succ(B,A).
my_head21([H|_],H).
my_succ22(A,B):-succ(A,B).
my_head23([H|_],H).
my_reverse24(A,B):-reverse(A,B).
my_min_list25(A,B):-min_list(A,B).
prim(my_head0/2).
prim(my_last1/2).
prim(my_max_list2/2).
prim(my_last3/2).
prim(my_pred4/2).
prim(my_head5/2).
prim(my_tail6/2).
prim(my_len7/2).
prim(my_reverse8/2).
prim(my_last9/2).
prim(my_min_list10/2).
prim(my_reverse11/2).
prim(my_head12/2).
prim(my_sumlist13/2).
prim(my_reverse14/2).
prim(my_reverse15/2).
prim(my_succ16/2).
prim(my_pred17/2).
prim(my_max_list18/2).
prim(my_min_list19/2).
prim(my_pred20/2).
prim(my_head21/2).
prim(my_succ22/2).
prim(my_head23/2).
prim(my_reverse24/2).
prim(my_min_list25/2).
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
p([['l','n','y'],['f','u','k','g'],['c','e','g','g']],[['l','n'],['f','u','k'],['c','e','g']]).
p([['w','m','c','y'],['e','b','b','s'],['j','q','j']],[['w','m','c'],['e','b','b'],['j','q']]).
