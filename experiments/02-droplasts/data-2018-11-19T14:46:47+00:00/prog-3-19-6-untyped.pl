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
my_max_list0(A,B):-max_list(A,B).
my_max_list1(A,B):-max_list(A,B).
my_min_list2(A,B):-min_list(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_succ4(A,B):-succ(A,B).
my_max_list5(A,B):-max_list(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_reverse7(A,B):-reverse(A,B).
my_min_list8(A,B):-min_list(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_pred11(A,B):-succ(B,A).
my_succ12(A,B):-succ(A,B).
my_len13(A,B):-length(A,B).
my_max_list14(A,B):-max_list(A,B).
my_tail15([_|TL],TL).
my_tail16([_|TL],TL).
my_sumlist17(A,B):-sumlist(A,B).
my_last18(A,B):-last(A,B).
prim(my_max_list0/2).
prim(my_max_list1/2).
prim(my_min_list2/2).
prim(my_sumlist3/2).
prim(my_succ4/2).
prim(my_max_list5/2).
prim(my_sumlist6/2).
prim(my_reverse7/2).
prim(my_min_list8/2).
prim(my_sumlist9/2).
prim(my_sumlist10/2).
prim(my_pred11/2).
prim(my_succ12/2).
prim(my_len13/2).
prim(my_max_list14/2).
prim(my_tail15/2).
prim(my_tail16/2).
prim(my_sumlist17/2).
prim(my_last18/2).
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
p([['d','d','x','h'],['x','b','x','n']],[['d','d','x'],['x','b','x']]).
p([['h','g','u'],['i','a','v'],['p','j','f','a'],['x','p','u']],[['h','g'],['i','a'],['p','j','f'],['x','p']]).
