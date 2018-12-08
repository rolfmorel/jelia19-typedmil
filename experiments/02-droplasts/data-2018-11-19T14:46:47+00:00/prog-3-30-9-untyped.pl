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
my_min_list0(A,B):-min_list(A,B).
my_sumlist1(A,B):-sumlist(A,B).
my_min_list2(A,B):-min_list(A,B).
my_min_list3(A,B):-min_list(A,B).
my_len4(A,B):-length(A,B).
my_max_list5(A,B):-max_list(A,B).
my_head6([H|_],H).
my_pred7(A,B):-succ(B,A).
my_head8([H|_],H).
my_head9([H|_],H).
my_tail10([_|TL],TL).
my_len11(A,B):-length(A,B).
my_max_list12(A,B):-max_list(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_min_list14(A,B):-min_list(A,B).
my_succ15(A,B):-succ(A,B).
my_reverse16(A,B):-reverse(A,B).
my_max_list17(A,B):-max_list(A,B).
my_succ18(A,B):-succ(A,B).
my_last19(A,B):-last(A,B).
my_reverse20(A,B):-reverse(A,B).
my_min_list21(A,B):-min_list(A,B).
my_max_list22(A,B):-max_list(A,B).
my_len23(A,B):-length(A,B).
my_min_list24(A,B):-min_list(A,B).
my_tail25([_|TL],TL).
my_succ26(A,B):-succ(A,B).
my_max_list27(A,B):-max_list(A,B).
my_max_list28(A,B):-max_list(A,B).
my_len29(A,B):-length(A,B).
prim(my_min_list0/2).
prim(my_sumlist1/2).
prim(my_min_list2/2).
prim(my_min_list3/2).
prim(my_len4/2).
prim(my_max_list5/2).
prim(my_head6/2).
prim(my_pred7/2).
prim(my_head8/2).
prim(my_head9/2).
prim(my_tail10/2).
prim(my_len11/2).
prim(my_max_list12/2).
prim(my_sumlist13/2).
prim(my_min_list14/2).
prim(my_succ15/2).
prim(my_reverse16/2).
prim(my_max_list17/2).
prim(my_succ18/2).
prim(my_last19/2).
prim(my_reverse20/2).
prim(my_min_list21/2).
prim(my_max_list22/2).
prim(my_len23/2).
prim(my_min_list24/2).
prim(my_tail25/2).
prim(my_succ26/2).
prim(my_max_list27/2).
prim(my_max_list28/2).
prim(my_len29/2).
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
p([['b','s','t','l'],['y','y','m','n'],['w','o','a','k'],['v','b','y','l']],[['b','s','t'],['y','y','m'],['w','o','a'],['v','b','y']]).
p([['c','q','u'],['w','n','g'],['e','w','e']],[['c','q'],['w','n'],['e','w']]).
