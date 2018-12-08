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
my_tail1([_|TL],TL).
my_max_list2(A,B):-max_list(A,B).
my_succ3(A,B):-succ(A,B).
my_len4(A,B):-length(A,B).
my_pred5(A,B):-succ(B,A).
my_min_list6(A,B):-min_list(A,B).
my_max_list7(A,B):-max_list(A,B).
my_min_list8(A,B):-min_list(A,B).
my_succ9(A,B):-succ(A,B).
my_min_list10(A,B):-min_list(A,B).
my_min_list11(A,B):-min_list(A,B).
my_reverse12(A,B):-reverse(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_pred14(A,B):-succ(B,A).
my_tail15([_|TL],TL).
my_min_list16(A,B):-min_list(A,B).
my_reverse17(A,B):-reverse(A,B).
my_max_list18(A,B):-max_list(A,B).
my_max_list19(A,B):-max_list(A,B).
my_len20(A,B):-length(A,B).
my_max_list21(A,B):-max_list(A,B).
my_min_list22(A,B):-min_list(A,B).
my_pred23(A,B):-succ(B,A).
my_last24(A,B):-last(A,B).
my_len25(A,B):-length(A,B).
my_len26(A,B):-length(A,B).
my_tail27([_|TL],TL).
my_tail28([_|TL],TL).
my_reverse29(A,B):-reverse(A,B).
prim(my_pred0/2).
prim(my_tail1/2).
prim(my_max_list2/2).
prim(my_succ3/2).
prim(my_len4/2).
prim(my_pred5/2).
prim(my_min_list6/2).
prim(my_max_list7/2).
prim(my_min_list8/2).
prim(my_succ9/2).
prim(my_min_list10/2).
prim(my_min_list11/2).
prim(my_reverse12/2).
prim(my_sumlist13/2).
prim(my_pred14/2).
prim(my_tail15/2).
prim(my_min_list16/2).
prim(my_reverse17/2).
prim(my_max_list18/2).
prim(my_max_list19/2).
prim(my_len20/2).
prim(my_max_list21/2).
prim(my_min_list22/2).
prim(my_pred23/2).
prim(my_last24/2).
prim(my_len25/2).
prim(my_len26/2).
prim(my_tail27/2).
prim(my_tail28/2).
prim(my_reverse29/2).
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
p([['m','v','w'],['h','o','t']],[['m','v'],['h','o']]).
p([['c','s','i','x'],['c','d','q','i'],['n','c','e']],[['c','s','i'],['c','d','q'],['n','c']]).
