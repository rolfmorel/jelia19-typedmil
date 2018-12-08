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
my_tail1([_|TL],TL).
my_min_list2(A,B):-min_list(A,B).
my_head3([H|_],H).
my_last4(A,B):-last(A,B).
my_succ5(A,B):-succ(A,B).
my_len6(A,B):-length(A,B).
my_min_list7(A,B):-min_list(A,B).
my_last8(A,B):-last(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_min_list10(A,B):-min_list(A,B).
my_len11(A,B):-length(A,B).
my_succ12(A,B):-succ(A,B).
my_last13(A,B):-last(A,B).
my_pred14(A,B):-succ(B,A).
my_reverse15(A,B):-reverse(A,B).
my_last16(A,B):-last(A,B).
my_succ17(A,B):-succ(A,B).
my_min_list18(A,B):-min_list(A,B).
my_max_list19(A,B):-max_list(A,B).
my_min_list20(A,B):-min_list(A,B).
my_last21(A,B):-last(A,B).
my_succ22(A,B):-succ(A,B).
my_len23(A,B):-length(A,B).
my_reverse24(A,B):-reverse(A,B).
my_tail25([_|TL],TL).
my_max_list26(A,B):-max_list(A,B).
my_succ27(A,B):-succ(A,B).
prim(my_tail0/2).
prim(my_tail1/2).
prim(my_min_list2/2).
prim(my_head3/2).
prim(my_last4/2).
prim(my_succ5/2).
prim(my_len6/2).
prim(my_min_list7/2).
prim(my_last8/2).
prim(my_sumlist9/2).
prim(my_min_list10/2).
prim(my_len11/2).
prim(my_succ12/2).
prim(my_last13/2).
prim(my_pred14/2).
prim(my_reverse15/2).
prim(my_last16/2).
prim(my_succ17/2).
prim(my_min_list18/2).
prim(my_max_list19/2).
prim(my_min_list20/2).
prim(my_last21/2).
prim(my_succ22/2).
prim(my_len23/2).
prim(my_reverse24/2).
prim(my_tail25/2).
prim(my_max_list26/2).
prim(my_succ27/2).
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
p([['o','i','i','v'],['k','g','u'],['r','n','u']],[['o','i','i'],['k','g'],['r','n']]).
p([['y','v','m','e'],['f','c','k','m'],['c','p','g']],[['y','v','m'],['f','c','k'],['c','p']]).
