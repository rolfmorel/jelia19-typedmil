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
my_head1([H|_],H).
my_min_list2(A,B):-min_list(A,B).
my_max_list3(A,B):-max_list(A,B).
my_tail4([_|TL],TL).
my_succ5(A,B):-succ(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_succ7(A,B):-succ(A,B).
my_reverse8(A,B):-reverse(A,B).
my_succ9(A,B):-succ(A,B).
my_pred10(A,B):-succ(B,A).
my_succ11(A,B):-succ(A,B).
my_reverse12(A,B):-reverse(A,B).
my_len13(A,B):-length(A,B).
my_tail14([_|TL],TL).
my_tail15([_|TL],TL).
my_min_list16(A,B):-min_list(A,B).
my_len17(A,B):-length(A,B).
my_head18([H|_],H).
my_last19(A,B):-last(A,B).
my_reverse20(A,B):-reverse(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_head22([H|_],H).
my_pred23(A,B):-succ(B,A).
my_max_list24(A,B):-max_list(A,B).
my_reverse25(A,B):-reverse(A,B).
my_min_list26(A,B):-min_list(A,B).
prim(my_sumlist0/2).
prim(my_head1/2).
prim(my_min_list2/2).
prim(my_max_list3/2).
prim(my_tail4/2).
prim(my_succ5/2).
prim(my_sumlist6/2).
prim(my_succ7/2).
prim(my_reverse8/2).
prim(my_succ9/2).
prim(my_pred10/2).
prim(my_succ11/2).
prim(my_reverse12/2).
prim(my_len13/2).
prim(my_tail14/2).
prim(my_tail15/2).
prim(my_min_list16/2).
prim(my_len17/2).
prim(my_head18/2).
prim(my_last19/2).
prim(my_reverse20/2).
prim(my_sumlist21/2).
prim(my_head22/2).
prim(my_pred23/2).
prim(my_max_list24/2).
prim(my_reverse25/2).
prim(my_min_list26/2).
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
p([['p','n','a','m'],['r','j','u','p']],[['p','n','a'],['r','j','u']]).
p([['b','x','k'],['i','u','l'],['q','k','i','k']],[['b','x'],['i','u'],['q','k','i']]).
