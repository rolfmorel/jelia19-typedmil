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
my_reverse0(A,B):-reverse(A,B).
my_len1(A,B):-length(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_reverse4(A,B):-reverse(A,B).
my_head5([H|_],H).
my_reverse6(A,B):-reverse(A,B).
my_last7(A,B):-last(A,B).
my_reverse8(A,B):-reverse(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_tail11([_|TL],TL).
my_sumlist12(A,B):-sumlist(A,B).
my_last13(A,B):-last(A,B).
my_reverse14(A,B):-reverse(A,B).
my_len15(A,B):-length(A,B).
my_pred16(A,B):-succ(B,A).
my_last17(A,B):-last(A,B).
my_pred18(A,B):-succ(B,A).
my_sumlist19(A,B):-sumlist(A,B).
my_reverse20(A,B):-reverse(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_max_list22(A,B):-max_list(A,B).
my_min_list23(A,B):-min_list(A,B).
prim(my_reverse0/2).
prim(my_len1/2).
prim(my_sumlist2/2).
prim(my_sumlist3/2).
prim(my_reverse4/2).
prim(my_head5/2).
prim(my_reverse6/2).
prim(my_last7/2).
prim(my_reverse8/2).
prim(my_sumlist9/2).
prim(my_sumlist10/2).
prim(my_tail11/2).
prim(my_sumlist12/2).
prim(my_last13/2).
prim(my_reverse14/2).
prim(my_len15/2).
prim(my_pred16/2).
prim(my_last17/2).
prim(my_pred18/2).
prim(my_sumlist19/2).
prim(my_reverse20/2).
prim(my_sumlist21/2).
prim(my_max_list22/2).
prim(my_min_list23/2).
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
p([['a','b','y','l'],['w','m','o'],['p','m','g'],['o','v','b']],[['a','b','y'],['w','m'],['p','m'],['o','v']]).
p([['g','m','m'],['e','b','s']],[['g','m'],['e','b']]).
