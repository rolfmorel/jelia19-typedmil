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
my_sumlist1(A,B):-sumlist(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_len4(A,B):-length(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_pred6(A,B):-succ(B,A).
my_min_list7(A,B):-min_list(A,B).
my_reverse8(A,B):-reverse(A,B).
my_succ9(A,B):-succ(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_head11([H|_],H).
my_succ12(A,B):-succ(A,B).
my_succ13(A,B):-succ(A,B).
my_reverse14(A,B):-reverse(A,B).
my_head15([H|_],H).
my_sumlist16(A,B):-sumlist(A,B).
my_head17([H|_],H).
my_last18(A,B):-last(A,B).
my_last19(A,B):-last(A,B).
my_pred20(A,B):-succ(B,A).
my_succ21(A,B):-succ(A,B).
my_last22(A,B):-last(A,B).
my_sumlist23(A,B):-sumlist(A,B).
my_tail24([_|TL],TL).
my_tail25([_|TL],TL).
my_succ26(A,B):-succ(A,B).
my_succ27(A,B):-succ(A,B).
my_max_list28(A,B):-max_list(A,B).
my_reverse29(A,B):-reverse(A,B).
prim(my_sumlist0/2).
prim(my_sumlist1/2).
prim(my_sumlist2/2).
prim(my_sumlist3/2).
prim(my_len4/2).
prim(my_sumlist5/2).
prim(my_pred6/2).
prim(my_min_list7/2).
prim(my_reverse8/2).
prim(my_succ9/2).
prim(my_sumlist10/2).
prim(my_head11/2).
prim(my_succ12/2).
prim(my_succ13/2).
prim(my_reverse14/2).
prim(my_head15/2).
prim(my_sumlist16/2).
prim(my_head17/2).
prim(my_last18/2).
prim(my_last19/2).
prim(my_pred20/2).
prim(my_succ21/2).
prim(my_last22/2).
prim(my_sumlist23/2).
prim(my_tail24/2).
prim(my_tail25/2).
prim(my_succ26/2).
prim(my_succ27/2).
prim(my_max_list28/2).
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
p([['g','o','u','h'],['x','b','x','s'],['t','k','g','v']],[['g','o','u'],['x','b','x'],['t','k','g']]).
p([['p','s','g','g'],['k','l','m','j'],['t','d','k'],['i','a','j','p']],[['p','s','g'],['k','l','m'],['t','d'],['i','a','j']]).
