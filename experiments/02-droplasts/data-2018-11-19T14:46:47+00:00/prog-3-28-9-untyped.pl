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
my_sumlist1(A,B):-sumlist(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_head3([H|_],H).
my_succ4(A,B):-succ(A,B).
my_succ5(A,B):-succ(A,B).
my_head6([H|_],H).
my_sumlist7(A,B):-sumlist(A,B).
my_len8(A,B):-length(A,B).
my_max_list9(A,B):-max_list(A,B).
my_head10([H|_],H).
my_sumlist11(A,B):-sumlist(A,B).
my_last12(A,B):-last(A,B).
my_reverse13(A,B):-reverse(A,B).
my_reverse14(A,B):-reverse(A,B).
my_pred15(A,B):-succ(B,A).
my_min_list16(A,B):-min_list(A,B).
my_pred17(A,B):-succ(B,A).
my_len18(A,B):-length(A,B).
my_pred19(A,B):-succ(B,A).
my_pred20(A,B):-succ(B,A).
my_sumlist21(A,B):-sumlist(A,B).
my_sumlist22(A,B):-sumlist(A,B).
my_min_list23(A,B):-min_list(A,B).
my_pred24(A,B):-succ(B,A).
my_head25([H|_],H).
my_reverse26(A,B):-reverse(A,B).
my_reverse27(A,B):-reverse(A,B).
prim(my_max_list0/2).
prim(my_sumlist1/2).
prim(my_sumlist2/2).
prim(my_head3/2).
prim(my_succ4/2).
prim(my_succ5/2).
prim(my_head6/2).
prim(my_sumlist7/2).
prim(my_len8/2).
prim(my_max_list9/2).
prim(my_head10/2).
prim(my_sumlist11/2).
prim(my_last12/2).
prim(my_reverse13/2).
prim(my_reverse14/2).
prim(my_pred15/2).
prim(my_min_list16/2).
prim(my_pred17/2).
prim(my_len18/2).
prim(my_pred19/2).
prim(my_pred20/2).
prim(my_sumlist21/2).
prim(my_sumlist22/2).
prim(my_min_list23/2).
prim(my_pred24/2).
prim(my_head25/2).
prim(my_reverse26/2).
prim(my_reverse27/2).
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
p([['i','d','b'],['j','o','g']],[['i','d'],['j','o']]).
p([['g','c','s','e'],['p','d','j','y'],['s','d','k','b']],[['g','c','s'],['p','d','j'],['s','d','k']]).
