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
my_reverse1(A,B):-reverse(A,B).
my_succ2(A,B):-succ(A,B).
my_last3(A,B):-last(A,B).
my_pred4(A,B):-succ(B,A).
my_max_list5(A,B):-max_list(A,B).
my_pred6(A,B):-succ(B,A).
my_pred7(A,B):-succ(B,A).
my_len8(A,B):-length(A,B).
my_reverse9(A,B):-reverse(A,B).
my_max_list10(A,B):-max_list(A,B).
my_min_list11(A,B):-min_list(A,B).
my_reverse12(A,B):-reverse(A,B).
my_pred13(A,B):-succ(B,A).
my_pred14(A,B):-succ(B,A).
my_pred15(A,B):-succ(B,A).
my_min_list16(A,B):-min_list(A,B).
my_last17(A,B):-last(A,B).
my_len18(A,B):-length(A,B).
my_succ19(A,B):-succ(A,B).
my_max_list20(A,B):-max_list(A,B).
my_pred21(A,B):-succ(B,A).
my_succ22(A,B):-succ(A,B).
my_max_list23(A,B):-max_list(A,B).
my_head24([H|_],H).
my_succ25(A,B):-succ(A,B).
my_max_list26(A,B):-max_list(A,B).
my_reverse27(A,B):-reverse(A,B).
prim(my_min_list0/2).
prim(my_reverse1/2).
prim(my_succ2/2).
prim(my_last3/2).
prim(my_pred4/2).
prim(my_max_list5/2).
prim(my_pred6/2).
prim(my_pred7/2).
prim(my_len8/2).
prim(my_reverse9/2).
prim(my_max_list10/2).
prim(my_min_list11/2).
prim(my_reverse12/2).
prim(my_pred13/2).
prim(my_pred14/2).
prim(my_pred15/2).
prim(my_min_list16/2).
prim(my_last17/2).
prim(my_len18/2).
prim(my_succ19/2).
prim(my_max_list20/2).
prim(my_pred21/2).
prim(my_succ22/2).
prim(my_max_list23/2).
prim(my_head24/2).
prim(my_succ25/2).
prim(my_max_list26/2).
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
p([['c','y','n'],['x','w','a','e'],['e','r','l'],['i','b','s']],[['c','y'],['x','w','a'],['e','r'],['i','b']]).
p([['l','l','w','g'],['j','k','b','p'],['j','r','q','k'],['k','e','f']],[['l','l','w'],['j','k','b'],['j','r','q'],['k','e']]).
