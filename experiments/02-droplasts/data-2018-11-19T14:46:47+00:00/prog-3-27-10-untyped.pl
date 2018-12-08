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
my_last1(A,B):-last(A,B).
my_reverse2(A,B):-reverse(A,B).
my_last3(A,B):-last(A,B).
my_min_list4(A,B):-min_list(A,B).
my_max_list5(A,B):-max_list(A,B).
my_min_list6(A,B):-min_list(A,B).
my_last7(A,B):-last(A,B).
my_pred8(A,B):-succ(B,A).
my_last9(A,B):-last(A,B).
my_last10(A,B):-last(A,B).
my_len11(A,B):-length(A,B).
my_min_list12(A,B):-min_list(A,B).
my_len13(A,B):-length(A,B).
my_head14([H|_],H).
my_len15(A,B):-length(A,B).
my_min_list16(A,B):-min_list(A,B).
my_head17([H|_],H).
my_max_list18(A,B):-max_list(A,B).
my_reverse19(A,B):-reverse(A,B).
my_max_list20(A,B):-max_list(A,B).
my_pred21(A,B):-succ(B,A).
my_succ22(A,B):-succ(A,B).
my_pred23(A,B):-succ(B,A).
my_min_list24(A,B):-min_list(A,B).
my_succ25(A,B):-succ(A,B).
my_succ26(A,B):-succ(A,B).
prim(my_pred0/2).
prim(my_last1/2).
prim(my_reverse2/2).
prim(my_last3/2).
prim(my_min_list4/2).
prim(my_max_list5/2).
prim(my_min_list6/2).
prim(my_last7/2).
prim(my_pred8/2).
prim(my_last9/2).
prim(my_last10/2).
prim(my_len11/2).
prim(my_min_list12/2).
prim(my_len13/2).
prim(my_head14/2).
prim(my_len15/2).
prim(my_min_list16/2).
prim(my_head17/2).
prim(my_max_list18/2).
prim(my_reverse19/2).
prim(my_max_list20/2).
prim(my_pred21/2).
prim(my_succ22/2).
prim(my_pred23/2).
prim(my_min_list24/2).
prim(my_succ25/2).
prim(my_succ26/2).
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
p([['i','s','i','h'],['i','o','f','e'],['d','n','b','c']],[['i','s','i'],['i','o','f'],['d','n','b']]).
p([['j','m','o','l'],['y','l','w'],['y','t','y'],['m','k','c','b']],[['j','m','o'],['y','l'],['y','t'],['m','k','c']]).
