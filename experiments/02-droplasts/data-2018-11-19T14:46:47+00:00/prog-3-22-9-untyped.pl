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
my_last1(A,B):-last(A,B).
my_min_list2(A,B):-min_list(A,B).
my_tail3([_|TL],TL).
my_max_list4(A,B):-max_list(A,B).
my_succ5(A,B):-succ(A,B).
my_succ6(A,B):-succ(A,B).
my_max_list7(A,B):-max_list(A,B).
my_reverse8(A,B):-reverse(A,B).
my_pred9(A,B):-succ(B,A).
my_last10(A,B):-last(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_len12(A,B):-length(A,B).
my_min_list13(A,B):-min_list(A,B).
my_succ14(A,B):-succ(A,B).
my_last15(A,B):-last(A,B).
my_succ16(A,B):-succ(A,B).
my_pred17(A,B):-succ(B,A).
my_tail18([_|TL],TL).
my_len19(A,B):-length(A,B).
my_tail20([_|TL],TL).
my_head21([H|_],H).
prim(my_max_list0/2).
prim(my_last1/2).
prim(my_min_list2/2).
prim(my_tail3/2).
prim(my_max_list4/2).
prim(my_succ5/2).
prim(my_succ6/2).
prim(my_max_list7/2).
prim(my_reverse8/2).
prim(my_pred9/2).
prim(my_last10/2).
prim(my_sumlist11/2).
prim(my_len12/2).
prim(my_min_list13/2).
prim(my_succ14/2).
prim(my_last15/2).
prim(my_succ16/2).
prim(my_pred17/2).
prim(my_tail18/2).
prim(my_len19/2).
prim(my_tail20/2).
prim(my_head21/2).
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
p([['a','n','r'],['b','x','i'],['y','f','s'],['n','g','w','b']],[['a','n'],['b','x'],['y','f'],['n','g','w']]).
p([['w','a','n','r'],['p','a','d'],['i','e','n','p']],[['w','a','n'],['p','a'],['i','e','n']]).
