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
my_head0([H|_],H).
my_min_list1(A,B):-min_list(A,B).
my_len2(A,B):-length(A,B).
my_succ3(A,B):-succ(A,B).
my_pred4(A,B):-succ(B,A).
my_tail5([_|TL],TL).
my_len6(A,B):-length(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_max_list8(A,B):-max_list(A,B).
my_pred9(A,B):-succ(B,A).
my_sumlist10(A,B):-sumlist(A,B).
my_min_list11(A,B):-min_list(A,B).
my_max_list12(A,B):-max_list(A,B).
my_last13(A,B):-last(A,B).
prim(my_head0/2).
prim(my_min_list1/2).
prim(my_len2/2).
prim(my_succ3/2).
prim(my_pred4/2).
prim(my_tail5/2).
prim(my_len6/2).
prim(my_sumlist7/2).
prim(my_max_list8/2).
prim(my_pred9/2).
prim(my_sumlist10/2).
prim(my_min_list11/2).
prim(my_max_list12/2).
prim(my_last13/2).
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
p([['i','h','t','n'],['p','c','t','g'],['j','x','n'],['a','c','j']],[['i','h','t'],['p','c','t'],['j','x'],['a','c']]).
p([['h','b','s','l'],['n','f','e'],['p','t','w','b'],['h','x','m']],[['h','b','s'],['n','f'],['p','t','w'],['h','x']]).
