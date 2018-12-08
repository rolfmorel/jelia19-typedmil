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
my_tail1([_|TL],TL).
my_last2(A,B):-last(A,B).
my_tail3([_|TL],TL).
my_head4([H|_],H).
my_last5(A,B):-last(A,B).
my_pred6(A,B):-succ(B,A).
my_len7(A,B):-length(A,B).
my_succ8(A,B):-succ(A,B).
my_head9([H|_],H).
my_min_list10(A,B):-min_list(A,B).
my_last11(A,B):-last(A,B).
my_tail12([_|TL],TL).
my_max_list13(A,B):-max_list(A,B).
my_tail14([_|TL],TL).
my_pred15(A,B):-succ(B,A).
my_last16(A,B):-last(A,B).
my_last17(A,B):-last(A,B).
prim(my_head0/2).
prim(my_tail1/2).
prim(my_last2/2).
prim(my_tail3/2).
prim(my_head4/2).
prim(my_last5/2).
prim(my_pred6/2).
prim(my_len7/2).
prim(my_succ8/2).
prim(my_head9/2).
prim(my_min_list10/2).
prim(my_last11/2).
prim(my_tail12/2).
prim(my_max_list13/2).
prim(my_tail14/2).
prim(my_pred15/2).
prim(my_last16/2).
prim(my_last17/2).
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
p([['b','e','o','t'],['u','b','f','q'],['r','i','j','e'],['n','i','q','j']],[['b','e','o'],['u','b','f'],['r','i','j'],['n','i','q']]).
p([['m','d','a','k'],['y','h','e','s'],['y','d','b','m'],['h','o','n']],[['m','d','a'],['y','h','e'],['y','d','b'],['h','o']]).
