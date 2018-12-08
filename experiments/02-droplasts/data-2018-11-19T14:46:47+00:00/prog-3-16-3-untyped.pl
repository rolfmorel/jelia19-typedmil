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
my_len0(A,B):-length(A,B).
my_tail1([_|TL],TL).
my_min_list2(A,B):-min_list(A,B).
my_len3(A,B):-length(A,B).
my_succ4(A,B):-succ(A,B).
my_pred5(A,B):-succ(B,A).
my_max_list6(A,B):-max_list(A,B).
my_tail7([_|TL],TL).
my_last8(A,B):-last(A,B).
my_succ9(A,B):-succ(A,B).
my_tail10([_|TL],TL).
my_len11(A,B):-length(A,B).
my_succ12(A,B):-succ(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_head14([H|_],H).
my_pred15(A,B):-succ(B,A).
prim(my_len0/2).
prim(my_tail1/2).
prim(my_min_list2/2).
prim(my_len3/2).
prim(my_succ4/2).
prim(my_pred5/2).
prim(my_max_list6/2).
prim(my_tail7/2).
prim(my_last8/2).
prim(my_succ9/2).
prim(my_tail10/2).
prim(my_len11/2).
prim(my_succ12/2).
prim(my_sumlist13/2).
prim(my_head14/2).
prim(my_pred15/2).
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
p([['f','c','d','q'],['i','k','r','l'],['d','w','g','d'],['y','e','h','f']],[['f','c','d'],['i','k','r'],['d','w','g'],['y','e','h']]).
p([['p','y','f','o'],['y','o','l'],['p','q','d']],[['p','y','f'],['y','o'],['p','q']]).
