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
my_last2(A,B):-last(A,B).
my_succ3(A,B):-succ(A,B).
my_head4([H|_],H).
my_min_list5(A,B):-min_list(A,B).
my_min_list6(A,B):-min_list(A,B).
my_head7([H|_],H).
my_tail8([_|TL],TL).
my_tail9([_|TL],TL).
my_pred10(A,B):-succ(B,A).
my_max_list11(A,B):-max_list(A,B).
my_min_list12(A,B):-min_list(A,B).
prim(my_sumlist0/2).
prim(my_head1/2).
prim(my_last2/2).
prim(my_succ3/2).
prim(my_head4/2).
prim(my_min_list5/2).
prim(my_min_list6/2).
prim(my_head7/2).
prim(my_tail8/2).
prim(my_tail9/2).
prim(my_pred10/2).
prim(my_max_list11/2).
prim(my_min_list12/2).
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
p([['n','r','x','l'],['s','g','c'],['r','x','o']],[['n','r','x'],['s','g'],['r','x']]).
p([['e','d','c'],['a','k','a'],['l','d','l','a'],['t','w','h','x']],[['e','d'],['a','k'],['l','d','l'],['t','w','h']]).
