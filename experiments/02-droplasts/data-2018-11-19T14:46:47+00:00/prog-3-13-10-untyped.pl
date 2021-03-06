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
my_tail1([_|TL],TL).
my_pred2(A,B):-succ(B,A).
my_succ3(A,B):-succ(A,B).
my_tail4([_|TL],TL).
my_sumlist5(A,B):-sumlist(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_pred7(A,B):-succ(B,A).
my_last8(A,B):-last(A,B).
my_last9(A,B):-last(A,B).
my_tail10([_|TL],TL).
my_last11(A,B):-last(A,B).
my_succ12(A,B):-succ(A,B).
prim(my_max_list0/2).
prim(my_tail1/2).
prim(my_pred2/2).
prim(my_succ3/2).
prim(my_tail4/2).
prim(my_sumlist5/2).
prim(my_sumlist6/2).
prim(my_pred7/2).
prim(my_last8/2).
prim(my_last9/2).
prim(my_tail10/2).
prim(my_last11/2).
prim(my_succ12/2).
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
p([['g','u','c','r'],['c','f','m','a']],[['g','u','c'],['c','f','m']]).
p([['t','o','u'],['l','s','q','w'],['o','e','n']],[['t','o'],['l','s','q'],['o','e']]).
