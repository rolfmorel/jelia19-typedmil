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
my_max_list1(A,B):-max_list(A,B).
my_min_list2(A,B):-min_list(A,B).
my_reverse3(A,B):-reverse(A,B).
my_reverse4(A,B):-reverse(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_min_list6(A,B):-min_list(A,B).
my_pred7(A,B):-succ(B,A).
my_pred8(A,B):-succ(B,A).
my_pred9(A,B):-succ(B,A).
my_sumlist10(A,B):-sumlist(A,B).
my_max_list11(A,B):-max_list(A,B).
my_tail12([_|TL],TL).
my_tail13([_|TL],TL).
my_pred14(A,B):-succ(B,A).
prim(my_min_list0/2).
prim(my_max_list1/2).
prim(my_min_list2/2).
prim(my_reverse3/2).
prim(my_reverse4/2).
prim(my_sumlist5/2).
prim(my_min_list6/2).
prim(my_pred7/2).
prim(my_pred8/2).
prim(my_pred9/2).
prim(my_sumlist10/2).
prim(my_max_list11/2).
prim(my_tail12/2).
prim(my_tail13/2).
prim(my_pred14/2).
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
p([['n','m','a','y'],['h','m','h'],['y','m','y','q']],[['n','m','a'],['h','m'],['y','m','y']]).
p([['x','m','d'],['w','r','d','x']],[['x','m'],['w','r','d']]).
