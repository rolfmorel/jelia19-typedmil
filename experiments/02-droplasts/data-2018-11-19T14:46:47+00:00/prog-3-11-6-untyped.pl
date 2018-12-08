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
my_succ0(A,B):-succ(A,B).
my_min_list1(A,B):-min_list(A,B).
my_succ2(A,B):-succ(A,B).
my_pred3(A,B):-succ(B,A).
my_sumlist4(A,B):-sumlist(A,B).
my_pred5(A,B):-succ(B,A).
my_tail6([_|TL],TL).
my_max_list7(A,B):-max_list(A,B).
my_min_list8(A,B):-min_list(A,B).
my_tail9([_|TL],TL).
my_sumlist10(A,B):-sumlist(A,B).
prim(my_succ0/2).
prim(my_min_list1/2).
prim(my_succ2/2).
prim(my_pred3/2).
prim(my_sumlist4/2).
prim(my_pred5/2).
prim(my_tail6/2).
prim(my_max_list7/2).
prim(my_min_list8/2).
prim(my_tail9/2).
prim(my_sumlist10/2).
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
p([['k','g','o'],['o','c','j'],['y','l','a']],[['k','g'],['o','c'],['y','l']]).
p([['f','n','r'],['i','q','u','j'],['l','f','a','q'],['i','a','s']],[['f','n'],['i','q','u'],['l','f','a'],['i','a']]).
