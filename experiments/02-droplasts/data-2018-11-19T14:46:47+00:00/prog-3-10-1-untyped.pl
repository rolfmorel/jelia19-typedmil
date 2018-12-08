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
my_succ1(A,B):-succ(A,B).
my_last2(A,B):-last(A,B).
my_head3([H|_],H).
my_max_list4(A,B):-max_list(A,B).
my_head5([H|_],H).
my_reverse6(A,B):-reverse(A,B).
my_head7([H|_],H).
my_head8([H|_],H).
my_head9([H|_],H).
prim(my_pred0/2).
prim(my_succ1/2).
prim(my_last2/2).
prim(my_head3/2).
prim(my_max_list4/2).
prim(my_head5/2).
prim(my_reverse6/2).
prim(my_head7/2).
prim(my_head8/2).
prim(my_head9/2).
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
p([['g','n','i','j'],['q','m','q'],['e','o','l'],['i','r','b','w']],[['g','n','i'],['q','m'],['e','o'],['i','r','b']]).
p([['a','q','o','f'],['s','p','y'],['c','h','i','j'],['x','t','k']],[['a','q','o'],['s','p'],['c','h','i'],['x','t']]).
