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
my_reverse0(A,B):-reverse(A,B).
my_succ1(A,B):-succ(A,B).
my_pred2(A,B):-succ(B,A).
my_reverse3(A,B):-reverse(A,B).
my_last4(A,B):-last(A,B).
my_pred5(A,B):-succ(B,A).
my_head6([H|_],H).
prim(my_reverse0/2).
prim(my_succ1/2).
prim(my_pred2/2).
prim(my_reverse3/2).
prim(my_last4/2).
prim(my_pred5/2).
prim(my_head6/2).
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
p([['m','k','u'],['l','w','h','g'],['k','v','h'],['t','m','p','h']],[['m','k'],['l','w','h'],['k','v'],['t','m','p']]).
p([['p','a','i','c'],['w','e','g'],['q','q','c']],[['p','a','i'],['w','e'],['q','q']]).
