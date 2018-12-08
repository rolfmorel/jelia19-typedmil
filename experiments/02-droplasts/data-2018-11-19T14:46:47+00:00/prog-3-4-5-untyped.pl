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
my_last0(A,B):-last(A,B).
my_head1([H|_],H).
my_succ2(A,B):-succ(A,B).
my_last3(A,B):-last(A,B).
prim(my_last0/2).
prim(my_head1/2).
prim(my_succ2/2).
prim(my_last3/2).
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
p([['h','y','j','p'],['l','h','l','r'],['d','c','b']],[['h','y','j'],['l','h','l'],['d','c']]).
p([['x','c','h'],['p','x','u'],['n','p','o'],['k','q','b']],[['x','c'],['p','x'],['n','p'],['k','q']]).
