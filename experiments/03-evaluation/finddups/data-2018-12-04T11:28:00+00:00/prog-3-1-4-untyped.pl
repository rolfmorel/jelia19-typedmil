:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
%metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
my_tail0([_|TL],TL).
my_head1([H|_],H).
my_element2(A,B):-member(B,A).
my_set3(A):-list_to_set(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_set3/1).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p(['D','D','K','M','h','d','j','h','Y'],'h').
p(['k','q','z','R','W','q','b'],'q').
p(['X','M','X','z','J','O'],'X').
p(['a','F','f','u','F','F','d','d','P','h'],'d').
p(['U','M','A','s','s','O'],'s').
q(['s','D','q','a','D','P','L','g','T'],'P').
q(['Z','U','d','f','L','r','y','H','e','f'],'U').
q(['f','F','d','r','P','q','q','x'],'P').
q(['I','Z','{','H','h','Z'],'{').
q(['k','d','X','n','G','R','i','G'],'n').
