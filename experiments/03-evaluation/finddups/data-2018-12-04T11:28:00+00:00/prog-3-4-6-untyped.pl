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
my_flatten4(A,B):-flatten(A,B).
my_odd5(A):-1 is A mod 2.
my_list_to_set6(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_set3/1).
prim(my_flatten4/2).
prim(my_odd5/1).
prim(my_list_to_set6/2).
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
p(['Z','N','Y','R','R'],'R').
p(['q','D','q','d','j','K','b','N','V','z'],'q').
p(['j','N','B','E','E','V','U','C','c','k'],'E').
p(['A','W','k','H','b','s','J','A'],'A').
p(['t','h','m','v','m','s'],'m').
q(['A','g','M','s','f','Z','S','s'],'S').
q(['d','C','z','f','k','u','f','M'],'u').
q(['F','d','Q','h','G','h','G'],'F').
q(['a','A','r','d','r','t','T'],'t').
q(['H','a','W','P','p','O','a','S','G'],'W').
