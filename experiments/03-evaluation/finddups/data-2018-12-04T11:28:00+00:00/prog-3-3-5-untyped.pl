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
my_flatten3(A,B):-flatten(A,B).
my_succ4(A,B):-succ(A,B),B =< 10.
my_tolower5(A,B):-downcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_flatten3/2).
prim(my_succ4/2).
prim(my_tolower5/2).
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
p(['m','m','f','w','f','U'],'m').
p(['y','i','y','u','o','Z','t','D','o','j'],'y').
p(['x','U','D','i','i','Q'],'i').
p(['R','i','R','t','R','G','a','r','L'],'R').
p(['U','Z','Z','U','s'],'Z').
q(['k','k','I','u','x','d','s','Q'],'d').
q(['K','j','T','b','L','y','j'],'T').
q(['F','Y','c','w','D','B','p','F','F','e'],'p').
q(['X','g','B','L','g','L','L','K','f','m'],'B').
q(['A','q','a','G','A','Y'],'Y').
