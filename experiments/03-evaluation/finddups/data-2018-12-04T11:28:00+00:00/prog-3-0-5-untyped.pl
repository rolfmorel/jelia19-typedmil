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
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
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
p(['F','a','I','s','Z','p','C','o','s','C'],'s').
p(['F','O','F','V','b','L','B','p','F'],'F').
p(['O','N','v','v','k','z','Y'],'v').
p(['x','L','m','K','x','k'],'x').
p(['c','a','J','N','a','U'],'a').
q(['C','k','J','G','J','z','M','e'],'z').
q(['D','l','i','i','D','K','Q'],'l').
q(['D','u','k','Z','o','y','B','w','D'],'w').
q(['X','v','e','v','i','d','o','h','y','A'],'i').
q(['U','b','C','O','Z','f','F','C','O'],'Z').
