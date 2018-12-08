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
p(['l','N','l','O','j','L','u'],'l').
p(['C','P','d','C','l'],'C').
p(['A','f','n','Z','H','u','f','e','Z'],'Z').
p(['M','H','n','U','a','a','F'],'a').
p(['Z','P','X','D','U','U','c','h','c','V'],'U').
q(['D','G','U','g','B','w','u','u','F','G'],'D').
q(['p','A','v','g','D','j','g','N','l'],'j').
q(['s','Q','N','a','Q','f','g','s'],'f').
q(['v','r','z','n','r','y','U','z','m','S','R'],'m').
q(['R','T','q','T','z','J','v','X','j'],'J').
