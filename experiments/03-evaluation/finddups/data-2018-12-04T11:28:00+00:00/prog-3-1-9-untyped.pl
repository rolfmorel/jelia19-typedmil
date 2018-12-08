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
my_uppercase3(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_uppercase3/1).
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
p(['Q','m','M','v','j','f','J','Q'],'Q').
p(['j','Y','I','K','u','U','I','F'],'I').
p(['H','n','T','Z','P','L','l','s','s'],'s').
p(['d','l','Z','Y','d'],'d').
p(['M','E','q','A','A','z','i'],'A').
q(['m','P','L','L','B','j','z','k','D','k'],'j').
q(['d','r','U','U','T','d','x','w'],'w').
q(['E','M','f','Y','i','v','e','V','f','o','e'],'v').
q(['p','W','L','q','q','S'],'W').
q(['P','B','S','d','u','N','O','l','e','u','X'],'X').
