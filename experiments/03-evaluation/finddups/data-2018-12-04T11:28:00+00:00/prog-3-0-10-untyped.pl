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
p(['Z','E','k','Y','Y','x','n'],'Y').
p(['I','O','U','l','U','f'],'U').
p(['n','G','v','c','r','A','R','f','G','G'],'G').
p(['B','Z','Z','E','s','v','M'],'Z').
p(['O','G','X','t','R','t','O','i','z','W'],'O').
q(['q','w','O','a','w','b','K','G','y','h','s'],'K').
q(['l','i','P','I','u','f','y','M','y'],'I').
q(['l','Q','O','P','N','Q'],'l').
q(['T','M','R','I','E','c','P','I'],'c').
q(['k','G','J','s','H','G'],'s').
