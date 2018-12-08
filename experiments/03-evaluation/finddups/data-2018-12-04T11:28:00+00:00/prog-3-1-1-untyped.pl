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
my_odd3(A):-1 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_odd3/1).
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
p(['l','N','N','t','c'],'N').
p(['e','e','k','G','i','l'],'e').
p(['S','L','S','T','B','d','M'],'S').
p(['b','t','l','c','l'],'l').
p(['B','i','K','r','y','H','p','y','H','v'],'y').
q(['R','T','W','X','u','Y','i','g','C','z','u'],'R').
q(['v','F','c','m','p','d','d','G','k','I','V'],'I').
q(['u','M','P','y','l','B','f','Q','l','I','R'],'B').
q(['S','T','r','i','E','u','w','E','l','J','D'],'J').
q(['D','W','q','v','j','f','i','f'],'j').
