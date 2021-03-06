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
my_double3(N,M):-M is 2*N,M =< 10.
my_tolower4(A,B):-downcase_atom(A,B).
my_even5(A):-0 is A mod 2.
my_len6(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_double3/2).
prim(my_tolower4/2).
prim(my_even5/1).
prim(my_len6/2).
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
p(['s','U','R','p','X','R','y'],'R').
p(['o','J','t','N','o','r','n','v'],'o').
p(['A','W','L','a','P','t','p','z','x','A'],'A').
p(['c','v','J','k','C','v','E','V','V','N'],'V').
p(['X','H','w','K','H','W','D','v'],'H').
q(['L','s','V','W','V','C','m','N','C','v','F'],'F').
q(['B','v','A','B','A','N','d','U','i'],'v').
q(['D','D','V','d','G','Y','M','g','w'],'w').
q(['N','b','f','r','c','h','X','f','x'],'r').
q(['L','b','I','J','h','L'],'b').
