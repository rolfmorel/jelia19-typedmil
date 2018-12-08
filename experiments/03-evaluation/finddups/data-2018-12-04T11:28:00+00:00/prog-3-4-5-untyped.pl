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
my_last4(A,B):-last(A,B).
my_toupper5(A,B):-upcase_atom(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_double3/2).
prim(my_last4/2).
prim(my_toupper5/2).
prim(my_pred6/2).
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
p(['P','g','H','r','H','N','B','N','K'],'H').
p(['e','S','P','d','e','u'],'e').
p(['z','p','U','b','O','I','p','h','h','C'],'p').
p(['I','i','r','a','S','V','R','r','h'],'r').
p(['m','v','Q','V','N','v','R'],'v').
q(['t','b','q','f','I','I','h','I','e','F','Y'],'Y').
q(['W','o','G','W','P','E','G'],'P').
q(['f','A','L','M','W','A','F','f','m'],'M').
q(['J','J','I','M','V','x'],'V').
q(['J','E','i','g','P','P'],'g').
