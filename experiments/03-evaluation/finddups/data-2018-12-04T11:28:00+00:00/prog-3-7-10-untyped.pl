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
my_even3(A):-0 is A mod 2.
my_double4(N,M):-M is 2*N,M =< 10.
my_succ5(A,B):-succ(A,B),B =< 10.
my_toupper6(A,B):-upcase_atom(A,B).
my_set7(A):-list_to_set(A,A).
my_len8(A,B):-length(A,B).
my_sumlist9(A,B):-sumlist(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_even3/1).
prim(my_double4/2).
prim(my_succ5/2).
prim(my_toupper6/2).
prim(my_set7/1).
prim(my_len8/2).
prim(my_sumlist9/2).
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
p(['e','J','d','V','e','C','L','I'],'e').
p(['q','j','a','J','N','E','u','b','P','b'],'b').
p(['p','n','G','p','T','G','J','H','w'],'G').
p(['L','o','X','Y','n','L'],'L').
p(['G','F','p','y','C','D','a','b','G','w'],'G').
q(['T','M','T','V','r','W'],'M').
q(['Z','b','S','b','n','U','y','e'],'Z').
q(['L','O','K','I','I','j','K','k'],'k').
q(['u','k','a','s','k','z'],'u').
q(['F','S','U','m','m','Y'],'Y').
