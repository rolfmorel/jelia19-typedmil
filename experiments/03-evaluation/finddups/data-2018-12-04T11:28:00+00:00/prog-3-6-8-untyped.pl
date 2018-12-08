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
my_len4(A,B):-length(A,B).
my_msort5(A,B):-msort(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_odd8(A):-1 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_even3/1).
prim(my_len4/2).
prim(my_msort5/2).
prim(my_sumlist6/2).
prim(my_succ7/2).
prim(my_odd8/1).
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
p(['o','N','G','N','c','N'],'N').
p(['K','a','K','P','p','s','L','p'],'p').
p(['Z','M','M','l','O','Q','V'],'M').
p(['F','w','q','p','q'],'q').
p(['L','D','Q','Z','Q','O'],'Q').
q(['U','Z','c','G','Z','k','D','K','K','a','k'],'U').
q(['B','o','o','z','Y','m','c','Z','l'],'Y').
q(['N','r','L','N','P','L'],'r').
q(['X','O','X','N','Y','W'],'W').
q(['K','y','Y','W','X','q','s','c','M','W','w'],'s').
