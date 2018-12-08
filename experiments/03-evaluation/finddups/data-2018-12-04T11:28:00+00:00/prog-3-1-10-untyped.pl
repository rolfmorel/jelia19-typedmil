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
my_len3(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
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
p(['U','E','R','s','a','i','e','U','L','O'],'U').
p(['i','A','A','S','C','r'],'A').
p(['L','d','Y','L','E'],'L').
p(['S','S','k','S','C','n','m','T'],'S').
p(['h','b','p','i','p'],'p').
q(['o','y','Z','b','g','O','q','f','Y','O','f'],'b').
q(['J','P','E','k','P','q'],'q').
q(['e','p','I','o','G','M','T','o','q','U','N'],'M').
q(['n','C','L','o','A','W','a','z','A'],'L').
q(['Z','Q','H','e','Z','M','c','M','v'],'v').
