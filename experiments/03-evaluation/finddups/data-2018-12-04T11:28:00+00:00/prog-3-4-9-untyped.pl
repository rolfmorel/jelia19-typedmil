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
my_pred3(A,B):-succ(B,A),A > 0.
my_last4(A,B):-last(A,B).
my_msort5(A,B):-msort(A,B).
my_lowercase6(A):-downcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_pred3/2).
prim(my_last4/2).
prim(my_msort5/2).
prim(my_lowercase6/1).
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
p(['I','D','G','G','U','Z','y','I','V','b'],'G').
p(['O','d','s','S','M','Z','Z','W','a','m'],'Z').
p(['y','k','N','A','y','d','k','r'],'k').
p(['k','n','l','n','x'],'n').
p(['M','S','q','G','k','q'],'q').
q(['d','Y','B','f','S','d','d','c'],'B').
q(['o','q','O','q','P','Y'],'Y').
q(['Y','P','h','U','T','m','R','y','H','U'],'h').
q(['M','r','O','[','q','M','Q','n','d','M'],'[').
q(['x','I','e','B','y','o','J','q','x','n','d'],'n').
