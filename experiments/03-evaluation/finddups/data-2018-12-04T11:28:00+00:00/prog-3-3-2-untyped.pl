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
my_set3(A):-list_to_set(A,A).
my_msort4(A,B):-msort(A,B).
my_even5(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_set3/1).
prim(my_msort4/2).
prim(my_even5/1).
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
p(['q','h','U','b','h','r','M','p','x','F'],'h').
p(['Q','m','h','S','Q','v'],'Q').
p(['e','U','e','l','M','U','W','G','B','g'],'e').
p(['k','b','J','G','G'],'G').
p(['f','N','k','H','u','K','K','g'],'K').
q(['F','Q','B','g','W','s','B','U','N'],'W').
q(['S','Q','Q','I','s','W'],'I').
q(['N','N','X','j','P','D'],'j').
q(['F','n','Y','t','S','t','F','B','Q','T','G'],'n').
q(['r','y','v','v','g','m','Y','a','F','K','H'],'y').
