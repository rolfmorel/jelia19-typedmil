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
my_min_list4(A,B):-min_list(A,B).
my_len5(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_set3/1).
prim(my_min_list4/2).
prim(my_len5/2).
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
p(['f','X','e','N','h','X','g'],'X').
p(['q','T','g','d','j','d','O','p','i'],'d').
p(['k','l','B','z','z','F','z'],'z').
p(['y','b','B','U','X','c','x','y','A','u'],'y').
p(['d','z','p','r','r','W','x','y','k'],'r').
q(['Z','h','B','B','C','i','B','J','F','z','u'],'F').
q(['B','d','B','N','k','v','z','D','B','k'],'N').
q(['s','i','V','z','u','k','Q','M','z','i'],'k').
q(['L','n','P','J','n','B','p'],'L').
q(['w','G','C','G','U','J'],'J').
