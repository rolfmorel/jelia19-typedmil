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
my_min_list3(A,B):-min_list(A,B).
my_even4(A):-0 is A mod 2.
my_pred5(A,B):-succ(B,A),A > 0.
my_toupper6(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_min_list3/2).
prim(my_even4/1).
prim(my_pred5/2).
prim(my_toupper6/2).
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
p(['q','w','U','d','w','R'],'w').
p(['z','k','k','L','j','h','p','d'],'k').
p(['J','M','N','N','z'],'N').
p(['O','D','j','O','w','e'],'O').
p(['T','h','R','h','f','i','Q','x','q'],'h').
q(['X','o','h','d','A','y','f','h','I','q'],'d').
q(['f','s','f','d','K','m','u','G','m'],'d').
q(['M','k','s','m','F','M','n','b','X','T','S'],'s').
q(['b','Z','b','d','m','Z','J','G','x'],'G').
q(['W','k','f','X','o','F','F','C','N','v'],'X').
