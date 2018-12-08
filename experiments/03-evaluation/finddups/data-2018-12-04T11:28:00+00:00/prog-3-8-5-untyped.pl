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
my_last4(A,B):-last(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_set6(A):-list_to_set(A,A).
my_list_to_set7(A,B):-list_to_set(A,B).
my_flatten8(A,B):-flatten(A,B).
my_max_list9(A,B):-max_list(A,B).
my_odd10(A):-1 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_min_list3/2).
prim(my_last4/2).
prim(my_succ5/2).
prim(my_set6/1).
prim(my_list_to_set7/2).
prim(my_flatten8/2).
prim(my_max_list9/2).
prim(my_odd10/1).
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
p(['f','R','D','g','f','W','R','D','g','k'],'f').
p(['i','t','M','J','C','v','u','C','D'],'C').
p(['X','a','a','t','D','E','t'],'a').
p(['n','I','n','O','M'],'n').
p(['n','U','z','b','b','O','U','r'],'b').
q(['A','s','u','A','i','V','P'],'s').
q(['w','H','Z','t','f','j','Z','p','C','z','b'],'j').
q(['X','D','E','e','L','u','R','u','A','x'],'D').
q(['I','Z','e','q','g','Y','e','t','S','g'],'Y').
q(['n','T','e','E','X','G','P','M','X'],'e').
