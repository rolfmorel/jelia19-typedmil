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
my_max_list3(A,B):-max_list(A,B).
my_odd4(A):-1 is A mod 2.
my_len5(A,B):-length(A,B).
my_flatten6(A,B):-flatten(A,B).
my_set7(A):-list_to_set(A,A).
my_uppercase8(A):-upcase_atom(A,A).
my_msort9(A,B):-msort(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_max_list3/2).
prim(my_odd4/1).
prim(my_len5/2).
prim(my_flatten6/2).
prim(my_set7/1).
prim(my_uppercase8/1).
prim(my_msort9/2).
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
p(['l','K','I','O','Y','j','I','K','r'],'I').
p(['C','y','Y','i','i','W','V','W','H','Z'],'W').
p(['p','D','f','f','V','R','M','Z'],'f').
p(['C','c','C','F','b','P','b','B','H'],'C').
p(['P','u','D','D','c','Y','U','T','F'],'D').
q(['O','p','H','w','T','O','N','T','e','K','q'],'w').
q(['x','W','Z','J','Q','C','C','u','x','H'],'u').
q(['w','V','v','z','Z','V','h','b','H'],'H').
q(['k','f','q','Z','X','s','I','C','C'],'I').
q(['t','m','I','e','R','x','t'],'x').
