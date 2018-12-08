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
my_last3(A,B):-last(A,B).
my_toupper4(A,B):-upcase_atom(A,B).
my_msort5(A,B):-msort(A,B).
my_set6(A):-list_to_set(A,A).
my_len7(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_last3/2).
prim(my_toupper4/2).
prim(my_msort5/2).
prim(my_set6/1).
prim(my_len7/2).
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
p(['Z','b','g','S','b','z','J'],'b').
p(['i','c','a','t','B','W','B'],'B').
p(['K','J','S','J','x','I','m'],'J').
p(['h','B','w','h','v','Z'],'h').
p(['H','U','u','U','b','P'],'U').
q(['X','X','V','a','[','y','y','x','R'],'[').
q(['d','e','u','h','f','r','d','f'],'e').
q(['y','M','K','U','M','U','y','m','Q','T','P'],'T').
q(['c','X','D','K','t','u','K','F'],'D').
q(['z','V','q','p','v','p','d'],'z').
