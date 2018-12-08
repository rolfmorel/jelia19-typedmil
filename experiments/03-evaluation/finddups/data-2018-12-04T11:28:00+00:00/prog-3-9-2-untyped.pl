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
my_uppercase3(A):-upcase_atom(A,A).
my_flatten4(A,B):-flatten(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_min_list6(A,B):-min_list(A,B).
my_set7(A):-list_to_set(A,A).
my_toupper8(A,B):-upcase_atom(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_sumlist10(A,B):-sumlist(A,B).
my_len11(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_uppercase3/1).
prim(my_flatten4/2).
prim(my_succ5/2).
prim(my_min_list6/2).
prim(my_set7/1).
prim(my_toupper8/2).
prim(my_double9/2).
prim(my_sumlist10/2).
prim(my_len11/2).
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
p(['P','u','P','i','x'],'P').
p(['A','u','D','A','M','T','x','C'],'A').
p(['z','F','V','Y','h','i','C','C','c','m'],'C').
p(['Y','B','P','M','T','B'],'B').
p(['d','x','l','s','A','Q','k','L','k'],'k').
q(['G','u','u','C','u','j','t','Z'],'t').
q(['x','e','x','[','t','S','Q','h','n'],'[').
q(['F','y','b','y','F','Z'],'Z').
q(['X','u','G','P','u','V','x','m','Q','b','n'],'n').
q(['x','c','I','f','r','M','M'],'c').
