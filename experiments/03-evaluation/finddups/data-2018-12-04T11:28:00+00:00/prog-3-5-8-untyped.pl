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
my_tolower3(A,B):-downcase_atom(A,B).
my_double4(N,M):-M is 2*N,M =< 10.
my_list_to_set5(A,B):-list_to_set(A,B).
my_toupper6(A,B):-upcase_atom(A,B).
my_flatten7(A,B):-flatten(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_tolower3/2).
prim(my_double4/2).
prim(my_list_to_set5/2).
prim(my_toupper6/2).
prim(my_flatten7/2).
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
p(['n','S','A','j','j','V','R'],'j').
p(['W','j','m','A','W'],'W').
p(['o','d','u','s','u','e','j','H'],'u').
p(['q','B','m','f','m','q','j','z','i'],'m').
p(['f','j','A','x','y','E','W','f','L','i'],'f').
q(['e','M','N','i','h','g','V','e','c','z'],'M').
q(['S','f','H','S','z','e','k','w','R'],'w').
q(['l','R','N','u','v','c','g','R'],'v').
q(['d','o','A','d','C','o','b','q'],'A').
q(['W','I','G','w','F','S','I','Q'],'F').
