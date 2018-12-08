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
my_last4(A,B):-last(A,B).
my_max_list5(A,B):-max_list(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_len7(A,B):-length(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_tolower3/2).
prim(my_last4/2).
prim(my_max_list5/2).
prim(my_uppercase6/1).
prim(my_len7/2).
prim(my_list_to_set8/2).
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
p(['F','E','p','E','J','j','s','M'],'E').
p(['l','g','i','i','e'],'i').
p(['n','J','x','J','g','p','p'],'J').
p(['O','X','W','M','O','q','P','x','X','t'],'O').
p(['e','A','A','z','A'],'A').
q(['v','U','g','M','L','g'],'L').
q(['h','y','u','h','J','h'],'u').
q(['m','F','U','m','k','P'],'F').
q(['W','B','D','X','U','S','S','S','S','L'],'X').
q(['A','U','r','s','b','A','A','R'],'r').
