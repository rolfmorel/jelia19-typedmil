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
my_double3(N,M):-M is 2*N,M =< 10.
my_last4(A,B):-last(A,B).
my_toupper5(A,B):-upcase_atom(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_len7(A,B):-length(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
my_even9(A):-0 is A mod 2.
my_flatten10(A,B):-flatten(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_double3/2).
prim(my_last4/2).
prim(my_toupper5/2).
prim(my_succ6/2).
prim(my_len7/2).
prim(my_list_to_set8/2).
prim(my_even9/1).
prim(my_flatten10/2).
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
p(['r','s','d','H','F','Z','G','s','g','m'],'s').
p(['x','l','q','u','v','H','q','i','v','e'],'q').
p(['d','M','S','l','z','M','g'],'M').
p(['j','m','j','U','m','q','Z'],'j').
p(['V','M','k','k','V'],'k').
q(['A','C','p','L','Y','k','s','L','R'],'p').
q(['G','T','D','l','V','P','l','m','d'],'T').
q(['C','H','A','I','C','g'],'I').
q(['H','R','X','z','X','f','D','X','N','j','k'],'z').
q(['p','U','F','x','h','u','U'],'h').
