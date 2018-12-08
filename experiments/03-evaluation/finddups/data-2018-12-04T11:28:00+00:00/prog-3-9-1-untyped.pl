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
my_flatten3(A,B):-flatten(A,B).
my_even4(A):-0 is A mod 2.
my_min_list5(A,B):-min_list(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_toupper7(A,B):-upcase_atom(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_set9(A):-list_to_set(A,A).
my_list_to_set10(A,B):-list_to_set(A,B).
my_uppercase11(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_flatten3/2).
prim(my_even4/1).
prim(my_min_list5/2).
prim(my_double6/2).
prim(my_toupper7/2).
prim(my_succ8/2).
prim(my_set9/1).
prim(my_list_to_set10/2).
prim(my_uppercase11/1).
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
p(['u','h','D','K','v','u'],'u').
p(['x','N','x','E','N','R','x','e'],'x').
p(['g','l','T','P','c','l','w','U','r','C'],'l').
p(['r','r','p','C','P','L'],'r').
p(['C','J','p','y','J','k','N','L','M'],'J').
q(['c','W','m','r','E','r','B','I'],'I').
q(['y','H','F','g','P','H','t','g'],'P').
q(['j','r','T','c','c','C','C','r'],'T').
q(['b','X','D','X','M','D','o','D','Z'],'o').
q(['X','t','E','e','g','M','F','f','O','p','O'],'F').
