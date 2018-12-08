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
my_len3(A,B):-length(A,B).
my_last4(A,B):-last(A,B).
my_msort5(A,B):-msort(A,B).
my_max_list6(A,B):-max_list(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
my_list_to_set9(A,B):-list_to_set(A,B).
my_flatten10(A,B):-flatten(A,B).
my_odd11(A):-1 is A mod 2.
my_double12(N,M):-M is 2*N,M =< 10.
my_lowercase13(A):-downcase_atom(A,A).
my_set14(A):-list_to_set(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
prim(my_last4/2).
prim(my_msort5/2).
prim(my_max_list6/2).
prim(my_sumlist7/2).
prim(my_toupper8/2).
prim(my_list_to_set9/2).
prim(my_flatten10/2).
prim(my_odd11/1).
prim(my_double12/2).
prim(my_lowercase13/1).
prim(my_set14/1).
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
p(['x','u','O','x','H','x'],'x').
p(['h','d','X','I','D','F','y','A','F'],'F').
p(['o','e','o','E','m','n','i'],'o').
p(['C','B','B','U','a'],'B').
p(['I','x','o','D','z','F','o'],'o').
q(['c','U','P','j','F','i','R','T','i','l'],'j').
q(['E','P','C','j','V','E'],'P').
q(['n','a','t','x','B','B'],'x').
q(['{','M','l','K','Q','u','b','l','A','l'],'{').
q(['E','f','E','E','w','b','K','x'],'K').
