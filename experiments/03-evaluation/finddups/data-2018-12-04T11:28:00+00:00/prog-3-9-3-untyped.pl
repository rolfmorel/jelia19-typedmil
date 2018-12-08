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
my_msort3(A,B):-msort(A,B).
my_uppercase4(A):-upcase_atom(A,A).
my_min_list5(A,B):-min_list(A,B).
my_flatten6(A,B):-flatten(A,B).
my_tolower7(A,B):-downcase_atom(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_lowercase9(A):-downcase_atom(A,A).
my_pred10(A,B):-succ(B,A),A > 0.
my_toupper11(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_msort3/2).
prim(my_uppercase4/1).
prim(my_min_list5/2).
prim(my_flatten6/2).
prim(my_tolower7/2).
prim(my_double8/2).
prim(my_lowercase9/1).
prim(my_pred10/2).
prim(my_toupper11/2).
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
p(['L','t','D','L','k','k','a','D'],'L').
p(['Y','n','H','x','H'],'H').
p(['E','W','T','W','g','A','q'],'W').
p(['z','m','O','T','O','K'],'O').
p(['K','s','D','o','I','s','J','h','P','w'],'s').
q(['y','N','g','g','r','E'],'r').
q(['Q','a','w','b','I','x','I','F','E','b'],'Q').
q(['I','y','n','P','R','x','X','M','g','I','N'],'P').
q(['L','M','m','m','y','a','I','d','a','i','c'],'i').
q(['I','a','X','a','D','z','L','F'],'L').
