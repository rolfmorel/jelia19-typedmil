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
my_pred4(A,B):-succ(B,A),A > 0.
my_max_list5(A,B):-max_list(A,B).
my_toupper6(A,B):-upcase_atom(A,B).
my_lowercase7(A):-downcase_atom(A,A).
my_uppercase8(A):-upcase_atom(A,A).
my_msort9(A,B):-msort(A,B).
my_list_to_set10(A,B):-list_to_set(A,B).
my_flatten11(A,B):-flatten(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_set13(A):-list_to_set(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
prim(my_pred4/2).
prim(my_max_list5/2).
prim(my_toupper6/2).
prim(my_lowercase7/1).
prim(my_uppercase8/1).
prim(my_msort9/2).
prim(my_list_to_set10/2).
prim(my_flatten11/2).
prim(my_succ12/2).
prim(my_set13/1).
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
p(['t','v','f','t','l','b'],'t').
p(['p','n','t','G','p','C','P','m'],'p').
p(['F','F','R','j','j'],'j').
p(['u','G','e','M','u','e','F','j','N','A'],'u').
p(['M','o','J','Y','X','o'],'o').
q(['f','b','l','P','S','R','a','M','F','M'],'a').
q(['h','l','K','I','l','O','B','g'],'K').
q(['Y','k','B','E','V','V'],'k').
q(['N','V','H','T','E','D','b','S','F','k','T'],'S').
q(['n','z','W','p','N','J','R','J','s','g'],'n').
