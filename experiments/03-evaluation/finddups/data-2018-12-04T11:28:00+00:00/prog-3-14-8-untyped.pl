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
my_toupper3(A,B):-upcase_atom(A,B).
my_flatten4(A,B):-flatten(A,B).
my_msort5(A,B):-msort(A,B).
my_len6(A,B):-length(A,B).
my_max_list7(A,B):-max_list(A,B).
my_tolower8(A,B):-downcase_atom(A,B).
my_last9(A,B):-last(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_lowercase11(A):-downcase_atom(A,A).
my_succ12(A,B):-succ(A,B),B =< 10.
my_list_to_set13(A,B):-list_to_set(A,B).
my_double14(N,M):-M is 2*N,M =< 10.
my_even15(A):-0 is A mod 2.
my_sumlist16(A,B):-sumlist(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_toupper3/2).
prim(my_flatten4/2).
prim(my_msort5/2).
prim(my_len6/2).
prim(my_max_list7/2).
prim(my_tolower8/2).
prim(my_last9/2).
prim(my_pred10/2).
prim(my_lowercase11/1).
prim(my_succ12/2).
prim(my_list_to_set13/2).
prim(my_double14/2).
prim(my_even15/1).
prim(my_sumlist16/2).
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
p(['F','Y','p','M','q','J','M'],'M').
p(['v','c','I','c','b','G'],'c').
p(['X','X','B','S','m','T','v','X'],'X').
p(['a','e','e','q','Y','z','e','q','A','K'],'e').
p(['B','w','M','a','a','T','r','j','m'],'a').
q(['X','z','A','j','A','e','v','S'],'S').
q(['Y','w','W','H','K','m','m'],'w').
q(['S','V','w','r','S','x'],'r').
q(['k','e','a','s','a','t','n'],'t').
q(['D','F','x','G','i','e','b','s','u','e'],'s').
