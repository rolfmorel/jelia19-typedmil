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
my_list_to_set3(A,B):-list_to_set(A,B).
my_msort4(A,B):-msort(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
my_toupper6(A,B):-upcase_atom(A,B).
my_flatten7(A,B):-flatten(A,B).
my_tolower8(A,B):-downcase_atom(A,B).
my_even9(A):-0 is A mod 2.
my_odd10(A):-1 is A mod 2.
my_uppercase11(A):-upcase_atom(A,A).
my_sumlist12(A,B):-sumlist(A,B).
my_len13(A,B):-length(A,B).
my_last14(A,B):-last(A,B).
my_max_list15(A,B):-max_list(A,B).
my_succ16(A,B):-succ(A,B),B =< 10.
my_set17(A):-list_to_set(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_list_to_set3/2).
prim(my_msort4/2).
prim(my_pred5/2).
prim(my_toupper6/2).
prim(my_flatten7/2).
prim(my_tolower8/2).
prim(my_even9/1).
prim(my_odd10/1).
prim(my_uppercase11/1).
prim(my_sumlist12/2).
prim(my_len13/2).
prim(my_last14/2).
prim(my_max_list15/2).
prim(my_succ16/2).
prim(my_set17/1).
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
p(['B','h','r','w','w','Z','Q','B','U'],'w').
p(['x','N','u','s','q','r','d','g','s','u'],'s').
p(['K','m','K','y','z','n','h'],'K').
p(['h','k','N','B','L','P','r','a','C','P'],'P').
p(['q','o','H','Z','m','m','M'],'m').
q(['A','u','L','A','Q','D','K','C','K','T','X'],'T').
q(['k','r','x','W','X','b','r'],'k').
q(['b','G','b','V','k','I','W','k','Q'],'V').
q(['B','b','V','d','B','p','X','j'],'V').
q(['H','E','Z','O','Z','R','c'],'E').
