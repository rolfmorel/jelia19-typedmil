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
my_pred4(A,B):-succ(B,A),A > 0.
my_flatten5(A,B):-flatten(A,B).
my_reverse6(A,B):-reverse(A,B).
my_tolower7(A,B):-downcase_atom(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_odd9(A):-1 is A mod 2.
my_max_list10(A,B):-max_list(A,B).
my_msort11(A,B):-msort(A,B).
my_uppercase12(A):-upcase_atom(A,A).
my_set13(A):-list_to_set(A,A).
my_toupper14(A,B):-upcase_atom(A,B).
my_len15(A,B):-length(A,B).
my_last16(A,B):-last(A,B).
my_min_list17(A,B):-min_list(A,B).
my_lowercase18(A):-downcase_atom(A,A).
my_even19(A):-0 is A mod 2.
my_sumlist20(A,B):-sumlist(A,B).
my_list_to_set21(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_double3/2).
prim(my_pred4/2).
prim(my_flatten5/2).
prim(my_reverse6/2).
prim(my_tolower7/2).
prim(my_succ8/2).
prim(my_odd9/1).
prim(my_max_list10/2).
prim(my_msort11/2).
prim(my_uppercase12/1).
prim(my_set13/1).
prim(my_toupper14/2).
prim(my_len15/2).
prim(my_last16/2).
prim(my_min_list17/2).
prim(my_lowercase18/1).
prim(my_even19/1).
prim(my_sumlist20/2).
prim(my_list_to_set21/2).
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
p(['p','m','j','B','B','t'],'B').
p(['w','g','m','f','V','Y','V','e'],'V').
p(['D','U','A','i','A','H'],'A').
p(['L','X','L','G','i','i','m','l','m'],'m').
p(['r','G','h','D','D','N','d','P'],'D').
q(['g','Q','f','g','p','c','g'],'Q').
q(['Z','V','r','C','E','o','o'],'E').
q(['g','Y','V','x','C','w','H','T','w','H','P'],'C').
q(['Q','c','E','I','d','H','Y','M','d'],'Y').
q(['f','b','L','b','M','P'],'L').
