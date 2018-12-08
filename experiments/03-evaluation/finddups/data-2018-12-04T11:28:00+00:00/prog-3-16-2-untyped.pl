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
my_lowercase3(A):-downcase_atom(A,A).
my_toupper4(A,B):-upcase_atom(A,B).
my_set5(A):-list_to_set(A,A).
my_max_list6(A,B):-max_list(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_even8(A):-0 is A mod 2.
my_tolower9(A,B):-downcase_atom(A,B).
my_min_list10(A,B):-min_list(A,B).
my_odd11(A):-1 is A mod 2.
my_reverse12(A,B):-reverse(A,B).
my_uppercase13(A):-upcase_atom(A,A).
my_last14(A,B):-last(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_succ16(A,B):-succ(A,B),B =< 10.
my_list_to_set17(A,B):-list_to_set(A,B).
my_double18(N,M):-M is 2*N,M =< 10.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_lowercase3/1).
prim(my_toupper4/2).
prim(my_set5/1).
prim(my_max_list6/2).
prim(my_pred7/2).
prim(my_even8/1).
prim(my_tolower9/2).
prim(my_min_list10/2).
prim(my_odd11/1).
prim(my_reverse12/2).
prim(my_uppercase13/1).
prim(my_last14/2).
prim(my_sumlist15/2).
prim(my_succ16/2).
prim(my_list_to_set17/2).
prim(my_double18/2).
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
p(['t','x','I','J','J'],'J').
p(['A','Y','q','q','U','A'],'q').
p(['K','J','S','X','D','n','N','t','N'],'N').
p(['k','v','m','v','x','X','P','D'],'v').
p(['H','r','R','Z','j','f','Z'],'Z').
q(['g','G','m','m','H','o'],'H').
q(['V','V','J','S','R','t','u','w','T','u'],'R').
q(['N','d','Q','p','d','n'],'Q').
q(['X','X','A','w','X','U','C'],'C').
q(['A','S','L','L','V','Q','d','M'],'V').
