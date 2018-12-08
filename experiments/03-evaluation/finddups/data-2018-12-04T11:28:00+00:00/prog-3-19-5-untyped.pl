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
my_min_list3(A,B):-min_list(A,B).
my_max_list4(A,B):-max_list(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_flatten6(A,B):-flatten(A,B).
my_even7(A):-0 is A mod 2.
my_succ8(A,B):-succ(A,B),B =< 10.
my_last9(A,B):-last(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_msort11(A,B):-msort(A,B).
my_len12(A,B):-length(A,B).
my_odd13(A):-1 is A mod 2.
my_toupper14(A,B):-upcase_atom(A,B).
my_uppercase15(A):-upcase_atom(A,A).
my_tolower16(A,B):-downcase_atom(A,B).
my_pred17(A,B):-succ(B,A),A > 0.
my_list_to_set18(A,B):-list_to_set(A,B).
my_lowercase19(A):-downcase_atom(A,A).
my_set20(A):-list_to_set(A,A).
my_reverse21(A,B):-reverse(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_min_list3/2).
prim(my_max_list4/2).
prim(my_double5/2).
prim(my_flatten6/2).
prim(my_even7/1).
prim(my_succ8/2).
prim(my_last9/2).
prim(my_sumlist10/2).
prim(my_msort11/2).
prim(my_len12/2).
prim(my_odd13/1).
prim(my_toupper14/2).
prim(my_uppercase15/1).
prim(my_tolower16/2).
prim(my_pred17/2).
prim(my_list_to_set18/2).
prim(my_lowercase19/1).
prim(my_set20/1).
prim(my_reverse21/2).
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
p(['I','k','U','A','U','r','N'],'U').
p(['a','s','s','i','i','c','c'],'s').
p(['P','g','g','a','z','P','w','D','y'],'g').
p(['u','V','x','t','E','x'],'x').
p(['T','o','o','q','A'],'o').
q(['o','I','D','j','h','v','B','D'],'o').
q(['y','y','s','s','g','g','n','K','O'],'n').
q(['D','I','O','A','W','K','E','O','H','E'],'A').
q(['r','U','K','b','Z','J','Z'],'b').
q(['F','H','o','N','N','R','f','Z','j','A'],'R').
