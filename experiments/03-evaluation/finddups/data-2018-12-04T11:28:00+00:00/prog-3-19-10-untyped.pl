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
my_max_list4(A,B):-max_list(A,B).
my_set5(A):-list_to_set(A,A).
my_len6(A,B):-length(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_reverse8(A,B):-reverse(A,B).
my_uppercase9(A):-upcase_atom(A,A).
my_even10(A):-0 is A mod 2.
my_pred11(A,B):-succ(B,A),A > 0.
my_min_list12(A,B):-min_list(A,B).
my_flatten13(A,B):-flatten(A,B).
my_succ14(A,B):-succ(A,B),B =< 10.
my_sumlist15(A,B):-sumlist(A,B).
my_odd16(A):-1 is A mod 2.
my_double17(N,M):-M is 2*N,M =< 10.
my_last18(A,B):-last(A,B).
my_tolower19(A,B):-downcase_atom(A,B).
my_msort20(A,B):-msort(A,B).
my_lowercase21(A):-downcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_toupper3/2).
prim(my_max_list4/2).
prim(my_set5/1).
prim(my_len6/2).
prim(my_list_to_set7/2).
prim(my_reverse8/2).
prim(my_uppercase9/1).
prim(my_even10/1).
prim(my_pred11/2).
prim(my_min_list12/2).
prim(my_flatten13/2).
prim(my_succ14/2).
prim(my_sumlist15/2).
prim(my_odd16/1).
prim(my_double17/2).
prim(my_last18/2).
prim(my_tolower19/2).
prim(my_msort20/2).
prim(my_lowercase21/1).
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
p(['d','V','R','a','H','R','R','B','D'],'R').
p(['o','X','v','H','o','v','S','n'],'o').
p(['r','D','u','g','r'],'r').
p(['N','I','t','Q','x','t','P','E','F','T'],'t').
p(['V','h','o','o','O','M'],'o').
q(['X','k','v','X','d','n','S'],'k').
q(['c','o','J','j','r','q','j','X'],'J').
q(['{','O','P','f','L','D','L','Y'],'{').
q(['E','B','R','r','N','B','w','q','X','a'],'R').
q(['G','c','c','Q','s','c'],'s').
