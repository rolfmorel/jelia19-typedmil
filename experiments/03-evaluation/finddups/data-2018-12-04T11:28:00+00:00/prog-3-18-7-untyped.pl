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
my_sumlist4(A,B):-sumlist(A,B).
my_set5(A):-list_to_set(A,A).
my_succ6(A,B):-succ(A,B),B =< 10.
my_last7(A,B):-last(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_list_to_set9(A,B):-list_to_set(A,B).
my_max_list10(A,B):-max_list(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_toupper12(A,B):-upcase_atom(A,B).
my_odd13(A):-1 is A mod 2.
my_flatten14(A,B):-flatten(A,B).
my_reverse15(A,B):-reverse(A,B).
my_even16(A):-0 is A mod 2.
my_uppercase17(A):-upcase_atom(A,A).
my_tolower18(A,B):-downcase_atom(A,B).
my_len19(A,B):-length(A,B).
my_lowercase20(A):-downcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_min_list3/2).
prim(my_sumlist4/2).
prim(my_set5/1).
prim(my_succ6/2).
prim(my_last7/2).
prim(my_double8/2).
prim(my_list_to_set9/2).
prim(my_max_list10/2).
prim(my_pred11/2).
prim(my_toupper12/2).
prim(my_odd13/1).
prim(my_flatten14/2).
prim(my_reverse15/2).
prim(my_even16/1).
prim(my_uppercase17/1).
prim(my_tolower18/2).
prim(my_len19/2).
prim(my_lowercase20/1).
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
p(['e','A','D','e','v','C','d'],'e').
p(['J','G','G','o','G'],'G').
p(['p','O','a','s','a','a','o'],'a').
p(['f','e','P','e','E','D','S'],'e').
p(['r','V','j','B','t','q','d','M','M'],'M').
q(['Q','P','p','c','P','n'],'c').
q(['L','h','U','f','M','U'],'L').
q(['K','H','Q','Q','z','U'],'z').
q(['Z','p','p','y','M','H','p','w'],'Z').
q(['D','a','a','m','g','K','k','o','D','I'],'I').
