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
my_flatten3(A,B):-flatten(A,B).
my_msort4(A,B):-msort(A,B).
my_max_list5(A,B):-max_list(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_toupper7(A,B):-upcase_atom(A,B).
my_last8(A,B):-last(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_even10(A):-0 is A mod 2.
my_min_list11(A,B):-min_list(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_reverse13(A,B):-reverse(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_flatten3/2).
prim(my_msort4/2).
prim(my_max_list5/2).
prim(my_lowercase6/1).
prim(my_toupper7/2).
prim(my_last8/2).
prim(my_sumlist9/2).
prim(my_even10/1).
prim(my_min_list11/2).
prim(my_succ12/2).
prim(my_reverse13/2).
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
p(['O','p','H','H','V','Q'],'H').
p(['Z','e','E','Q','Q','O'],'Q').
p(['t','G','v','h','o','v','G','S','m'],'G').
p(['o','G','T','G','X','y','Z','Z','x'],'G').
p(['T','S','G','d','d'],'d').
q(['A','u','O','h','u','t','Z','t'],'Z').
q(['K','e','S','t','s','I','s'],'I').
q(['s','T','n','O','h','n','A','U','l'],'T').
q(['K','v','K','d','D','u','b','z','z','R'],'v').
q(['l','n','M','T','Q','F','Q','B','h','L','G'],'l').
