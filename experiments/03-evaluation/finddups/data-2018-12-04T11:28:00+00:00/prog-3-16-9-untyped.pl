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
my_tolower3(A,B):-downcase_atom(A,B).
my_odd4(A):-1 is A mod 2.
my_succ5(A,B):-succ(A,B),B =< 10.
my_uppercase6(A):-upcase_atom(A,A).
my_max_list7(A,B):-max_list(A,B).
my_len8(A,B):-length(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_sumlist10(A,B):-sumlist(A,B).
my_msort11(A,B):-msort(A,B).
my_min_list12(A,B):-min_list(A,B).
my_last13(A,B):-last(A,B).
my_double14(N,M):-M is 2*N,M =< 10.
my_list_to_set15(A,B):-list_to_set(A,B).
my_flatten16(A,B):-flatten(A,B).
my_set17(A):-list_to_set(A,A).
my_reverse18(A,B):-reverse(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_tolower3/2).
prim(my_odd4/1).
prim(my_succ5/2).
prim(my_uppercase6/1).
prim(my_max_list7/2).
prim(my_len8/2).
prim(my_pred9/2).
prim(my_sumlist10/2).
prim(my_msort11/2).
prim(my_min_list12/2).
prim(my_last13/2).
prim(my_double14/2).
prim(my_list_to_set15/2).
prim(my_flatten16/2).
prim(my_set17/1).
prim(my_reverse18/2).
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
p(['z','O','F','Y','p','m','c','s','w','F'],'F').
p(['a','Q','R','m','l','T','F','U','U','m'],'m').
p(['w','t','A','k','f','W','H','M','k'],'k').
p(['Q','Q','Z','k','J','v','J','U'],'J').
p(['B','g','U','o','o','F','w','X','j'],'o').
q(['o','V','H','f','A','l','Z','l','i'],'H').
q(['J','C','A','S','P','S','V'],'J').
q(['m','R','z','F','I','N','F','t'],'m').
q(['Z','C','i','w','A','A','Y','s','i','R','k'],'w').
q(['l','D','U','F','v','m','k','Z','B','Z','k'],'v').
