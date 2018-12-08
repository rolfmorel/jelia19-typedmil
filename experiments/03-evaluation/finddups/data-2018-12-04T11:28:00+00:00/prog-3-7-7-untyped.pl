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
my_pred5(A,B):-succ(B,A),A > 0.
my_uppercase6(A):-upcase_atom(A,A).
my_last7(A,B):-last(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_msort9(A,B):-msort(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_min_list3/2).
prim(my_sumlist4/2).
prim(my_pred5/2).
prim(my_uppercase6/1).
prim(my_last7/2).
prim(my_succ8/2).
prim(my_msort9/2).
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
p(['v','A','w','p','X','Y','h','v'],'v').
p(['Z','W','N','Z','N','K'],'Z').
p(['K','H','U','y','O','b','b','A','J'],'b').
p(['j','G','C','f','o','f','C','k'],'C').
p(['n','M','R','Z','Z'],'Z').
q(['J','j','K','j','O','m','t','D'],'J').
q(['F','h','r','G','Y','F','H'],'G').
q(['k','W','A','G','V','A','E','p','L','e'],'L').
q(['r','n','H','x','z','W','I','R','W','W','l'],'r').
q(['J','e','H','Y','Y','l'],'J').
