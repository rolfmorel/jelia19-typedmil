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
my_pred3(A,B):-succ(B,A),A > 0.
my_double4(N,M):-M is 2*N,M =< 10.
my_odd5(A):-1 is A mod 2.
my_set6(A):-list_to_set(A,A).
my_msort7(A,B):-msort(A,B).
my_uppercase8(A):-upcase_atom(A,A).
my_succ9(A,B):-succ(A,B),B =< 10.
my_list_to_set10(A,B):-list_to_set(A,B).
my_even11(A):-0 is A mod 2.
my_sumlist12(A,B):-sumlist(A,B).
my_min_list13(A,B):-min_list(A,B).
my_lowercase14(A):-downcase_atom(A,A).
my_last15(A,B):-last(A,B).
my_reverse16(A,B):-reverse(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_pred3/2).
prim(my_double4/2).
prim(my_odd5/1).
prim(my_set6/1).
prim(my_msort7/2).
prim(my_uppercase8/1).
prim(my_succ9/2).
prim(my_list_to_set10/2).
prim(my_even11/1).
prim(my_sumlist12/2).
prim(my_min_list13/2).
prim(my_lowercase14/1).
prim(my_last15/2).
prim(my_reverse16/2).
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
p(['t','q','v','N','q'],'q').
p(['T','E','s','K','Y','V','d','K','f'],'K').
p(['C','a','w','O','g','a','t','f'],'a').
p(['D','P','n','L','J','P'],'P').
p(['p','B','o','p','i','p'],'p').
q(['T','v','A','v','t','j','a','H','o','B','m'],'A').
q(['x','u','u','H','q','S','P','r'],'q').
q(['l','b','K','p','K','E','Q','X','r'],'E').
q(['O','B','l','i','i','A','z'],'O').
q(['S','Z','P','l','k','T','l','r'],'Z').
