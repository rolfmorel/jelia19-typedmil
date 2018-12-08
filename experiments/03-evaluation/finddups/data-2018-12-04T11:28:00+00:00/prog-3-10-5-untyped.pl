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
my_even4(A):-0 is A mod 2.
my_toupper5(A,B):-upcase_atom(A,B).
my_last6(A,B):-last(A,B).
my_max_list7(A,B):-max_list(A,B).
my_min_list8(A,B):-min_list(A,B).
my_odd9(A):-1 is A mod 2.
my_succ10(A,B):-succ(A,B),B =< 10.
my_msort11(A,B):-msort(A,B).
my_set12(A):-list_to_set(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_flatten3/2).
prim(my_even4/1).
prim(my_toupper5/2).
prim(my_last6/2).
prim(my_max_list7/2).
prim(my_min_list8/2).
prim(my_odd9/1).
prim(my_succ10/2).
prim(my_msort11/2).
prim(my_set12/1).
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
p(['G','c','A','N','s','A','g','o','s'],'s').
p(['k','a','p','R','p','a'],'p').
p(['i','U','F','h','F'],'F').
p(['i','l','s','w','y','l','t','v'],'l').
p(['o','F','q','O','b','q','W','E','X'],'q').
q(['f','Y','p','s','g','D','e','s','G','x','N'],'G').
q(['e','o','M','Y','o','r','I'],'I').
q(['N','U','G','A','d','U','M'],'G').
q(['g','a','G','G','X','c'],'X').
q(['F','Q','A','R','j','i','T','J','l','F'],'J').
