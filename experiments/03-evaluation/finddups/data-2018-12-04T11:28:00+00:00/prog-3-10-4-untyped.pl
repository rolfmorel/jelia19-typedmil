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
my_succ3(A,B):-succ(A,B),B =< 10.
my_even4(A):-0 is A mod 2.
my_list_to_set5(A,B):-list_to_set(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_msort7(A,B):-msort(A,B).
my_last8(A,B):-last(A,B).
my_odd9(A):-1 is A mod 2.
my_min_list10(A,B):-min_list(A,B).
my_max_list11(A,B):-max_list(A,B).
my_uppercase12(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_succ3/2).
prim(my_even4/1).
prim(my_list_to_set5/2).
prim(my_lowercase6/1).
prim(my_msort7/2).
prim(my_last8/2).
prim(my_odd9/1).
prim(my_min_list10/2).
prim(my_max_list11/2).
prim(my_uppercase12/1).
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
p(['O','O','c','r','r','U','v'],'O').
p(['c','c','M','J','g','B','c','c'],'c').
p(['P','M','M','b','n','f','c','E','M'],'M').
p(['L','p','r','P','g','p'],'p').
p(['F','D','x','o','q','F','W'],'F').
q(['F','t','i','j','I','V','V','L','P','i'],'L').
q(['x','K','x','d','T','x','d'],'T').
q(['Y','i','N','L','D','t','u','i','a'],'a').
q(['V','r','K','n','i','i'],'V').
q(['y','y','F','D','Q','p','W'],'W').
