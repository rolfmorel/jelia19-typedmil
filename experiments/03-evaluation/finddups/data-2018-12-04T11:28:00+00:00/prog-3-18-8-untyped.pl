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
my_even3(A):-0 is A mod 2.
my_min_list4(A,B):-min_list(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_odd6(A):-1 is A mod 2.
my_flatten7(A,B):-flatten(A,B).
my_lowercase8(A):-downcase_atom(A,A).
my_last9(A,B):-last(A,B).
my_set10(A):-list_to_set(A,A).
my_uppercase11(A):-upcase_atom(A,A).
my_len12(A,B):-length(A,B).
my_max_list13(A,B):-max_list(A,B).
my_msort14(A,B):-msort(A,B).
my_pred15(A,B):-succ(B,A),A > 0.
my_toupper16(A,B):-upcase_atom(A,B).
my_tolower17(A,B):-downcase_atom(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_reverse19(A,B):-reverse(A,B).
my_double20(N,M):-M is 2*N,M =< 10.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_even3/1).
prim(my_min_list4/2).
prim(my_succ5/2).
prim(my_odd6/1).
prim(my_flatten7/2).
prim(my_lowercase8/1).
prim(my_last9/2).
prim(my_set10/1).
prim(my_uppercase11/1).
prim(my_len12/2).
prim(my_max_list13/2).
prim(my_msort14/2).
prim(my_pred15/2).
prim(my_toupper16/2).
prim(my_tolower17/2).
prim(my_sumlist18/2).
prim(my_reverse19/2).
prim(my_double20/2).
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
p(['f','i','X','J','A','f','u','g','f','G'],'f').
p(['D','n','n','Y','n','f','D'],'n').
p(['B','Y','l','z','n','B','C','N','g','B'],'B').
p(['I','I','e','U','c'],'I').
p(['H','K','T','G','H','C','H'],'H').
q(['T','r','T','Q','N','o'],'r').
q(['e','z','d','W','P','V','O','R','G','O','z'],'G').
q(['T','X','K','U','N','T'],'X').
q(['H','j','X','T','Q','X','Y'],'H').
q(['w','m','N','p','s','v','b','w','W'],'W').
