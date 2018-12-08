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
my_sumlist3(A,B):-sumlist(A,B).
my_set4(A):-list_to_set(A,A).
my_pred5(A,B):-succ(B,A),A > 0.
my_list_to_set6(A,B):-list_to_set(A,B).
my_len7(A,B):-length(A,B).
my_min_list8(A,B):-min_list(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_succ10(A,B):-succ(A,B),B =< 10.
my_max_list11(A,B):-max_list(A,B).
my_even12(A):-0 is A mod 2.
my_tolower13(A,B):-downcase_atom(A,B).
my_uppercase14(A):-upcase_atom(A,A).
my_last15(A,B):-last(A,B).
my_reverse16(A,B):-reverse(A,B).
my_odd17(A):-1 is A mod 2.
my_msort18(A,B):-msort(A,B).
my_toupper19(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_sumlist3/2).
prim(my_set4/1).
prim(my_pred5/2).
prim(my_list_to_set6/2).
prim(my_len7/2).
prim(my_min_list8/2).
prim(my_double9/2).
prim(my_succ10/2).
prim(my_max_list11/2).
prim(my_even12/1).
prim(my_tolower13/2).
prim(my_uppercase14/1).
prim(my_last15/2).
prim(my_reverse16/2).
prim(my_odd17/1).
prim(my_msort18/2).
prim(my_toupper19/2).
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
p(['V','V','j','O','w'],'V').
p(['g','d','g','f','P','t','L'],'g').
p(['T','q','Y','a','T','D'],'T').
p(['I','x','W','S','U','Y','W','T','w','n'],'W').
p(['v','M','z','F','w','C','v'],'v').
q(['s','K','h','e','q','e','N','m','L'],'s').
q(['h','b','X','d','U','H','v','v','U','y'],'X').
q(['O','c','W','H','c','Q','f','r','G','e'],'Q').
q(['J','H','o','D','H','W'],'J').
q(['z','o','v','G','y','j','v'],'o').
