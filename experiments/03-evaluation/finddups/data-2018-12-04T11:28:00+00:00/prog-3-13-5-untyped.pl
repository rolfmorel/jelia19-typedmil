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
my_double3(N,M):-M is 2*N,M =< 10.
my_msort4(A,B):-msort(A,B).
my_max_list5(A,B):-max_list(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_odd7(A):-1 is A mod 2.
my_sumlist8(A,B):-sumlist(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_lowercase10(A):-downcase_atom(A,A).
my_toupper11(A,B):-upcase_atom(A,B).
my_list_to_set12(A,B):-list_to_set(A,B).
my_set13(A):-list_to_set(A,A).
my_last14(A,B):-last(A,B).
my_even15(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_double3/2).
prim(my_msort4/2).
prim(my_max_list5/2).
prim(my_pred6/2).
prim(my_odd7/1).
prim(my_sumlist8/2).
prim(my_succ9/2).
prim(my_lowercase10/1).
prim(my_toupper11/2).
prim(my_list_to_set12/2).
prim(my_set13/1).
prim(my_last14/2).
prim(my_even15/1).
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
p(['d','d','d','y','w','K','M'],'d').
p(['l','L','S','l','u'],'l').
p(['X','p','Z','X','Z','O','L','W','Z','v'],'X').
p(['E','f','P','G','E','h'],'E').
p(['K','K','i','J','Q'],'K').
q(['b','v','u','p','j','Q','G','E','G','p','q'],'Q').
q(['M','c','k','G','H','n','H','Y','{','n','i'],'{').
q(['K','c','a','c','h','C','z','v','k','D','z'],'h').
q(['M','b','M','b','k','c','K','N','x'],'N').
q(['l','l','G','m','f','t','B'],'f').
