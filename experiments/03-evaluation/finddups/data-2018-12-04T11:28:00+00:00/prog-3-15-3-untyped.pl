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
my_toupper3(A,B):-upcase_atom(A,B).
my_pred4(A,B):-succ(B,A),A > 0.
my_len5(A,B):-length(A,B).
my_set6(A):-list_to_set(A,A).
my_list_to_set7(A,B):-list_to_set(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_lowercase9(A):-downcase_atom(A,A).
my_flatten10(A,B):-flatten(A,B).
my_odd11(A):-1 is A mod 2.
my_last12(A,B):-last(A,B).
my_min_list13(A,B):-min_list(A,B).
my_reverse14(A,B):-reverse(A,B).
my_succ15(A,B):-succ(A,B),B =< 10.
my_max_list16(A,B):-max_list(A,B).
my_uppercase17(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_toupper3/2).
prim(my_pred4/2).
prim(my_len5/2).
prim(my_set6/1).
prim(my_list_to_set7/2).
prim(my_double8/2).
prim(my_lowercase9/1).
prim(my_flatten10/2).
prim(my_odd11/1).
prim(my_last12/2).
prim(my_min_list13/2).
prim(my_reverse14/2).
prim(my_succ15/2).
prim(my_max_list16/2).
prim(my_uppercase17/1).
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
p(['h','h','x','F','P','M','L'],'h').
p(['N','X','v','l','v'],'v').
p(['U','f','V','f','n','Y','C','f','v','U'],'f').
p(['G','V','K','I','K','w','i','z','K'],'K').
p(['D','W','N','N','V','V','t','v'],'N').
q(['l','v','c','B','B','W','y','F','F'],'y').
q(['w','c','q','k','d','E','X','E','w'],'X').
q(['W','e','a','u','j','u','Q'],'W').
q(['o','Z','f','Y','G','k','k','D','v','p','B'],'G').
q(['G','b','R','n','R','j'],'G').
