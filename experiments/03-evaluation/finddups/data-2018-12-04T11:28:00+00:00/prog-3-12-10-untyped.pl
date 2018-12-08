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
my_uppercase4(A):-upcase_atom(A,A).
my_toupper5(A,B):-upcase_atom(A,B).
my_odd6(A):-1 is A mod 2.
my_double7(N,M):-M is 2*N,M =< 10.
my_flatten8(A,B):-flatten(A,B).
my_list_to_set9(A,B):-list_to_set(A,B).
my_len10(A,B):-length(A,B).
my_max_list11(A,B):-max_list(A,B).
my_tolower12(A,B):-downcase_atom(A,B).
my_even13(A):-0 is A mod 2.
my_set14(A):-list_to_set(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_sumlist3/2).
prim(my_uppercase4/1).
prim(my_toupper5/2).
prim(my_odd6/1).
prim(my_double7/2).
prim(my_flatten8/2).
prim(my_list_to_set9/2).
prim(my_len10/2).
prim(my_max_list11/2).
prim(my_tolower12/2).
prim(my_even13/1).
prim(my_set14/1).
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
p(['q','A','X','f','A','b','h','U'],'A').
p(['s','N','F','M','j','s','h','y','T','M'],'M').
p(['Z','H','f','B','B','H'],'H').
p(['j','E','d','E','g','e','E','j'],'E').
p(['I','x','B','f','h','K','i','I','s','h'],'I').
q(['T','P','A','P','I','p','B'],'p').
q(['W','B','B','v','X','c','I','H','k','Q'],'Q').
q(['w','o','v','I','S','J','M','t','h','B','J'],'S').
q(['t','D','q','L','L','q'],'t').
q(['h','x','J','B','U','J','q','V','M'],'M').
