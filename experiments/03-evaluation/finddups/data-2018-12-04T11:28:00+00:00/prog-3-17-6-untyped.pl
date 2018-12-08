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
my_sumlist4(A,B):-sumlist(A,B).
my_len5(A,B):-length(A,B).
my_reverse6(A,B):-reverse(A,B).
my_tolower7(A,B):-downcase_atom(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
my_set9(A):-list_to_set(A,A).
my_last10(A,B):-last(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_min_list12(A,B):-min_list(A,B).
my_flatten13(A,B):-flatten(A,B).
my_list_to_set14(A,B):-list_to_set(A,B).
my_msort15(A,B):-msort(A,B).
my_lowercase16(A):-downcase_atom(A,A).
my_odd17(A):-1 is A mod 2.
my_double18(N,M):-M is 2*N,M =< 10.
my_succ19(A,B):-succ(A,B),B =< 10.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_even3/1).
prim(my_sumlist4/2).
prim(my_len5/2).
prim(my_reverse6/2).
prim(my_tolower7/2).
prim(my_toupper8/2).
prim(my_set9/1).
prim(my_last10/2).
prim(my_pred11/2).
prim(my_min_list12/2).
prim(my_flatten13/2).
prim(my_list_to_set14/2).
prim(my_msort15/2).
prim(my_lowercase16/1).
prim(my_odd17/1).
prim(my_double18/2).
prim(my_succ19/2).
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
p(['m','V','R','R','N','j','h','G','q','k'],'R').
p(['Q','B','o','d','o','B','F','V'],'B').
p(['c','L','U','Y','g','Y'],'Y').
p(['q','D','a','l','O','P','q','Y','x'],'q').
p(['z','Q','e','e','P','G','e','k','F'],'e').
q(['P','f','Z','o','Z','E','M','l','r','Z'],'o').
q(['c','V','M','S','G','p','G'],'M').
q(['J','j','V','X','j','V','U','H'],'X').
q(['a','X','E','i','b','i'],'b').
q(['i','L','L','i','s','R','g','T','B','y','y'],'R').
