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
my_len3(A,B):-length(A,B).
my_list_to_set4(A,B):-list_to_set(A,B).
my_even5(A):-0 is A mod 2.
my_reverse6(A,B):-reverse(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_uppercase8(A):-upcase_atom(A,A).
my_tolower9(A,B):-downcase_atom(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_msort11(A,B):-msort(A,B).
my_flatten12(A,B):-flatten(A,B).
my_min_list13(A,B):-min_list(A,B).
my_toupper14(A,B):-upcase_atom(A,B).
my_max_list15(A,B):-max_list(A,B).
my_odd16(A):-1 is A mod 2.
my_sumlist17(A,B):-sumlist(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
prim(my_list_to_set4/2).
prim(my_even5/1).
prim(my_reverse6/2).
prim(my_succ7/2).
prim(my_uppercase8/1).
prim(my_tolower9/2).
prim(my_lowercase10/1).
prim(my_msort11/2).
prim(my_flatten12/2).
prim(my_min_list13/2).
prim(my_toupper14/2).
prim(my_max_list15/2).
prim(my_odd16/1).
prim(my_sumlist17/2).
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
p(['f','R','p','b','m','f','E','l','E'],'f').
p(['k','W','c','c','b'],'c').
p(['z','C','y','y','l','d','P','Z','n','n'],'y').
p(['I','S','o','o','f','S','f','a','e'],'f').
p(['g','J','E','a','E','z','O'],'E').
q(['k','V','l','M','q','V'],'q').
q(['D','T','l','d','B','l','F','d','W'],'B').
q(['e','e','F','m','A','S','Z','{','X','P'],'{').
q(['U','Q','u','U','Y','y'],'Y').
q(['z','B','E','K','g','b','z','f','B','B'],'E').
