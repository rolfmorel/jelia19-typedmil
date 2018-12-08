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
my_list_to_set3(A,B):-list_to_set(A,B).
my_pred4(A,B):-succ(B,A),A > 0.
my_succ5(A,B):-succ(A,B),B =< 10.
my_reverse6(A,B):-reverse(A,B).
my_flatten7(A,B):-flatten(A,B).
my_even8(A):-0 is A mod 2.
my_tolower9(A,B):-downcase_atom(A,B).
my_set10(A):-list_to_set(A,A).
my_toupper11(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_list_to_set3/2).
prim(my_pred4/2).
prim(my_succ5/2).
prim(my_reverse6/2).
prim(my_flatten7/2).
prim(my_even8/1).
prim(my_tolower9/2).
prim(my_set10/1).
prim(my_toupper11/2).
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
p(['c','L','J','p','D','D'],'D').
p(['i','X','b','R','p','X','o','I'],'X').
p(['G','G','r','R','X'],'G').
p(['y','t','h','h','n','L'],'h').
p(['c','E','y','c','I','c','A'],'c').
q(['E','E','b','d','y','F','T','T'],'F').
q(['K','a','K','R','e','n','z'],'a').
q(['p','D','P','U','B','P','u','f'],'f').
q(['t','g','N','v','r','z','Z','v','e'],'z').
q(['u','b','V','q','J','l','R','l'],'q').
