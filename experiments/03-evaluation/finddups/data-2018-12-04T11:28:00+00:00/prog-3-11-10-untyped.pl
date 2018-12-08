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
my_max_list3(A,B):-max_list(A,B).
my_pred4(A,B):-succ(B,A),A > 0.
my_lowercase5(A):-downcase_atom(A,A).
my_reverse6(A,B):-reverse(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_msort8(A,B):-msort(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_flatten10(A,B):-flatten(A,B).
my_len11(A,B):-length(A,B).
my_even12(A):-0 is A mod 2.
my_list_to_set13(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_max_list3/2).
prim(my_pred4/2).
prim(my_lowercase5/1).
prim(my_reverse6/2).
prim(my_sumlist7/2).
prim(my_msort8/2).
prim(my_double9/2).
prim(my_flatten10/2).
prim(my_len11/2).
prim(my_even12/1).
prim(my_list_to_set13/2).
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
p(['E','N','A','i','E'],'E').
p(['J','O','c','a','c','O','J','Q','T'],'J').
p(['p','x','G','x','H'],'x').
p(['h','U','e','l','S','j','S'],'S').
p(['n','X','e','h','X'],'X').
q(['c','F','F','a','w','K','u','F','h'],'K').
q(['h','S','h','i','B','V','B'],'S').
q(['p','Y','j','j','j','G'],'G').
q(['R','Y','d','m','m','m','a','R'],'d').
q(['c','I','R','D','r','e','w','C','q','I'],'e').
