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
my_last3(A,B):-last(A,B).
my_lowercase4(A):-downcase_atom(A,A).
my_set5(A):-list_to_set(A,A).
my_double6(N,M):-M is 2*N,M =< 10.
my_reverse7(A,B):-reverse(A,B).
my_even8(A):-0 is A mod 2.
my_tolower9(A,B):-downcase_atom(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
my_uppercase12(A):-upcase_atom(A,A).
my_len13(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_last3/2).
prim(my_lowercase4/1).
prim(my_set5/1).
prim(my_double6/2).
prim(my_reverse7/2).
prim(my_even8/1).
prim(my_tolower9/2).
prim(my_sumlist10/2).
prim(my_list_to_set11/2).
prim(my_uppercase12/1).
prim(my_len13/2).
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
p(['N','C','Z','Y','a','B','Y','k'],'Y').
p(['R','D','J','i','K','o','o','W','p'],'o').
p(['z','S','D','o','D','T','h','X','H','z'],'z').
p(['i','t','J','h','a','n','q','q'],'q').
p(['J','C','j','j','h','W'],'j').
q(['F','O','Q','p','w','W','W','q'],'w').
q(['S','S','W','b','b','H','C','q','L'],'q').
q(['f','C','O','S','K','I','C','o','X'],'O').
q(['E','t','y','E','y','E','P','U','t','g','E'],'P').
q(['H','H','W','b','L','y','r','P','G','S','Y'],'r').
