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
my_succ3(A,B):-succ(A,B),B =< 10.
my_tolower4(A,B):-downcase_atom(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_toupper6(A,B):-upcase_atom(A,B).
my_reverse7(A,B):-reverse(A,B).
my_last8(A,B):-last(A,B).
my_max_list9(A,B):-max_list(A,B).
my_set10(A):-list_to_set(A,A).
my_msort11(A,B):-msort(A,B).
my_len12(A,B):-length(A,B).
my_double13(N,M):-M is 2*N,M =< 10.
my_min_list14(A,B):-min_list(A,B).
my_even15(A):-0 is A mod 2.
my_list_to_set16(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_succ3/2).
prim(my_tolower4/2).
prim(my_lowercase5/1).
prim(my_toupper6/2).
prim(my_reverse7/2).
prim(my_last8/2).
prim(my_max_list9/2).
prim(my_set10/1).
prim(my_msort11/2).
prim(my_len12/2).
prim(my_double13/2).
prim(my_min_list14/2).
prim(my_even15/1).
prim(my_list_to_set16/2).
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
p(['z','K','T','z','W'],'z').
p(['Z','V','J','Y','Y','a','O'],'Y').
p(['N','N','Q','T','a','N','h','p','j'],'N').
p(['G','M','l','d','U','l'],'l').
p(['o','I','Y','o','y'],'o').
q(['W','z','S','K','z','J','N'],'W').
q(['B','h','d','h','u','U','F','R','p','e'],'U').
q(['p','j','V','y','E','P','w','p'],'y').
q(['h','w','T','u','E','V','p','K','K','T','P'],'E').
q(['S','C','R','C','R','L'],'L').
