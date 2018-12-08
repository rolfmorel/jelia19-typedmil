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
my_min_list4(A,B):-min_list(A,B).
my_last5(A,B):-last(A,B).
my_len6(A,B):-length(A,B).
my_odd7(A):-1 is A mod 2.
my_pred8(A,B):-succ(B,A),A > 0.
my_lowercase9(A):-downcase_atom(A,A).
my_toupper10(A,B):-upcase_atom(A,B).
my_even11(A):-0 is A mod 2.
my_reverse12(A,B):-reverse(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_double3/2).
prim(my_min_list4/2).
prim(my_last5/2).
prim(my_len6/2).
prim(my_odd7/1).
prim(my_pred8/2).
prim(my_lowercase9/1).
prim(my_toupper10/2).
prim(my_even11/1).
prim(my_reverse12/2).
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
p(['d','Q','Q','d','q'],'d').
p(['q','a','a','W','w'],'a').
p(['f','b','Z','u','f'],'f').
p(['q','s','o','K','O','M','Y','Y','Q'],'Y').
p(['Q','Y','D','K','c','W','W','B','r'],'W').
q(['A','O','O','N','n','e','Q','W'],'W').
q(['D','C','P','s','A','Z','s','D','E','R'],'P').
q(['N','c','F','x','t','T','K','T','R'],'N').
q(['e','y','d','o','K','z','K','M','S'],'e').
q(['Y','n','P','z','l','v','U','H','z','q','U'],'n').
