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
my_odd3(A):-1 is A mod 2.
my_double4(N,M):-M is 2*N,M =< 10.
my_len5(A,B):-length(A,B).
my_set6(A):-list_to_set(A,A).
my_last7(A,B):-last(A,B).
my_uppercase8(A):-upcase_atom(A,A).
my_toupper9(A,B):-upcase_atom(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_even11(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_odd3/1).
prim(my_double4/2).
prim(my_len5/2).
prim(my_set6/1).
prim(my_last7/2).
prim(my_uppercase8/1).
prim(my_toupper9/2).
prim(my_lowercase10/1).
prim(my_even11/1).
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
p(['i','a','G','X','S','A','A','S'],'A').
p(['P','N','s','s','u','i'],'s').
p(['o','P','A','Z','Z','M','w','U','o'],'o').
p(['R','W','G','R','A','k'],'R').
p(['d','e','e','G','G'],'e').
q(['j','x','j','W','R','T','a','r','c','N','A'],'T').
q(['a','t','T','Y','r','i','z','t'],'a').
q(['H','Y','W','e','e','v','M','e','X','O'],'v').
q(['U','M','s','a','f','Q','N','g','N','w','N'],'U').
q(['N','B','p','Q','i','i'],'Q').
