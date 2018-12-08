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
my_len4(A,B):-length(A,B).
my_even5(A):-0 is A mod 2.
my_toupper6(A,B):-upcase_atom(A,B).
my_max_list7(A,B):-max_list(A,B).
my_flatten8(A,B):-flatten(A,B).
my_list_to_set9(A,B):-list_to_set(A,B).
my_odd10(A):-1 is A mod 2.
my_set11(A):-list_to_set(A,A).
my_tolower12(A,B):-downcase_atom(A,B).
my_min_list13(A,B):-min_list(A,B).
my_reverse14(A,B):-reverse(A,B).
my_uppercase15(A):-upcase_atom(A,A).
my_lowercase16(A):-downcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_last3/2).
prim(my_len4/2).
prim(my_even5/1).
prim(my_toupper6/2).
prim(my_max_list7/2).
prim(my_flatten8/2).
prim(my_list_to_set9/2).
prim(my_odd10/1).
prim(my_set11/1).
prim(my_tolower12/2).
prim(my_min_list13/2).
prim(my_reverse14/2).
prim(my_uppercase15/1).
prim(my_lowercase16/1).
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
p(['H','V','x','x','P'],'x').
p(['x','g','t','u','O','O','p','V','W','Q'],'O').
p(['d','k','F','T','h','c','F','Z','F','s'],'F').
p(['V','A','U','V','s','H','s','S','n','U'],'U').
p(['S','o','Z','V','A','T','n','r','A','b'],'A').
q(['A','W','j','S','c','u','G','Y','u','i'],'A').
q(['E','D','O','J','S','c','O','Q','U'],'Q').
q(['B','C','C','z','l','l','g','m'],'m').
q(['F','G','Q','S','Q','x','U','b','r','C','r'],'x').
q(['I','w','k','G','c','Q','e','N','G'],'c').
