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
my_tolower3(A,B):-downcase_atom(A,B).
my_succ4(A,B):-succ(A,B),B =< 10.
my_odd5(A):-1 is A mod 2.
my_len6(A,B):-length(A,B).
my_max_list7(A,B):-max_list(A,B).
my_flatten8(A,B):-flatten(A,B).
my_even9(A):-0 is A mod 2.
my_set10(A):-list_to_set(A,A).
my_lowercase11(A):-downcase_atom(A,A).
my_reverse12(A,B):-reverse(A,B).
my_list_to_set13(A,B):-list_to_set(A,B).
my_toupper14(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_tolower3/2).
prim(my_succ4/2).
prim(my_odd5/1).
prim(my_len6/2).
prim(my_max_list7/2).
prim(my_flatten8/2).
prim(my_even9/1).
prim(my_set10/1).
prim(my_lowercase11/1).
prim(my_reverse12/2).
prim(my_list_to_set13/2).
prim(my_toupper14/2).
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
p(['Z','a','N','l','F','z','N','V'],'N').
p(['V','Q','L','s','w','t','t','t'],'t').
p(['z','m','l','A','I','t','s','z','W'],'z').
p(['b','J','n','m','Q','U','t','Q'],'Q').
p(['D','N','u','D','S','a'],'D').
q(['a','m','J','o','C','C','N','C','X'],'a').
q(['D','x','p','d','H','d','u'],'x').
q(['V','Y','e','V','t','o','v','A'],'A').
q(['R','t','Y','W','I','j','I','x','S'],'x').
q(['I','r','y','O','i','t','I','A'],'A').
