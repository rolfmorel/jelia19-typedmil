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
my_double4(N,M):-M is 2*N,M =< 10.
my_set5(A):-list_to_set(A,A).
my_toupper6(A,B):-upcase_atom(A,B).
my_tolower7(A,B):-downcase_atom(A,B).
my_msort8(A,B):-msort(A,B).
my_lowercase9(A):-downcase_atom(A,A).
my_len10(A,B):-length(A,B).
my_uppercase11(A):-upcase_atom(A,A).
my_last12(A,B):-last(A,B).
my_pred13(A,B):-succ(B,A),A > 0.
my_sumlist14(A,B):-sumlist(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_even3/1).
prim(my_double4/2).
prim(my_set5/1).
prim(my_toupper6/2).
prim(my_tolower7/2).
prim(my_msort8/2).
prim(my_lowercase9/1).
prim(my_len10/2).
prim(my_uppercase11/1).
prim(my_last12/2).
prim(my_pred13/2).
prim(my_sumlist14/2).
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
p(['M','g','K','c','Y','h','d','e','Q','e'],'e').
p(['c','x','y','o','j','B','o','e','t'],'o').
p(['l','d','Z','d','i','C','b','Y','C','s'],'d').
p(['S','m','J','m','E','X'],'m').
p(['w','s','N','N','W','z','g'],'N').
q(['w','O','y','j','F','x','l','J','y'],'x').
q(['N','z','O','R','f','K','f','S'],'N').
q(['H','M','n','H','j','r','J','Y','d','V'],'r').
q(['e','y','D','Q','I','I','I','u','R','u','r'],'R').
q(['v','D','l','Z','v','s'],'s').
