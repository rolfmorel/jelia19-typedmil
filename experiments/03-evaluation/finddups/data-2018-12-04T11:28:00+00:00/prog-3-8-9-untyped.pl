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
my_uppercase3(A):-upcase_atom(A,A).
my_set4(A):-list_to_set(A,A).
my_last5(A,B):-last(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_min_list7(A,B):-min_list(A,B).
my_len8(A,B):-length(A,B).
my_toupper9(A,B):-upcase_atom(A,B).
my_succ10(A,B):-succ(A,B),B =< 10.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_uppercase3/1).
prim(my_set4/1).
prim(my_last5/2).
prim(my_double6/2).
prim(my_min_list7/2).
prim(my_len8/2).
prim(my_toupper9/2).
prim(my_succ10/2).
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
p(['r','w','H','Y','H'],'H').
p(['s','s','d','O','O'],'s').
p(['c','o','J','a','p','J','e','e'],'e').
p(['e','n','w','S','X','w'],'w').
p(['K','x','a','M','a'],'a').
q(['W','o','q','r','M','m','o'],'M').
q(['V','W','L','X','p','z','G','p','Q','u'],'z').
q(['J','J','E','l','l','U','O','s','y','W'],'E').
q(['U','E','h','z','l','P','E','k','p'],'P').
q(['O','Y','q','I','c','q'],'Y').
