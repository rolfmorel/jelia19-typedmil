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
my_sumlist3(A,B):-sumlist(A,B).
my_min_list4(A,B):-min_list(A,B).
my_flatten5(A,B):-flatten(A,B).
my_tolower6(A,B):-downcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_sumlist3/2).
prim(my_min_list4/2).
prim(my_flatten5/2).
prim(my_tolower6/2).
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
p(['K','W','Y','I','Y','s','Y','U','n'],'Y').
p(['H','a','H','Y','G','t','s'],'H').
p(['c','A','A','r','T','O','U','f','c','U'],'A').
p(['J','m','T','B','y','f','B','T'],'B').
p(['s','W','F','g','l','F'],'F').
q(['H','k','I','x','E','a','H','A'],'x').
q(['W','X','E','E','h','H','d','k','X'],'W').
q(['h','T','W','h','m','m'],'W').
q(['t','N','N','y','T','T','l','o','w','M'],'y').
q(['v','e','t','n','A','F','U','U'],'v').
