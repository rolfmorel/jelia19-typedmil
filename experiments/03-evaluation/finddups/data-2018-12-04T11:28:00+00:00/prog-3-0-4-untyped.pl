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
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
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
p(['G','m','z','h','r','G'],'G').
p(['W','n','C','k','Q','U','X','n','S'],'n').
p(['K','y','P','N','T','e','E','m','G','e'],'e').
p(['v','f','o','X','P','o','Y'],'o').
p(['P','k','u','C','k'],'k').
q(['p','y','R','L','t','M','M'],'p').
q(['k','e','u','u','V','X','M','f','O'],'f').
q(['b','E','e','l','T','B','C','T'],'B').
q(['P','R','Z','P','y','g'],'g').
q(['g','k','l','M','M','n','f'],'g').
