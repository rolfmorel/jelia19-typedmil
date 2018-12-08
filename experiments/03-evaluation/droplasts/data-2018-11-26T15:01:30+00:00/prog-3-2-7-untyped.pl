:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
my_tail0([_|TL],TL).
my_reverse1(A,B):-reverse(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_toupper3(A,B):-upcase_atom(A,B).
my_uppercase4(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_toupper3/2).
prim(my_uppercase4/1).
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
p([['W','s','r','U'],['l','T','f','I']],[['W','s','r'],['l','T','f']]).
p([['r','C','V'],['d','p','b','m']],[['r','C'],['d','p','b']]).
p([['e','Q','p'],['G','L','y','O'],['Y','p','p','e']],[['e','Q'],['G','L','y'],['Y','p','p']]).
p([['p','E','f'],['K','F','o','s'],['t','O','r','Y']],[['p','E'],['K','F','o'],['t','O','r']]).
p([['e','Z','U'],['F','F','E']],[['e','Z'],['F','F']]).
q([['d','i','q'],['S','M','G']],[['d','i'],['S','M','G']]).
q([['Z','P','T','g'],['y','y','i']],[['Z','P','T','g'],['y','y']]).
q([['w','q','M','h'],['B','W','q','n']],[['w','q','M'],['B','W','q','n']]).
q([['F','n','m','y'],['X','T','o','F']],[['F','n','m'],['X','T','o','F']]).
q([['s','i','l','V'],['L','d','g'],['v','w','k'],['y','f','L','K']],[['s','i','l','V'],['L','d','g'],['v','w'],['y','f','L','K']]).
