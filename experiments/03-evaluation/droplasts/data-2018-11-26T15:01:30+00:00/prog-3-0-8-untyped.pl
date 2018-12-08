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

prim(my_tail0/2).
prim(my_reverse1/2).
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
p([['Y','t','I','e'],['Z','k','H']],[['Y','t','I'],['Z','k']]).
p([['d','L','S','m'],['s','I','P'],['G','F','C','Y'],['F','L','l','m']],[['d','L','S'],['s','I'],['G','F','C'],['F','L','l']]).
p([['I','i','n','B'],['F','X','g'],['Z','G','A','j']],[['I','i','n'],['F','X'],['Z','G','A']]).
p([['n','M','l','z'],['W','S','l'],['V','C','o'],['Q','P','B']],[['n','M','l'],['W','S'],['V','C'],['Q','P']]).
p([['M','x','V'],['H','U','U'],['i','H','z']],[['M','x'],['H','U'],['i','H']]).
q([['u','u','K'],['w','M','H','Q'],['K','U','p']],[['u','u','K'],['w','M','H'],['K','U','p']]).
q([['g','I','I'],['w','Y','U']],[['g','I','I'],['w','Y']]).
q([['c','E','Z','Q'],['R','f','r','O']],[['c','E','Z','Q'],['R','f','r']]).
q([['b','P','L'],['Z','X','R']],[['b','P','L'],['Z','X']]).
q([['C','y','o'],['N','x','Q']],[['C','y','o'],['N','x']]).
