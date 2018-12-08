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
p([['s','O','i'],['h','R','h']],[['s','O'],['h','R']]).
p([['B','l','X'],['h','V','A','w'],['S','u','s'],['B','V','f']],[['B','l'],['h','V','A'],['S','u'],['B','V']]).
p([['U','G','F','w'],['T','D','N'],['R','n','i','Q'],['Z','B','k']],[['U','G','F'],['T','D'],['R','n','i'],['Z','B']]).
p([['H','T','H'],['O','F','N'],['q','w','M']],[['H','T'],['O','F'],['q','w']]).
p([['d','B','g'],['e','w','t','c'],['P','w','b','L']],[['d','B'],['e','w','t'],['P','w','b']]).
q([['Z','s','u'],['l','A','n','M'],['d','Q','f','A']],[['Z','s'],['l','A','n','M'],['d','Q','f','A']]).
q([['u','I','y','o'],['Z','b','c'],['k','r','s','Z']],[['u','I','y','o'],['Z','b'],['k','r','s','Z']]).
q([['L','y','M','U'],['H','s','i','y']],[['L','y','M'],['H','s','i','y']]).
q([['y','z','Y'],['a','P','v']],[['y','z'],['a','P','v']]).
q([['L','m','S','C'],['s','J','K','A'],['N','V','x','O']],[['L','m','S','C'],['s','J','K'],['N','V','x','O']]).
