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
p([['U','k','z','y'],['i','A','K','E'],['C','q','R']],[['U','k','z'],['i','A','K'],['C','q']]).
p([['E','V','o'],['z','q','A','D'],['w','z','Y']],[['E','V'],['z','q','A'],['w','z']]).
p([['x','w','i','O'],['b','R','W','j']],[['x','w','i'],['b','R','W']]).
p([['X','V','J'],['R','C','J','F'],['c','n','f','E'],['R','c','w','e']],[['X','V'],['R','C','J'],['c','n','f'],['R','c','w']]).
p([['F','D','M'],['z','g','J'],['o','L','h','L'],['g','Z','O']],[['F','D'],['z','g'],['o','L','h'],['g','Z']]).
q([['Q','Y','u','t'],['F','Z','q','m']],[['Q','Y','u','t'],['F','Z','q']]).
q([['c','Q','c'],['R','y','F'],['g','N','F','M']],[['c','Q'],['R','y','F'],['g','N','F','M']]).
q([['l','S','P'],['c','h','B','P'],['x','c','r']],[['l','S','P'],['c','h','B'],['x','c','r']]).
q([['f','M','r'],['Y','E','Y']],[['f','M'],['Y','E','Y']]).
q([['s','O','u'],['o','b','p','m'],['J','a','F']],[['s','O','u'],['o','b','p','m'],['J','a']]).
