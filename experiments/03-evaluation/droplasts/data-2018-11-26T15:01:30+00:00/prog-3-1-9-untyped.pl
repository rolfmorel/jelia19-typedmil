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

my_set3(A):-list_to_set(A,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_set3/1).
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
p([['g','D','v'],['T','t','o','t'],['S','a','U','S'],['P','l','T','O']],[['g','D'],['T','t','o'],['S','a','U'],['P','l','T']]).
p([['p','W','i'],['o','e','z','m'],['w','e','A'],['M','U','J','j']],[['p','W'],['o','e','z'],['w','e'],['M','U','J']]).
p([['m','u','u','j'],['R','B','o'],['f','L','X','A']],[['m','u','u'],['R','B'],['f','L','X']]).
p([['I','i','Q'],['z','M','M','e'],['M','N','c','s'],['L','G','d']],[['I','i'],['z','M','M'],['M','N','c'],['L','G']]).
p([['J','A','b','W'],['D','O','K','C'],['j','n','n','a']],[['J','A','b'],['D','O','K'],['j','n','n']]).
q([['o','Q','b'],['I','L','C'],['a','X','G','n'],['q','Y','A']],[['o','Q'],['I','L','C'],['a','X','G','n'],['q','Y']]).
q([['x','z','j'],['W','b','u'],['I','A','a'],['B','Q','Z']],[['x','z','j'],['W','b'],['I','A','a'],['B','Q','Z']]).
q([['q','M','e'],['R','i','n','o']],[['q','M','e'],['R','i','n']]).
q([['k','i','L','p'],['Q','C','K','f']],[['k','i','L','p'],['Q','C','K']]).
q([['l','C','f'],['g','Y','X'],['W','a','w'],['C','K','C']],[['l','C','f'],['g','Y','X'],['W','a','w'],['C','K']]).
