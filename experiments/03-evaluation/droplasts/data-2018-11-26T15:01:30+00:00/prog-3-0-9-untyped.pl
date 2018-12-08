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
p([['v','V','J'],['n','e','S'],['k','W','n','o'],['m','d','P']],[['v','V'],['n','e'],['k','W','n'],['m','d']]).
p([['n','I','S','Q'],['y','d','l'],['f','Z','X','L'],['f','F','O']],[['n','I','S'],['y','d'],['f','Z','X'],['f','F']]).
p([['v','b','b'],['a','W','P'],['E','e','B'],['X','e','v','k']],[['v','b'],['a','W'],['E','e'],['X','e','v']]).
p([['W','x','o','u'],['n','u','D','D']],[['W','x','o'],['n','u','D']]).
p([['q','a','m','U'],['G','X','h','J']],[['q','a','m'],['G','X','h']]).
q([['S','t','g'],['o','h','L','h'],['S','f','b']],[['S','t'],['o','h','L','h'],['S','f','b']]).
q([['Z','X','P','R'],['X','D','E','T'],['z','H','u','J'],['K','Y','t']],[['Z','X','P','R'],['X','D','E','T'],['z','H','u'],['K','Y','t']]).
q([['x','U','R'],['L','I','h']],[['x','U'],['L','I','h']]).
q([['K','T','D'],['g','j','l'],['J','k','x']],[['K','T'],['g','j','l'],['J','k','x']]).
q([['b','a','M'],['K','T','q'],['W','Z','C']],[['b','a','M'],['K','T','q'],['W','Z']]).
