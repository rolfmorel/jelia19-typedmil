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
p([['q','X','H','R'],['V','O','m','p'],['E','U','j'],['L','v','h']],[['q','X','H'],['V','O','m'],['E','U'],['L','v']]).
p([['r','f','z'],['V','d','S','y'],['R','I','C','x'],['y','m','e']],[['r','f'],['V','d','S'],['R','I','C'],['y','m']]).
p([['I','X','t'],['P','S','R','J'],['l','X','b']],[['I','X'],['P','S','R'],['l','X']]).
p([['V','L','B'],['y','X','n','L'],['F','z','V'],['W','d','o']],[['V','L'],['y','X','n'],['F','z'],['W','d']]).
p([['P','y','f','R'],['g','J','t']],[['P','y','f'],['g','J']]).
q([['Q','G','Q','w'],['M','c','H'],['d','X','T','i'],['V','x','i']],[['Q','G','Q'],['M','c'],['d','X','T','i'],['V','x','i']]).
q([['H','i','D','Y'],['f','p','e','O'],['C','i','i','O'],['C','L','P','x']],[['H','i','D','Y'],['f','p','e','O'],['C','i','i','O'],['C','L','P']]).
q([['U','x','x','u'],['U','q','d','V'],['I','q','q','T']],[['U','x','x','u'],['U','q','d'],['I','q','q','T']]).
q([['M','o','w'],['X','q','V']],[['M','o'],['X','q','V']]).
q([['y','d','c'],['q','V','U']],[['y','d'],['q','V','U']]).
