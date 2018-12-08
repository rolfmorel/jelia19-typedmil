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
p([['H','o','V','M'],['j','o','V','G'],['p','k','k','y'],['i','Z','M','H']],[['H','o','V'],['j','o','V'],['p','k','k'],['i','Z','M']]).
p([['j','T','c','B'],['a','I','W','q'],['s','u','l'],['I','w','V','A']],[['j','T','c'],['a','I','W'],['s','u'],['I','w','V']]).
p([['W','B','N','o'],['z','i','T','d'],['b','D','w']],[['W','B','N'],['z','i','T'],['b','D']]).
p([['I','t','q','z'],['E','W','O','k'],['c','S','v'],['S','M','N']],[['I','t','q'],['E','W','O'],['c','S'],['S','M']]).
p([['Q','P','W'],['Z','q','P','d']],[['Q','P'],['Z','q','P']]).
q([['B','D','P','W'],['F','V','N','x']],[['B','D','P','W'],['F','V','N']]).
q([['K','V','o','Z'],['U','q','r'],['V','p','z']],[['K','V','o'],['U','q','r'],['V','p','z']]).
q([['H','u','X','A'],['A','N','r'],['x','T','H','l']],[['H','u','X','A'],['A','N','r'],['x','T','H']]).
q([['f','l','p'],['l','X','V','C'],['f','J','A'],['h','y','H']],[['f','l'],['l','X','V','C'],['f','J'],['h','y','H']]).
q([['W','I','C'],['m','e','i','G'],['Y','C','W']],[['W','I'],['m','e','i','G'],['Y','C','W']]).
