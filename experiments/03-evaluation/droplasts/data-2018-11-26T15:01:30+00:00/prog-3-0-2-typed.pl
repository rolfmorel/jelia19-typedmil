:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
my_tail0([_|TL],TL).
my_reverse1(A,B):-reverse(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(list(char)),list(list(char))],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([['a','Y','k','Q'],['c','C','p'],['O','a','p'],['e','o','o','L']],[['a','Y','k'],['c','C'],['O','a'],['e','o','o']]).
p([['S','u','O','Y'],['G','S','K','R'],['Q','T','j','O']],[['S','u','O'],['G','S','K'],['Q','T','j']]).
p([['W','t','R'],['p','o','f'],['S','J','S']],[['W','t'],['p','o'],['S','J']]).
p([['D','H','q'],['M','M','U']],[['D','H'],['M','M']]).
p([['u','N','e','C'],['j','G','L']],[['u','N','e'],['j','G']]).
q([['r','F','q','X'],['C','K','V'],['m','k','r']],[['r','F','q','X'],['C','K','V'],['m','k']]).
q([['o','W','p'],['P','k','g','I'],['w','W','U','s'],['a','p','g']],[['o','W','p'],['P','k','g','I'],['w','W','U'],['a','p','g']]).
q([['Z','P','A'],['V','Y','S','J'],['Z','s','c','Q'],['m','K','m','n']],[['Z','P','A'],['V','Y','S','J'],['Z','s','c','Q'],['m','K','m']]).
q([['u','k','e','A'],['G','d','T'],['u','c','x','r']],[['u','k','e'],['G','d','T'],['u','c','x','r']]).
q([['X','W','e','A'],['D','C','O'],['Z','Y','P','J']],[['X','W','e'],['D','C','O'],['Z','Y','P','J']]).
