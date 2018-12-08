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
p([['V','V','L'],['S','t','q','V']],[['V','V'],['S','t','q']]).
p([['z','M','R'],['p','E','U'],['e','C','r']],[['z','M'],['p','E'],['e','C']]).
p([['e','s','L','w'],['L','k','z','y'],['T','c','n','r']],[['e','s','L'],['L','k','z'],['T','c','n']]).
p([['m','F','N','E'],['H','m','W'],['f','y','C'],['f','j','E','s']],[['m','F','N'],['H','m'],['f','y'],['f','j','E']]).
p([['i','z','o'],['h','U','x'],['t','T','s','C'],['m','Y','u']],[['i','z'],['h','U'],['t','T','s'],['m','Y']]).
q([['o','R','m'],['o','M','B','F'],['f','b','D','a'],['n','R','O','W']],[['o','R','m'],['o','M','B','F'],['f','b','D'],['n','R','O']]).
q([['j','c','Q','r'],['q','H','u'],['F','N','l']],[['j','c','Q','r'],['q','H','u'],['F','N']]).
q([['x','D','o'],['Q','k','e']],[['x','D'],['Q','k','e']]).
q([['x','M','H','I'],['m','i','H'],['w','O','M','P']],[['x','M','H','I'],['m','i','H'],['w','O','M']]).
q([['H','d','V'],['W','G','n']],[['H','d','V'],['W','G']]).
