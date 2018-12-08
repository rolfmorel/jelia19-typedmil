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

my_len3(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_len3,[list(_),int]).
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
p([['Q','G','L'],['Z','N','F'],['m','j','f','w'],['y','u','F']],[['Q','G'],['Z','N'],['m','j','f'],['y','u']]).
p([['z','d','i'],['C','f','I']],[['z','d'],['C','f']]).
p([['T','a','q'],['S','B','Y','u'],['b','k','d'],['L','m','X','G']],[['T','a'],['S','B','Y'],['b','k'],['L','m','X']]).
p([['N','a','z'],['z','e','Y','S'],['N','j','R'],['r','U','s']],[['N','a'],['z','e','Y'],['N','j'],['r','U']]).
p([['J','W','I','K'],['K','Q','Y'],['L','q','y','L'],['F','B','Y']],[['J','W','I'],['K','Q'],['L','q','y'],['F','B']]).
q([['k','c','j'],['D','u','G'],['Z','N','Q','l'],['L','q','L','H']],[['k','c','j'],['D','u','G'],['Z','N','Q','l'],['L','q','L']]).
q([['C','x','s','u'],['X','l','h'],['s','D','l']],[['C','x','s'],['X','l','h'],['s','D','l']]).
q([['K','H','J'],['e','N','M'],['y','b','U']],[['K','H','J'],['e','N','M'],['y','b']]).
q([['L','q','d','H'],['S','y','e','X'],['F','n','y'],['k','B','O']],[['L','q','d','H'],['S','y','e','X'],['F','n','y'],['k','B']]).
q([['I','U','M','E'],['D','E','X','j'],['b','O','G','A'],['s','k','y']],[['I','U','M','E'],['D','E','X'],['b','O','G','A'],['s','k','y']]).
