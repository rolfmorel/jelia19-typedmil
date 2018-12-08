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
