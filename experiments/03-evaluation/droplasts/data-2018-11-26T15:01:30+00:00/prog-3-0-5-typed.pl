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
p([['B','A','T','n'],['t','q','L','M'],['E','O','A','m'],['v','w','z']],[['B','A','T'],['t','q','L'],['E','O','A'],['v','w']]).
p([['s','L','f'],['T','b','k'],['E','E','Y','G']],[['s','L'],['T','b'],['E','E','Y']]).
p([['q','K','G'],['Y','J','O','i'],['X','w','q'],['m','i','Z','t']],[['q','K'],['Y','J','O'],['X','w'],['m','i','Z']]).
p([['Y','q','t','R'],['K','N','K']],[['Y','q','t'],['K','N']]).
p([['l','c','o'],['w','K','v','r'],['w','c','y'],['x','N','z','T']],[['l','c'],['w','K','v'],['w','c'],['x','N','z']]).
q([['I','D','F','z'],['w','M','d'],['y','t','a','o']],[['I','D','F'],['w','M','d'],['y','t','a','o']]).
q([['a','a','v','Y'],['n','q','O','D'],['D','x','C','z'],['B','J','C','p']],[['a','a','v','Y'],['n','q','O','D'],['D','x','C'],['B','J','C','p']]).
q([['e','W','o','o'],['M','i','W'],['E','y','L'],['Y','K','i']],[['e','W','o','o'],['M','i','W'],['E','y'],['Y','K','i']]).
q([['z','W','A'],['d','b','b','L'],['j','K','n'],['p','v','t','g']],[['z','W'],['d','b','b','L'],['j','K','n'],['p','v','t']]).
q([['X','o','O'],['Z','i','m'],['v','a','E','d'],['J','h','H','x']],[['X','o'],['Z','i','m'],['v','a','E','d'],['J','h','H','x']]).
