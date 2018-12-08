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

my_odd3(A):-1 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_odd3,[int]).
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
p([['s','c','p'],['A','z','M','Q'],['E','K','L','N'],['k','y','r','L']],[['s','c'],['A','z','M'],['E','K','L'],['k','y','r']]).
p([['y','z','J'],['D','E','U'],['y','k','O','C'],['G','S','p','I']],[['y','z'],['D','E'],['y','k','O'],['G','S','p']]).
p([['L','u','R','w'],['M','B','r'],['d','X','T'],['e','x','P']],[['L','u','R'],['M','B'],['d','X'],['e','x']]).
p([['S','k','l','J'],['f','N','Z']],[['S','k','l'],['f','N']]).
p([['o','o','P','j'],['x','I','o','t']],[['o','o','P'],['x','I','o']]).
q([['V','t','t','H'],['x','w','O']],[['V','t','t'],['x','w','O']]).
q([['S','Q','T'],['X','Y','u']],[['S','Q','T'],['X','Y']]).
q([['m','t','f'],['L','R','h']],[['m','t','f'],['L','R']]).
q([['p','q','J'],['D','a','H'],['R','c','p','B']],[['p','q','J'],['D','a'],['R','c','p','B']]).
q([['b','Y','i','n'],['d','e','f']],[['b','Y','i'],['d','e','f']]).
