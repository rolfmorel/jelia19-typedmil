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

my_max_list3(A,B):-max_list(A,B).
my_sumlist4(A,B):-sumlist(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_max_list3,[list(int),int]).
prim(my_sumlist4,[list(int),int]).
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
p([['q','D','C','L'],['O','V','C','t'],['I','u','y']],[['q','D','C'],['O','V','C'],['I','u']]).
p([['g','n','H','l'],['k','G','G','A'],['H','n','k','G'],['v','R','z','G']],[['g','n','H'],['k','G','G'],['H','n','k'],['v','R','z']]).
p([['b','E','X','J'],['e','M','b']],[['b','E','X'],['e','M']]).
p([['O','S','i','t'],['E','F','U']],[['O','S','i'],['E','F']]).
p([['U','Y','U','J'],['k','i','O']],[['U','Y','U'],['k','i']]).
q([['R','J','T','G'],['H','i','h'],['l','e','a','H']],[['R','J','T'],['H','i','h'],['l','e','a','H']]).
q([['M','o','h','X'],['x','y','w'],['V','S','U','a']],[['M','o','h','X'],['x','y'],['V','S','U','a']]).
q([['R','k','C'],['f','z','S'],['L','G','w','o']],[['R','k'],['f','z','S'],['L','G','w','o']]).
q([['N','b','j','J'],['M','D','F','K']],[['N','b','j'],['M','D','F','K']]).
q([['Q','c','s','a'],['M','x','k','d']],[['Q','c','s','a'],['M','x','k']]).
