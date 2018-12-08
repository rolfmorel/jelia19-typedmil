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

my_list_to_set3(A,B):-list_to_set(A,B).
my_succ4(A,B):-succ(A,B),B =< 10.
my_flatten5(A,B):-flatten(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_succ4,[int,int]).
prim(my_flatten5,[list(list(T)),list(T)]).
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
p([['m','E','U','C'],['l','i','R'],['E','E','T','T'],['Q','d','y']],[['m','E','U'],['l','i'],['E','E','T'],['Q','d']]).
p([['e','v','j'],['p','O','Q']],[['e','v'],['p','O']]).
p([['o','a','B','F'],['U','M','B'],['U','J','p'],['Z','v','y','e']],[['o','a','B'],['U','M'],['U','J'],['Z','v','y']]).
p([['A','V','q'],['N','d','e','y']],[['A','V'],['N','d','e']]).
p([['x','r','p','x'],['e','k','r']],[['x','r','p'],['e','k']]).
q([['s','L','d'],['C','B','K'],['C','P','r','J'],['h','j','J']],[['s','L','d'],['C','B'],['C','P','r','J'],['h','j']]).
q([['D','d','b','T'],['I','S','z'],['n','p','m','W']],[['D','d','b','T'],['I','S','z'],['n','p','m']]).
q([['Y','Y','O'],['p','f','R','f'],['J','u','e','P'],['y','x','V','q']],[['Y','Y','O'],['p','f','R'],['J','u','e'],['y','x','V','q']]).
q([['A','I','i','Q'],['I','I','L','Y'],['e','w','a','r']],[['A','I','i','Q'],['I','I','L','Y'],['e','w','a']]).
q([['e','Y','q','Y'],['C','v','k','D'],['o','X','R','Q'],['u','Y','j']],[['e','Y','q','Y'],['C','v','k'],['o','X','R','Q'],['u','Y']]).
