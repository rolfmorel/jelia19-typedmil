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
my_sumlist4(A,B):-sumlist(A,B).
my_odd5(A):-1 is A mod 2.
my_double6(N,M):-M is 2*N,M =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_sumlist4,[list(int),int]).
prim(my_odd5,[int]).
prim(my_double6,[int,int]).
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
p([['E','m','t','N'],['q','a','t']],[['E','m','t'],['q','a']]).
p([['a','z','n'],['u','Q','K']],[['a','z'],['u','Q']]).
p([['h','k','x','W'],['q','O','U','D']],[['h','k','x'],['q','O','U']]).
p([['L','P','i'],['F','S','r','d'],['X','t','N']],[['L','P'],['F','S','r'],['X','t']]).
p([['z','y','T'],['m','L','C','w'],['c','D','o','L'],['K','U','L']],[['z','y'],['m','L','C'],['c','D','o'],['K','U']]).
q([['x','A','C'],['h','F','o','w'],['i','W','R','e'],['X','A','X','e']],[['x','A','C'],['h','F','o','w'],['i','W','R'],['X','A','X','e']]).
q([['B','k','T'],['K','s','t','z'],['m','c','G','w'],['S','m','L','N']],[['B','k','T'],['K','s','t'],['m','c','G'],['S','m','L','N']]).
q([['g','G','O'],['R','r','d','g']],[['g','G','O'],['R','r','d']]).
q([['d','T','C'],['T','A','E','N'],['r','g','H','q'],['J','S','g']],[['d','T'],['T','A','E','N'],['r','g','H','q'],['J','S','g']]).
q([['y','n','g','X'],['B','r','U'],['C','A','W']],[['y','n','g'],['B','r','U'],['C','A','W']]).
