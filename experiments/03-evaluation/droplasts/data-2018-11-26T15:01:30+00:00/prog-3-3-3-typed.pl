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

my_msort3(A,B):-msort(A,B).
my_odd4(A):-1 is A mod 2.
my_sumlist5(A,B):-sumlist(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_msort3,[list(int),list(int)]).
prim(my_odd4,[int]).
prim(my_sumlist5,[list(int),int]).
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
p([['r','L','C'],['X','q','R','O'],['n','Z','c','A'],['m','M','B','L']],[['r','L'],['X','q','R'],['n','Z','c'],['m','M','B']]).
p([['Z','H','i'],['f','b','n','L'],['Q','w','j','N'],['M','p','N','j']],[['Z','H'],['f','b','n'],['Q','w','j'],['M','p','N']]).
p([['L','y','x'],['x','e','z','o'],['n','b','M','k'],['h','B','r']],[['L','y'],['x','e','z'],['n','b','M'],['h','B']]).
p([['s','b','G'],['w','d','W'],['x','A','g','h'],['g','I','X','n']],[['s','b'],['w','d'],['x','A','g'],['g','I','X']]).
p([['S','c','X','P'],['r','q','E','y'],['T','e','c','r']],[['S','c','X'],['r','q','E'],['T','e','c']]).
q([['q','V','z'],['h','S','t','w']],[['q','V','z'],['h','S','t']]).
q([['d','A','h','V'],['H','f','M'],['a','c','T'],['C','n','p','o']],[['d','A','h','V'],['H','f','M'],['a','c','T'],['C','n','p']]).
q([['s','U','q','W'],['u','Q','i']],[['s','U','q'],['u','Q','i']]).
q([['C','N','f'],['M','e','o','I']],[['C','N','f'],['M','e','o']]).
q([['l','D','Y'],['X','M','L']],[['l','D','Y'],['X','M']]).
