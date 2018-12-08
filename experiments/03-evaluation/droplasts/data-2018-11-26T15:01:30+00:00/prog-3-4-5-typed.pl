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
my_double4(N,M):-M is 2*N,M =< 10.
my_even5(A):-0 is A mod 2.
my_odd6(A):-1 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_max_list3,[list(int),int]).
prim(my_double4,[int,int]).
prim(my_even5,[int]).
prim(my_odd6,[int]).
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
p([['f','N','v'],['z','B','W','G'],['B','x','O','w'],['Z','h','n']],[['f','N'],['z','B','W'],['B','x','O'],['Z','h']]).
p([['O','z','e'],['c','K','k'],['i','C','M'],['D','W','M','Z']],[['O','z'],['c','K'],['i','C'],['D','W','M']]).
p([['I','d','P'],['q','C','d','I']],[['I','d'],['q','C','d']]).
p([['T','j','A','h'],['S','i','S','F'],['g','Z','j','W']],[['T','j','A'],['S','i','S'],['g','Z','j']]).
p([['s','H','l'],['A','I','L'],['J','P','C','J']],[['s','H'],['A','I'],['J','P','C']]).
q([['K','u','k'],['X','w','t'],['f','M','g']],[['K','u','k'],['X','w','t'],['f','M']]).
q([['B','Q','z'],['b','y','b','X']],[['B','Q'],['b','y','b','X']]).
q([['v','i','s','S'],['L','s','s']],[['v','i','s'],['L','s','s']]).
q([['V','U','G','a'],['M','b','X'],['A','N','U'],['H','X','z','E']],[['V','U','G','a'],['M','b'],['A','N','U'],['H','X','z','E']]).
q([['P','C','U'],['Z','I','g'],['Z','t','H'],['z','V','C','L']],[['P','C','U'],['Z','I','g'],['Z','t','H'],['z','V','C']]).
