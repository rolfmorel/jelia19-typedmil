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

my_toupper3(A,B):-upcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_toupper3,[char,char]).
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
p([['g','T','h','G'],['L','i','I'],['B','C','l'],['l','w','A']],[['g','T','h'],['L','i'],['B','C'],['l','w']]).
p([['X','k','X'],['K','I','s','e'],['E','z','H','J'],['k','N','k']],[['X','k'],['K','I','s'],['E','z','H'],['k','N']]).
p([['K','M','O'],['T','L','L'],['x','Y','Y','a'],['m','m','z','X']],[['K','M'],['T','L'],['x','Y','Y'],['m','m','z']]).
p([['k','D','V'],['v','W','p','X'],['W','B','B'],['m','g','O']],[['k','D'],['v','W','p'],['W','B'],['m','g']]).
p([['Z','q','s'],['W','X','F','r'],['a','q','a']],[['Z','q'],['W','X','F'],['a','q']]).
q([['c','z','W','v'],['r','o','o']],[['c','z','W'],['r','o','o']]).
q([['p','x','F'],['g','N','O','M'],['N','l','z','M'],['c','s','l','X']],[['p','x','F'],['g','N','O'],['N','l','z','M'],['c','s','l','X']]).
q([['m','q','m','v'],['q','S','x'],['s','X','d']],[['m','q','m'],['q','S','x'],['s','X','d']]).
q([['j','N','d','E'],['o','G','m','h'],['u','k','G'],['H','g','H']],[['j','N','d'],['o','G','m'],['u','k','G'],['H','g','H']]).
q([['J','N','z','Z'],['m','P','p','V'],['v','s','Q','R'],['i','e','s']],[['J','N','z','Z'],['m','P','p'],['v','s','Q'],['i','e','s']]).
