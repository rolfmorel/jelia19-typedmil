:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
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
my_flatten4(A,B):-flatten(A,B).
my_head5([H|_],H).
my_lowercase6(A):-downcase_atom(A,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_toupper3/2).
prim(my_flatten4/2).
prim(my_head5/2).
prim(my_lowercase6/1).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([['P','v','r','C'],['q','h','k'],['F','O','a','K'],['z','m','X','A']],[['P','v','r'],['q','h'],['F','O','a'],['z','m','X']]).
p([['Z','w','Y','V'],['g','W','D']],[['Z','w','Y'],['g','W']]).
p([['D','z','K','E'],['E','Z','F','H']],[['D','z','K'],['E','Z','F']]).
p([['y','x','n'],['u','b','p'],['T','C','Q','t']],[['y','x'],['u','b'],['T','C','Q']]).
p([['D','p','H','H'],['U','f','E','V']],[['D','p','H'],['U','f','E']]).
q([['n','v','T'],['R','m','s','r'],['u','f','E','T'],['G','Q','u']],[['n','v'],['R','m','s'],['u','f','E','T'],['G','Q','u']]).
q([['D','n','o'],['H','S','T'],['g','I','u']],[['D','n','o'],['H','S'],['g','I','u']]).
q([['x','h','V'],['h','l','P','M'],['v','g','W','B'],['g','z','m']],[['x','h','V'],['h','l','P'],['v','g','W','B'],['g','z','m']]).
q([['O','L','d'],['u','a','X'],['Z','B','q','h'],['Q','P','P','Y']],[['O','L'],['u','a'],['Z','B','q','h'],['Q','P','P','Y']]).
q([['c','O','k','z'],['M','S','W']],[['c','O','k'],['M','S','W']]).
