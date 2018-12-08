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

my_msort3(A,B):-msort(A,B).
my_last4(A,B):-last(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_msort3/2).
prim(my_last4/2).
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
p([['x','g','l','b'],['h','q','x'],['y','C','t']],[['x','g','l'],['h','q'],['y','C']]).
p([['K','g','a'],['c','c','f'],['P','d','L','g'],['h','a','j']],[['K','g'],['c','c'],['P','d','L'],['h','a']]).
p([['Y','J','s'],['D','V','p'],['V','h','l','q'],['p','Q','z']],[['Y','J'],['D','V'],['V','h','l'],['p','Q']]).
p([['c','U','m'],['x','W','B']],[['c','U'],['x','W']]).
p([['v','I','d','X'],['o','z','z'],['y','N','O'],['S','r','H','i']],[['v','I','d'],['o','z'],['y','N'],['S','r','H']]).
q([['E','K','o'],['W','h','Y']],[['E','K','o'],['W','h']]).
q([['W','q','g'],['J','m','C']],[['W','q','g'],['J','m']]).
q([['N','B','X'],['D','u','e','z'],['Y','c','d','a'],['k','G','H','z']],[['N','B'],['D','u','e','z'],['Y','c','d','a'],['k','G','H','z']]).
q([['l','d','m','j'],['o','x','z','X'],['g','m','n'],['n','c','W','F']],[['l','d','m'],['o','x','z','X'],['g','m','n'],['n','c','W']]).
q([['N','n','R','z'],['Z','u','T','B']],[['N','n','R'],['Z','u','T','B']]).
