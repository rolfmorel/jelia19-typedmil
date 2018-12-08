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

my_last3(A,B):-last(A,B).
my_min_list4(A,B):-min_list(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_last3/2).
prim(my_min_list4/2).
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
p([['B','h','g'],['I','r','g'],['q','g','u','o']],[['B','h'],['I','r'],['q','g','u']]).
p([['A','m','j'],['D','h','h','p'],['J','a','x','C'],['b','n','R']],[['A','m'],['D','h','h'],['J','a','x'],['b','n']]).
p([['f','e','c','f'],['J','V','p']],[['f','e','c'],['J','V']]).
p([['x','Z','u'],['m','R','U'],['N','s','q','G'],['h','D','x','A']],[['x','Z'],['m','R'],['N','s','q'],['h','D','x']]).
p([['x','S','k'],['V','o','k'],['T','S','x','E']],[['x','S'],['V','o'],['T','S','x']]).
q([['w','d','y'],['M','B','O']],[['w','d','y'],['M','B']]).
q([['g','U','G','u'],['N','i','X'],['f','M','B']],[['g','U','G','u'],['N','i'],['f','M','B']]).
q([['X','X','x'],['f','f','C','m'],['C','S','Y','X']],[['X','X','x'],['f','f','C','m'],['C','S','Y']]).
q([['C','w','S'],['O','D','I'],['O','B','F'],['c','f','D']],[['C','w'],['O','D','I'],['O','B'],['c','f','D']]).
q([['Z','Z','u'],['j','H','S'],['k','A','V'],['C','m','U','O']],[['Z','Z','u'],['j','H','S'],['k','A'],['C','m','U']]).
