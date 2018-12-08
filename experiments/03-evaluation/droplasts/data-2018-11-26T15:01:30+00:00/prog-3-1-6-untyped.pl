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
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_last3/2).
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
p([['D','l','V'],['H','v','l'],['z','X','I']],[['D','l'],['H','v'],['z','X']]).
p([['j','N','H'],['h','V','Z','N'],['I','Q','O'],['y','P','W']],[['j','N'],['h','V','Z'],['I','Q'],['y','P']]).
p([['Z','D','J'],['g','Y','a','j'],['S','G','I','O']],[['Z','D'],['g','Y','a'],['S','G','I']]).
p([['q','Y','W','S'],['C','n','Q'],['q','w','a','y'],['v','t','b','Z']],[['q','Y','W'],['C','n'],['q','w','a'],['v','t','b']]).
p([['J','s','U'],['l','a','k'],['M','G','p'],['b','D','X','I']],[['J','s'],['l','a'],['M','G'],['b','D','X']]).
q([['z','T','L','a'],['h','M','s'],['p','I','m','e'],['A','j','p']],[['z','T','L','a'],['h','M','s'],['p','I','m'],['A','j','p']]).
q([['n','A','d','w'],['a','O','W','W'],['i','Q','Q','h'],['S','j','f']],[['n','A','d','w'],['a','O','W'],['i','Q','Q','h'],['S','j','f']]).
q([['c','V','h'],['a','i','e','d']],[['c','V'],['a','i','e','d']]).
q([['S','B','d'],['z','q','Q'],['F','e','z']],[['S','B','d'],['z','q','Q'],['F','e']]).
q([['D','Q','C','K'],['U','K','L']],[['D','Q','C'],['U','K','L']]).
