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

my_pred3(A,B):-succ(B,A),A > 0.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_pred3/2).
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
p([['Z','o','b','s'],['o','k','P'],['v','Y','a','K'],['R','m','P','e']],[['Z','o','b'],['o','k'],['v','Y','a'],['R','m','P']]).
p([['I','Y','d'],['p','c','q','X'],['Q','k','M','p']],[['I','Y'],['p','c','q'],['Q','k','M']]).
p([['Q','V','J','w'],['w','V','o'],['r','m','B','t']],[['Q','V','J'],['w','V'],['r','m','B']]).
p([['Q','M','I'],['h','h','u','H']],[['Q','M'],['h','h','u']]).
p([['j','i','M'],['E','j','j','o']],[['j','i'],['E','j','j']]).
q([['I','G','J'],['v','a','Z'],['o','I','z','G']],[['I','G','J'],['v','a'],['o','I','z','G']]).
q([['Y','S','M','U'],['F','h','z']],[['Y','S','M'],['F','h','z']]).
q([['r','E','w'],['r','T','h','k'],['G','Y','J','e']],[['r','E','w'],['r','T','h','k'],['G','Y','J']]).
q([['J','N','p','C'],['a','U','j'],['n','W','X','F'],['e','I','d','T']],[['J','N','p'],['a','U','j'],['n','W','X','F'],['e','I','d','T']]).
q([['o','n','p','I'],['S','e','P','v']],[['o','n','p','I'],['S','e','P']]).
