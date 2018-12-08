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
p([['d','B','E','B'],['u','C','A'],['i','m','I','k']],[['d','B','E'],['u','C'],['i','m','I']]).
p([['N','i','r','Q'],['W','G','x','m']],[['N','i','r'],['W','G','x']]).
p([['r','I','c'],['O','x','J','n'],['E','V','p']],[['r','I'],['O','x','J'],['E','V']]).
p([['P','F','Z'],['t','R','k','P'],['S','I','N']],[['P','F'],['t','R','k'],['S','I']]).
p([['S','m','I','q'],['j','I','I'],['P','t','Q','O'],['z','W','H']],[['S','m','I'],['j','I'],['P','t','Q'],['z','W']]).
q([['j','t','O','F'],['a','L','Z','D'],['v','Z','w'],['E','D','A']],[['j','t','O','F'],['a','L','Z'],['v','Z','w'],['E','D','A']]).
q([['V','t','v'],['W','n','O'],['v','m','e'],['V','I','N']],[['V','t','v'],['W','n','O'],['v','m'],['V','I','N']]).
q([['p','b','W','e'],['C','u','A','Q']],[['p','b','W'],['C','u','A','Q']]).
q([['f','h','T'],['y','y','X'],['a','z','o','C']],[['f','h'],['y','y','X'],['a','z','o','C']]).
q([['V','c','I'],['d','U','N','R'],['w','Y','V','T']],[['V','c'],['d','U','N','R'],['w','Y','V','T']]).
