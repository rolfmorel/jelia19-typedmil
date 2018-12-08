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

my_succ3(A,B):-succ(A,B),B =< 10.
my_len4(A,B):-length(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_pred6(A,B):-succ(B,A),A > 0.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_succ3/2).
prim(my_len4/2).
prim(my_double5/2).
prim(my_pred6/2).
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
p([['L','X','F'],['s','B','o']],[['L','X'],['s','B']]).
p([['A','N','t'],['D','b','C','K'],['I','q','e','b'],['p','r','I']],[['A','N'],['D','b','C'],['I','q','e'],['p','r']]).
p([['F','P','d','X'],['p','g','T'],['k','p','L']],[['F','P','d'],['p','g'],['k','p']]).
p([['r','S','M'],['X','a','w']],[['r','S'],['X','a']]).
p([['z','v','a','D'],['Q','n','E','O'],['d','c','s']],[['z','v','a'],['Q','n','E'],['d','c']]).
q([['T','h','Q','E'],['Q','h','j'],['p','M','z','j']],[['T','h','Q','E'],['Q','h','j'],['p','M','z']]).
q([['G','L','O'],['b','M','P'],['r','d','X','d'],['A','j','b','b']],[['G','L','O'],['b','M','P'],['r','d','X'],['A','j','b','b']]).
q([['F','y','r','d'],['p','L','f']],[['F','y','r','d'],['p','L']]).
q([['a','t','u'],['K','w','x'],['Q','x','V','Q'],['F','q','x']],[['a','t','u'],['K','w','x'],['Q','x','V','Q'],['F','q']]).
q([['V','D','z','I'],['O','H','R'],['r','e','N']],[['V','D','z'],['O','H','R'],['r','e','N']]).
