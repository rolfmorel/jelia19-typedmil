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

my_flatten3(A,B):-flatten(A,B).
my_max_list4(A,B):-max_list(A,B).
my_element5(A,B):-member(B,A).
my_min_list6(A,B):-min_list(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_flatten3/2).
prim(my_max_list4/2).
prim(my_element5/2).
prim(my_min_list6/2).
prim(my_double7/2).
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
p([['d','C','U'],['s','J','f','o'],['X','E','U','c']],[['d','C'],['s','J','f'],['X','E','U']]).
p([['b','p','S'],['j','e','v'],['l','B','C','T'],['s','c','a']],[['b','p'],['j','e'],['l','B','C'],['s','c']]).
p([['b','m','m','T'],['I','d','X','P'],['M','B','b','t']],[['b','m','m'],['I','d','X'],['M','B','b']]).
p([['a','P','V','k'],['f','A','O']],[['a','P','V'],['f','A']]).
p([['T','l','R'],['I','O','j']],[['T','l'],['I','O']]).
q([['f','a','j','k'],['q','X','u','Z']],[['f','a','j','k'],['q','X','u']]).
q([['E','O','r','U'],['r','d','z'],['r','U','a','X'],['Q','U','d','W']],[['E','O','r','U'],['r','d','z'],['r','U','a'],['Q','U','d','W']]).
q([['u','M','P','F'],['r','L','D','M']],[['u','M','P'],['r','L','D','M']]).
q([['c','J','h'],['E','r','x','b'],['I','U','g']],[['c','J'],['E','r','x','b'],['I','U','g']]).
q([['B','H','o','U'],['V','d','p','p'],['K','i','g'],['Z','E','F','Y']],[['B','H','o','U'],['V','d','p','p'],['K','i','g'],['Z','E','F']]).
