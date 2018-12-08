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
my_max_list4(A,B):-max_list(A,B).
my_flatten5(A,B):-flatten(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_msort7(A,B):-msort(A,B).
my_even8(A):-0 is A mod 2.
my_pred9(A,B):-succ(B,A),A > 0.
my_uppercase10(A):-upcase_atom(A,A).
my_toupper11(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_last3/2).
prim(my_max_list4/2).
prim(my_flatten5/2).
prim(my_list_to_set6/2).
prim(my_msort7/2).
prim(my_even8/1).
prim(my_pred9/2).
prim(my_uppercase10/1).
prim(my_toupper11/2).
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
p([['e','B','q','f'],['e','A','o','d']],[['e','B','q'],['e','A','o']]).
p([['c','o','A'],['F','P','q'],['a','J','A'],['j','V','L']],[['c','o'],['F','P'],['a','J'],['j','V']]).
p([['u','N','D','J'],['Q','a','Y'],['R','e','x']],[['u','N','D'],['Q','a'],['R','e']]).
p([['Z','E','e','E'],['b','N','I'],['x','T','D'],['D','g','b']],[['Z','E','e'],['b','N'],['x','T'],['D','g']]).
p([['F','B','e'],['t','Q','y'],['r','K','M','H']],[['F','B'],['t','Q'],['r','K','M']]).
q([['Z','y','r','W'],['I','t','w','s'],['n','f','U']],[['Z','y','r'],['I','t','w','s'],['n','f','U']]).
q([['Z','Z','I'],['s','n','Y','u'],['F','X','Z']],[['Z','Z'],['s','n','Y','u'],['F','X','Z']]).
q([['L','G','e','s'],['t','n','z','N']],[['L','G','e','s'],['t','n','z']]).
q([['q','A','y'],['n','t','M','r'],['P','n','N','x'],['M','m','p','i']],[['q','A','y'],['n','t','M'],['P','n','N','x'],['M','m','p']]).
q([['l','A','Z'],['Y','s','T','s']],[['l','A','Z'],['Y','s','T']]).
