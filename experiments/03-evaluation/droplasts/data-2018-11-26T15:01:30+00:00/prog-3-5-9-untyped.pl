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

my_set3(A):-list_to_set(A,A).
my_sumlist4(A,B):-sumlist(A,B).
my_msort5(A,B):-msort(A,B).
my_head6([H|_],H).
my_list_to_set7(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_set3/1).
prim(my_sumlist4/2).
prim(my_msort5/2).
prim(my_head6/2).
prim(my_list_to_set7/2).
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
p([['V','c','A','o'],['t','c','s','a'],['m','O','W']],[['V','c','A'],['t','c','s'],['m','O']]).
p([['J','A','Z','X'],['w','J','q','S']],[['J','A','Z'],['w','J','q']]).
p([['N','x','Y'],['D','U','C']],[['N','x'],['D','U']]).
p([['C','y','x','R'],['W','l','b','S']],[['C','y','x'],['W','l','b']]).
p([['V','Y','e','L'],['n','o','t'],['u','t','F','c']],[['V','Y','e'],['n','o'],['u','t','F']]).
q([['l','a','h'],['N','f','l'],['c','x','N','m']],[['l','a'],['N','f','l'],['c','x','N','m']]).
q([['C','m','h'],['X','w','T','M']],[['C','m','h'],['X','w','T']]).
q([['k','C','k'],['c','o','m','J'],['L','t','g','e']],[['k','C'],['c','o','m','J'],['L','t','g','e']]).
q([['W','w','y'],['X','h','b','g']],[['W','w'],['X','h','b','g']]).
q([['J','E','V'],['H','e','Q'],['Y','O','y'],['H','q','V','e']],[['J','E'],['H','e','Q'],['Y','O','y'],['H','q','V','e']]).
