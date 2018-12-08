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

my_head3([H|_],H).
my_min_list4(A,B):-min_list(A,B).
my_flatten5(A,B):-flatten(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_uppercase7(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_head3/2).
prim(my_min_list4/2).
prim(my_flatten5/2).
prim(my_sumlist6/2).
prim(my_uppercase7/1).
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
p([['k','j','x','S'],['z','U','I']],[['k','j','x'],['z','U']]).
p([['x','E','t','S'],['d','t','C','Y'],['b','K','b','f'],['o','G','S','U']],[['x','E','t'],['d','t','C'],['b','K','b'],['o','G','S']]).
p([['W','j','d'],['N','h','M'],['t','u','V','b'],['S','b','A']],[['W','j'],['N','h'],['t','u','V'],['S','b']]).
p([['p','D','s'],['Y','n','l'],['O','w','Y','A']],[['p','D'],['Y','n'],['O','w','Y']]).
p([['B','V','i'],['P','W','j'],['J','h','y'],['j','z','b']],[['B','V'],['P','W'],['J','h'],['j','z']]).
q([['n','X','U','L'],['l','l','r','k'],['X','t','E'],['T','P','I']],[['n','X','U','L'],['l','l','r','k'],['X','t'],['T','P','I']]).
q([['J','n','z'],['M','V','a','q']],[['J','n'],['M','V','a','q']]).
q([['E','k','e','B'],['x','Z','r'],['N','k','t'],['j','W','O']],[['E','k','e','B'],['x','Z','r'],['N','k'],['j','W','O']]).
q([['R','L','G'],['L','y','E']],[['R','L','G'],['L','y']]).
q([['w','w','x'],['K','G','J','e'],['D','S','h','m'],['A','d','p','E']],[['w','w','x'],['K','G','J'],['D','S','h','m'],['A','d','p','E']]).
