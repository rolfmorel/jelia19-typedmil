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
my_tolower4(A,B):-downcase_atom(A,B).
my_max_list5(A,B):-max_list(A,B).
my_toupper6(A,B):-upcase_atom(A,B).
my_last7(A,B):-last(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_head3/2).
prim(my_tolower4/2).
prim(my_max_list5/2).
prim(my_toupper6/2).
prim(my_last7/2).
prim(my_list_to_set8/2).
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
p([['B','q','b','W'],['F','o','M','X'],['B','x','r']],[['B','q','b'],['F','o','M'],['B','x']]).
p([['L','u','x','h'],['v','z','H']],[['L','u','x'],['v','z']]).
p([['s','Z','d'],['P','k','W']],[['s','Z'],['P','k']]).
p([['G','j','v','s'],['l','f','z','E'],['r','Z','l'],['h','h','Q','I']],[['G','j','v'],['l','f','z'],['r','Z'],['h','h','Q']]).
p([['b','W','D'],['t','J','x','t']],[['b','W'],['t','J','x']]).
q([['Y','g','L','s'],['x','p','X','l'],['p','H','J']],[['Y','g','L','s'],['x','p','X','l'],['p','H']]).
q([['Q','c','W'],['g','G','K'],['W','N','O','K'],['v','N','V']],[['Q','c','W'],['g','G','K'],['W','N','O','K'],['v','N']]).
q([['n','Y','b'],['b','E','H'],['B','W','y'],['l','I','M']],[['n','Y','b'],['b','E'],['B','W','y'],['l','I','M']]).
q([['V','U','K'],['Y','e','o'],['y','P','H'],['T','u','q','X']],[['V','U','K'],['Y','e'],['y','P'],['T','u','q','X']]).
q([['B','w','U','U'],['J','p','F','J'],['q','X','B','O'],['M','X','x','Y']],[['B','w','U','U'],['J','p','F'],['q','X','B'],['M','X','x','Y']]).
