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


filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).

my_flatten4(A,B):-flatten(A,B).
my_max_list5(A,B):-max_list(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_set7(A):-list_to_set(A,A).
my_succ8(A,B):-succ(A,B),B =< 10.
my_element9(A,B):-member(B,A).
my_list_to_set10(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_flatten4/2).
prim(my_max_list5/2).
prim(my_lowercase6/1).
prim(my_set7/1).
prim(my_succ8/2).
prim(my_element9/2).
prim(my_list_to_set10/2).
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
p([['O','F','z','n'],['S','C','r'],['r','Q','j','Z'],['v','W','L','t']],[['O','F','z'],['S','C'],['r','Q','j'],['v','W','L']]).
p([['i','f','f'],['e','G','d','C']],[['i','f'],['e','G','d']]).
p([['v','n','r'],['c','F','X','s'],['U','J','P'],['X','Z','a']],[['v','n'],['c','F','X'],['U','J'],['X','Z']]).
p([['X','J','x','j'],['c','g','f'],['d','N','w'],['p','o','Y']],[['X','J','x'],['c','g'],['d','N'],['p','o']]).
p([['l','b','z','w'],['G','j','c'],['Q','x','f'],['c','g','r']],[['l','b','z'],['G','j'],['Q','x'],['c','g']]).
q([['u','r','F','a'],['B','V','t','P']],[['u','r','F'],['B','V','t','P']]).
q([['K','T','V','Z'],['m','h','j']],[['K','T','V','Z'],['m','h']]).
q([['a','f','h'],['O','C','K','C'],['u','w','j','q']],[['a','f','h'],['O','C','K','C'],['u','w','j']]).
q([['Q','H','M'],['N','X','r','H']],[['Q','H'],['N','X','r','H']]).
q([['W','y','H'],['m','g','l','W'],['c','H','U','m']],[['W','y','H'],['m','g','l','W'],['c','H','U']]).
