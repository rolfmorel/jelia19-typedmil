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

my_tolower4(A,B):-downcase_atom(A,B).
my_set5(A):-list_to_set(A,A).
my_flatten6(A,B):-flatten(A,B).
my_min_list7(A,B):-min_list(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_list_to_set9(A,B):-list_to_set(A,B).
my_sumlist10(A,B):-sumlist(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_tolower4/2).
prim(my_set5/1).
prim(my_flatten6/2).
prim(my_min_list7/2).
prim(my_succ8/2).
prim(my_list_to_set9/2).
prim(my_sumlist10/2).
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
p([['f','l','z','i'],['Z','G','j','E']],[['f','l','z'],['Z','G','j']]).
p([['u','F','x'],['A','d','I','L'],['B','x','U','d']],[['u','F'],['A','d','I'],['B','x','U']]).
p([['Y','B','Z'],['f','t','c'],['Z','v','W'],['H','q','X','g']],[['Y','B'],['f','t'],['Z','v'],['H','q','X']]).
p([['d','L','O','d'],['K','f','W'],['K','T','B','F'],['Q','v','a','c']],[['d','L','O'],['K','f'],['K','T','B'],['Q','v','a']]).
p([['A','n','n','D'],['r','b','a']],[['A','n','n'],['r','b']]).
q([['R','m','O'],['Y','W','N','k'],['R','s','C','d']],[['R','m','O'],['Y','W','N','k'],['R','s','C']]).
q([['h','O','U'],['r','h','W','E']],[['h','O'],['r','h','W','E']]).
q([['T','A','x','d'],['V','A','i']],[['T','A','x','d'],['V','A']]).
q([['e','m','P'],['m','X','q']],[['e','m','P'],['m','X']]).
q([['u','c','B'],['C','O','g','a']],[['u','c','B'],['C','O','g']]).
