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

my_even3(A):-0 is A mod 2.
my_tolower4(A,B):-downcase_atom(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.

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

my_toupper7(A,B):-upcase_atom(A,B).
my_last8(A,B):-last(A,B).
my_min_list9(A,B):-min_list(A,B).
my_sumlist10(A,B):-sumlist(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_even3/1).
prim(my_tolower4/2).
prim(my_succ5/2).
prim(my_toupper7/2).
prim(my_last8/2).
prim(my_min_list9/2).
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
p([['h','C','Z','s'],['v','T','R']],[['h','C','Z'],['v','T']]).
p([['W','x','R','p'],['e','a','Z','d'],['J','Z','R','d']],[['W','x','R'],['e','a','Z'],['J','Z','R']]).
p([['T','Y','R'],['J','B','Z'],['n','m','E'],['Y','M','u','r']],[['T','Y'],['J','B'],['n','m'],['Y','M','u']]).
p([['S','Z','k'],['G','S','a','T']],[['S','Z'],['G','S','a']]).
p([['U','k','b','h'],['X','P','G'],['U','W','t']],[['U','k','b'],['X','P'],['U','W']]).
q([['X','J','O'],['z','e','C','y'],['k','z','Z','S'],['B','n','z','K']],[['X','J','O'],['z','e','C','y'],['k','z','Z','S'],['B','n','z']]).
q([['u','p','t','S'],['f','V','D'],['Y','U','A','O'],['N','C','L']],[['u','p','t'],['f','V','D'],['Y','U','A'],['N','C','L']]).
q([['O','N','I','h'],['q','b','h'],['h','m','M']],[['O','N','I'],['q','b','h'],['h','m','M']]).
q([['D','m','g'],['y','V','n'],['f','L','Q','n'],['H','O','k']],[['D','m'],['y','V','n'],['f','L','Q'],['H','O','k']]).
q([['f','y','Y'],['M','x','n']],[['f','y'],['M','x','n']]).
