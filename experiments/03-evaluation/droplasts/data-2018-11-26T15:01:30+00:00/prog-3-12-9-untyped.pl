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
my_toupper4(A,B):-upcase_atom(A,B).
my_min_list5(A,B):-min_list(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_list_to_set7(A,B):-list_to_set(A,B).
my_tolower8(A,B):-downcase_atom(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_uppercase10(A):-upcase_atom(A,A).

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

my_max_list12(A,B):-max_list(A,B).
my_even13(A):-0 is A mod 2.
my_msort14(A,B):-msort(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_flatten3/2).
prim(my_toupper4/2).
prim(my_min_list5/2).
prim(my_succ6/2).
prim(my_list_to_set7/2).
prim(my_tolower8/2).
prim(my_double9/2).
prim(my_uppercase10/1).
prim(my_max_list12/2).
prim(my_even13/1).
prim(my_msort14/2).
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
p([['u','z','T'],['b','w','h','n'],['k','Q','f','U'],['q','R','t','j']],[['u','z'],['b','w','h'],['k','Q','f'],['q','R','t']]).
p([['G','M','M','C'],['B','E','G'],['p','J','b']],[['G','M','M'],['B','E'],['p','J']]).
p([['m','K','u','S'],['D','C','D']],[['m','K','u'],['D','C']]).
p([['C','H','h','G'],['v','V','N','B'],['a','d','D']],[['C','H','h'],['v','V','N'],['a','d']]).
p([['P','k','Q'],['i','N','x','g'],['F','N','K'],['o','H','j','C']],[['P','k'],['i','N','x'],['F','N'],['o','H','j']]).
q([['Z','m','G','x'],['V','c','w']],[['Z','m','G'],['V','c','w']]).
q([['U','G','P','m'],['L','C','r']],[['U','G','P','m'],['L','C']]).
q([['t','a','y','A'],['u','d','p'],['d','i','Y','M'],['U','w','L','m']],[['t','a','y','A'],['u','d','p'],['d','i','Y'],['U','w','L']]).
q([['y','c','t','R'],['e','F','S','U'],['a','n','w','p']],[['y','c','t','R'],['e','F','S','U'],['a','n','w']]).
q([['V','U','a'],['m','G','l'],['P','P','k'],['e','w','D','z']],[['V','U'],['m','G'],['P','P','k'],['e','w','D','z']]).
