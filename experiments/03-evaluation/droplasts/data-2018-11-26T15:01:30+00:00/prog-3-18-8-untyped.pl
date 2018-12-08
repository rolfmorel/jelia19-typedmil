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

my_max_list3(A,B):-max_list(A,B).
my_lowercase4(A):-downcase_atom(A,A).
my_odd5(A):-1 is A mod 2.
my_min_list6(A,B):-min_list(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_succ8(A,B):-succ(A,B),B =< 10.
my_element9(A,B):-member(B,A).

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

my_last11(A,B):-last(A,B).
my_list_to_set12(A,B):-list_to_set(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_double14(N,M):-M is 2*N,M =< 10.
my_head15([H|_],H).
my_even16(A):-0 is A mod 2.
my_set17(A):-list_to_set(A,A).
my_tolower18(A,B):-downcase_atom(A,B).
my_msort19(A,B):-msort(A,B).
my_flatten20(A,B):-flatten(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_max_list3/2).
prim(my_lowercase4/1).
prim(my_odd5/1).
prim(my_min_list6/2).
prim(my_pred7/2).
prim(my_succ8/2).
prim(my_element9/2).
prim(my_last11/2).
prim(my_list_to_set12/2).
prim(my_toupper13/2).
prim(my_double14/2).
prim(my_head15/2).
prim(my_even16/1).
prim(my_set17/1).
prim(my_tolower18/2).
prim(my_msort19/2).
prim(my_flatten20/2).
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
p([['j','N','D','r'],['g','z','q']],[['j','N','D'],['g','z']]).
p([['R','D','r'],['w','X','d','Q'],['d','B','Z','E']],[['R','D'],['w','X','d'],['d','B','Z']]).
p([['I','o','r'],['w','l','x','f'],['D','y','R','k']],[['I','o'],['w','l','x'],['D','y','R']]).
p([['K','A','P'],['H','U','Z','d'],['K','R','S']],[['K','A'],['H','U','Z'],['K','R']]).
p([['m','a','h','U'],['Q','E','o'],['N','x','G']],[['m','a','h'],['Q','E'],['N','x']]).
q([['I','n','P'],['N','a','u']],[['I','n'],['N','a','u']]).
q([['e','I','x','m'],['t','F','W']],[['e','I','x'],['t','F','W']]).
q([['h','H','X','e'],['V','w','h']],[['h','H','X','e'],['V','w']]).
q([['x','j','L','a'],['j','k','d','p'],['m','J','q']],[['x','j','L'],['j','k','d','p'],['m','J','q']]).
q([['x','Y','V'],['Z','l','J'],['e','v','w','d']],[['x','Y','V'],['Z','l'],['e','v','w','d']]).
