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
my_len4(A,B):-length(A,B).

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

my_toupper6(A,B):-upcase_atom(A,B).
my_lowercase7(A):-downcase_atom(A,A).
my_double8(N,M):-M is 2*N,M =< 10.
my_msort9(A,B):-msort(A,B).
my_succ10(A,B):-succ(A,B),B =< 10.
my_list_to_set11(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_flatten3/2).
prim(my_len4/2).
prim(my_toupper6/2).
prim(my_lowercase7/1).
prim(my_double8/2).
prim(my_msort9/2).
prim(my_succ10/2).
prim(my_list_to_set11/2).
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
p([['u','d','w','r'],['H','Q','X','v']],[['u','d','w'],['H','Q','X']]).
p([['M','N','L'],['C','H','I','l'],['L','V','g','w']],[['M','N'],['C','H','I'],['L','V','g']]).
p([['x','v','y','n'],['d','D','M']],[['x','v','y'],['d','D']]).
p([['t','u','J'],['l','d','H','v'],['R','Q','S']],[['t','u'],['l','d','H'],['R','Q']]).
p([['l','Q','O','Q'],['U','C','I','Y']],[['l','Q','O'],['U','C','I']]).
q([['m','Y','Z','s'],['o','x','z']],[['m','Y','Z','s'],['o','x']]).
q([['j','J','T','h'],['M','F','c']],[['j','J','T','h'],['M','F']]).
q([['r','l','S'],['R','v','y'],['k','n','s','c'],['n','n','E']],[['r','l'],['R','v','y'],['k','n','s','c'],['n','n','E']]).
q([['z','n','x'],['O','A','y'],['y','H','X','W']],[['z','n'],['O','A','y'],['y','H','X','W']]).
q([['f','y','k','R'],['Q','c','C']],[['f','y','k'],['Q','c','C']]).
