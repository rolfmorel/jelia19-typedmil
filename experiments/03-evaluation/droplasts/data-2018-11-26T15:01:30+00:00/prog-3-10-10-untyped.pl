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
my_last4(A,B):-last(A,B).

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

my_uppercase6(A):-upcase_atom(A,A).
my_lowercase7(A):-downcase_atom(A,A).
my_succ8(A,B):-succ(A,B),B =< 10.
my_even9(A):-0 is A mod 2.
my_pred10(A,B):-succ(B,A),A > 0.
my_toupper11(A,B):-upcase_atom(A,B).
my_min_list12(A,B):-min_list(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_flatten3/2).
prim(my_last4/2).
prim(my_uppercase6/1).
prim(my_lowercase7/1).
prim(my_succ8/2).
prim(my_even9/1).
prim(my_pred10/2).
prim(my_toupper11/2).
prim(my_min_list12/2).
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
p([['e','j','b','q'],['m','j','G','y']],[['e','j','b'],['m','j','G']]).
p([['m','e','D','X'],['B','K','v'],['D','s','B']],[['m','e','D'],['B','K'],['D','s']]).
p([['U','p','f','H'],['b','P','Z'],['d','y','p','P']],[['U','p','f'],['b','P'],['d','y','p']]).
p([['m','G','w','h'],['b','M','U','P'],['r','H','L','F'],['M','u','i']],[['m','G','w'],['b','M','U'],['r','H','L'],['M','u']]).
p([['n','Y','A','H'],['y','E','l'],['A','n','y'],['l','K','n']],[['n','Y','A'],['y','E'],['A','n'],['l','K']]).
q([['v','y','C'],['s','w','T'],['k','z','P'],['i','w','x']],[['v','y'],['s','w','T'],['k','z','P'],['i','w']]).
q([['r','v','O','E'],['O','u','l'],['X','K','e','D']],[['r','v','O','E'],['O','u','l'],['X','K','e']]).
q([['K','J','x'],['D','S','v'],['m','M','R','M']],[['K','J','x'],['D','S','v'],['m','M','R']]).
q([['Q','p','F'],['c','k','o'],['G','Z','G','s']],[['Q','p'],['c','k','o'],['G','Z','G','s']]).
q([['U','T','F','b'],['e','b','O','j']],[['U','T','F'],['e','b','O','j']]).
