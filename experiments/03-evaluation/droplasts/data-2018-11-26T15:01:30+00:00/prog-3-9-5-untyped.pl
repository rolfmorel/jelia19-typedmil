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

my_pred3(A,B):-succ(B,A),A > 0.
my_lowercase4(A):-downcase_atom(A,A).
my_tolower5(A,B):-downcase_atom(A,B).
my_toupper6(A,B):-upcase_atom(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).

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

my_uppercase9(A):-upcase_atom(A,A).
my_succ10(A,B):-succ(A,B),B =< 10.
my_flatten11(A,B):-flatten(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_pred3/2).
prim(my_lowercase4/1).
prim(my_tolower5/2).
prim(my_toupper6/2).
prim(my_list_to_set7/2).
prim(my_uppercase9/1).
prim(my_succ10/2).
prim(my_flatten11/2).
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
p([['M','N','y','h'],['m','K','j','V'],['s','y','H','b']],[['M','N','y'],['m','K','j'],['s','y','H']]).
p([['Q','F','v'],['F','e','v','p'],['D','Q','U','E'],['P','K','J','a']],[['Q','F'],['F','e','v'],['D','Q','U'],['P','K','J']]).
p([['k','K','L'],['b','i','A'],['W','J','s'],['F','h','A','w']],[['k','K'],['b','i'],['W','J'],['F','h','A']]).
p([['A','f','s','J'],['u','j','Q'],['K','B','Q','B'],['M','w','Z','G']],[['A','f','s'],['u','j'],['K','B','Q'],['M','w','Z']]).
p([['A','o','l'],['J','a','f','g'],['N','O','x'],['Q','y','U']],[['A','o'],['J','a','f'],['N','O'],['Q','y']]).
q([['N','L','f'],['s','N','G']],[['N','L','f'],['s','N']]).
q([['f','f','r','Z'],['R','i','w','o'],['U','j','g']],[['f','f','r','Z'],['R','i','w'],['U','j','g']]).
q([['B','R','b'],['g','w','o','R'],['y','w','T','k'],['s','j','t','l']],[['B','R','b'],['g','w','o','R'],['y','w','T','k'],['s','j','t']]).
q([['U','c','f'],['c','u','h','U'],['q','O','p'],['m','N','U','D']],[['U','c','f'],['c','u','h','U'],['q','O','p'],['m','N','U']]).
q([['t','Z','p','W'],['N','i','O','u'],['d','o','B'],['f','Y','A','Y']],[['t','Z','p','W'],['N','i','O','u'],['d','o'],['f','Y','A','Y']]).
