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

my_succ3(A,B):-succ(A,B),B =< 10.
my_element4(A,B):-member(B,A).
my_toupper5(A,B):-upcase_atom(A,B).
my_head6([H|_],H).
my_odd7(A):-1 is A mod 2.
my_set8(A):-list_to_set(A,A).
my_msort9(A,B):-msort(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_flatten11(A,B):-flatten(A,B).
my_len12(A,B):-length(A,B).
my_min_list13(A,B):-min_list(A,B).
my_pred14(A,B):-succ(B,A),A > 0.
my_tolower15(A,B):-downcase_atom(A,B).

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

my_even17(A):-0 is A mod 2.
my_list_to_set18(A,B):-list_to_set(A,B).
my_uppercase19(A):-upcase_atom(A,A).
my_double20(N,M):-M is 2*N,M =< 10.
my_max_list21(A,B):-max_list(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_succ3/2).
prim(my_element4/2).
prim(my_toupper5/2).
prim(my_head6/2).
prim(my_odd7/1).
prim(my_set8/1).
prim(my_msort9/2).
prim(my_lowercase10/1).
prim(my_flatten11/2).
prim(my_len12/2).
prim(my_min_list13/2).
prim(my_pred14/2).
prim(my_tolower15/2).
prim(my_even17/1).
prim(my_list_to_set18/2).
prim(my_uppercase19/1).
prim(my_double20/2).
prim(my_max_list21/2).
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
p([['w','V','h'],['k','w','Z']],[['w','V'],['k','w']]).
p([['g','s','h','M'],['t','b','i'],['k','t','T'],['k','P','u']],[['g','s','h'],['t','b'],['k','t'],['k','P']]).
p([['a','q','q'],['T','Q','U'],['C','G','j','z'],['O','g','b','G']],[['a','q'],['T','Q'],['C','G','j'],['O','g','b']]).
p([['s','c','i'],['Y','S','r','k'],['r','Y','S','x'],['s','Y','r']],[['s','c'],['Y','S','r'],['r','Y','S'],['s','Y']]).
p([['k','j','D'],['h','M','o']],[['k','j'],['h','M']]).
q([['R','A','w','U'],['D','a','c','F'],['j','g','W'],['b','M','S','U']],[['R','A','w'],['D','a','c','F'],['j','g','W'],['b','M','S','U']]).
q([['h','H','Y'],['N','n','r'],['x','S','b','Z']],[['h','H'],['N','n','r'],['x','S','b','Z']]).
q([['l','H','v','C'],['U','D','x','E'],['Q','T','A','d'],['l','J','W','E']],[['l','H','v','C'],['U','D','x'],['Q','T','A'],['l','J','W','E']]).
q([['r','d','L'],['W','M','f'],['W','h','i'],['U','V','a']],[['r','d'],['W','M'],['W','h','i'],['U','V','a']]).
q([['C','k','f','x'],['O','B','B'],['C','f','E','z']],[['C','k','f','x'],['O','B'],['C','f','E','z']]).
