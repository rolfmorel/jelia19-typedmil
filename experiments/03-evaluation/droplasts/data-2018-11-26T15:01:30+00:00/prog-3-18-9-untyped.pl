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

my_set3(A):-list_to_set(A,A).
my_tolower4(A,B):-downcase_atom(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_even6(A):-0 is A mod 2.
my_flatten7(A,B):-flatten(A,B).
my_len8(A,B):-length(A,B).
my_head9([H|_],H).
my_max_list10(A,B):-max_list(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.

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

my_msort14(A,B):-msort(A,B).
my_pred15(A,B):-succ(B,A),A > 0.
my_double16(N,M):-M is 2*N,M =< 10.
my_last17(A,B):-last(A,B).
my_uppercase18(A):-upcase_atom(A,A).
my_toupper19(A,B):-upcase_atom(A,B).
my_lowercase20(A):-downcase_atom(A,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_set3/1).
prim(my_tolower4/2).
prim(my_list_to_set5/2).
prim(my_even6/1).
prim(my_flatten7/2).
prim(my_len8/2).
prim(my_head9/2).
prim(my_max_list10/2).
prim(my_sumlist11/2).
prim(my_succ12/2).
prim(my_msort14/2).
prim(my_pred15/2).
prim(my_double16/2).
prim(my_last17/2).
prim(my_uppercase18/1).
prim(my_toupper19/2).
prim(my_lowercase20/1).
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
p([['S','F','W'],['X','J','u','O'],['r','y','q','e'],['I','g','P']],[['S','F'],['X','J','u'],['r','y','q'],['I','g']]).
p([['R','r','L'],['v','l','V'],['c','H','M','j']],[['R','r'],['v','l'],['c','H','M']]).
p([['S','N','h'],['F','f','s'],['f','g','N','b'],['d','D','L','u']],[['S','N'],['F','f'],['f','g','N'],['d','D','L']]).
p([['q','Y','s'],['y','o','e'],['r','q','S','U']],[['q','Y'],['y','o'],['r','q','S']]).
p([['H','n','l','c'],['d','L','t','G']],[['H','n','l'],['d','L','t']]).
q([['R','v','S'],['b','c','K','d'],['d','R','b','X']],[['R','v'],['b','c','K','d'],['d','R','b','X']]).
q([['Z','T','B'],['L','S','A','D']],[['Z','T','B'],['L','S','A']]).
q([['z','N','Y','t'],['S','V','Z','v']],[['z','N','Y'],['S','V','Z','v']]).
q([['Z','F','O'],['K','v','A','L']],[['Z','F'],['K','v','A','L']]).
q([['J','s','t','I'],['u','W','k','Y'],['L','s','Q'],['r','O','P']],[['J','s','t'],['u','W','k','Y'],['L','s','Q'],['r','O','P']]).
