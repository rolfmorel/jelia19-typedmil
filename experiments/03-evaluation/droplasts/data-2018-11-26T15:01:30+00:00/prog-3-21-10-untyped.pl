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

my_head3([H|_],H).

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

my_lowercase5(A):-downcase_atom(A,A).
my_element6(A,B):-member(B,A).
my_set7(A):-list_to_set(A,A).
my_toupper8(A,B):-upcase_atom(A,B).
my_tolower9(A,B):-downcase_atom(A,B).
my_len10(A,B):-length(A,B).
my_last11(A,B):-last(A,B).
my_max_list12(A,B):-max_list(A,B).
my_double13(N,M):-M is 2*N,M =< 10.
my_min_list14(A,B):-min_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_flatten16(A,B):-flatten(A,B).
my_pred17(A,B):-succ(B,A),A > 0.
my_list_to_set18(A,B):-list_to_set(A,B).
my_uppercase19(A):-upcase_atom(A,A).
my_msort20(A,B):-msort(A,B).
my_odd21(A):-1 is A mod 2.
my_succ22(A,B):-succ(A,B),B =< 10.
my_even23(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_head3/2).
prim(my_lowercase5/1).
prim(my_element6/2).
prim(my_set7/1).
prim(my_toupper8/2).
prim(my_tolower9/2).
prim(my_len10/2).
prim(my_last11/2).
prim(my_max_list12/2).
prim(my_double13/2).
prim(my_min_list14/2).
prim(my_sumlist15/2).
prim(my_flatten16/2).
prim(my_pred17/2).
prim(my_list_to_set18/2).
prim(my_uppercase19/1).
prim(my_msort20/2).
prim(my_odd21/1).
prim(my_succ22/2).
prim(my_even23/1).
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
p([['B','W','A','Z'],['b','e','U','I'],['e','C','k'],['S','K','G','p']],[['B','W','A'],['b','e','U'],['e','C'],['S','K','G']]).
p([['Y','y','z'],['r','V','K'],['y','J','n']],[['Y','y'],['r','V'],['y','J']]).
p([['T','K','p'],['a','P','V','r']],[['T','K'],['a','P','V']]).
p([['u','f','L','Y'],['h','P','x','E'],['Y','n','a','k'],['l','y','K','t']],[['u','f','L'],['h','P','x'],['Y','n','a'],['l','y','K']]).
p([['W','b','b','j'],['R','k','s']],[['W','b','b'],['R','k']]).
q([['V','c','s'],['r','C','d']],[['V','c','s'],['r','C']]).
q([['r','W','A','z'],['Z','u','Z','t'],['C','V','A'],['H','A','z']],[['r','W','A'],['Z','u','Z','t'],['C','V','A'],['H','A','z']]).
q([['L','y','G','s'],['K','p','I'],['E','k','u','M']],[['L','y','G'],['K','p','I'],['E','k','u','M']]).
q([['N','g','s'],['u','C','D'],['V','N','B','F']],[['N','g'],['u','C','D'],['V','N','B','F']]).
q([['k','t','X'],['Y','r','G','H']],[['k','t'],['Y','r','G','H']]).
