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

my_lowercase3(A):-downcase_atom(A,A).
my_pred4(A,B):-succ(B,A),A > 0.
my_odd5(A):-1 is A mod 2.
my_tolower6(A,B):-downcase_atom(A,B).
my_double7(N,M):-M is 2*N,M =< 10.

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

my_sumlist9(A,B):-sumlist(A,B).
my_len10(A,B):-length(A,B).
my_last11(A,B):-last(A,B).
my_list_to_set12(A,B):-list_to_set(A,B).
my_even13(A):-0 is A mod 2.
my_msort14(A,B):-msort(A,B).
my_head15([H|_],H).
my_max_list16(A,B):-max_list(A,B).
my_toupper17(A,B):-upcase_atom(A,B).
my_set18(A):-list_to_set(A,A).
my_uppercase19(A):-upcase_atom(A,A).
my_succ20(A,B):-succ(A,B),B =< 10.
my_min_list21(A,B):-min_list(A,B).
my_flatten22(A,B):-flatten(A,B).
my_element23(A,B):-member(B,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_lowercase3/1).
prim(my_pred4/2).
prim(my_odd5/1).
prim(my_tolower6/2).
prim(my_double7/2).
prim(my_sumlist9/2).
prim(my_len10/2).
prim(my_last11/2).
prim(my_list_to_set12/2).
prim(my_even13/1).
prim(my_msort14/2).
prim(my_head15/2).
prim(my_max_list16/2).
prim(my_toupper17/2).
prim(my_set18/1).
prim(my_uppercase19/1).
prim(my_succ20/2).
prim(my_min_list21/2).
prim(my_flatten22/2).
prim(my_element23/2).
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
p([['v','S','G','m'],['C','r','D'],['N','j','g'],['b','j','C']],[['v','S','G'],['C','r'],['N','j'],['b','j']]).
p([['I','j','F','Z'],['q','C','h','B'],['H','l','x','L'],['W','Y','J','l']],[['I','j','F'],['q','C','h'],['H','l','x'],['W','Y','J']]).
p([['L','d','Y'],['A','o','c'],['a','Z','f','V']],[['L','d'],['A','o'],['a','Z','f']]).
p([['r','N','v'],['k','A','h'],['g','l','j'],['F','k','Z','w']],[['r','N'],['k','A'],['g','l'],['F','k','Z']]).
p([['X','J','U','f'],['C','E','k']],[['X','J','U'],['C','E']]).
q([['A','n','O','k'],['d','r','F','P'],['N','K','e'],['r','n','E']],[['A','n','O','k'],['d','r','F','P'],['N','K','e'],['r','n']]).
q([['Y','V','W'],['I','K','X'],['M','y','m','b']],[['Y','V'],['I','K','X'],['M','y','m','b']]).
q([['r','Q','S','H'],['A','u','O','T'],['Z','E','D','r'],['H','A','w']],[['r','Q','S','H'],['A','u','O','T'],['Z','E','D','r'],['H','A']]).
q([['c','u','K'],['K','J','a'],['a','O','U','N']],[['c','u','K'],['K','J'],['a','O','U','N']]).
q([['l','f','i','f'],['B','p','W','e'],['R','D','H']],[['l','f','i','f'],['B','p','W','e'],['R','D']]).
