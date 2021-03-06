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
my_toupper4(A,B):-upcase_atom(A,B).
my_len5(A,B):-length(A,B).
my_flatten6(A,B):-flatten(A,B).
my_set7(A):-list_to_set(A,A).
my_last8(A,B):-last(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_odd10(A):-1 is A mod 2.
my_uppercase11(A):-upcase_atom(A,A).
my_tolower12(A,B):-downcase_atom(A,B).
my_head13([H|_],H).
my_msort14(A,B):-msort(A,B).
my_pred15(A,B):-succ(B,A),A > 0.

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

my_max_list17(A,B):-max_list(A,B).
my_even18(A):-0 is A mod 2.
my_list_to_set19(A,B):-list_to_set(A,B).
my_min_list20(A,B):-min_list(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_element22(A,B):-member(B,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_succ3/2).
prim(my_toupper4/2).
prim(my_len5/2).
prim(my_flatten6/2).
prim(my_set7/1).
prim(my_last8/2).
prim(my_double9/2).
prim(my_odd10/1).
prim(my_uppercase11/1).
prim(my_tolower12/2).
prim(my_head13/2).
prim(my_msort14/2).
prim(my_pred15/2).
prim(my_max_list17/2).
prim(my_even18/1).
prim(my_list_to_set19/2).
prim(my_min_list20/2).
prim(my_sumlist21/2).
prim(my_element22/2).
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
p([['F','B','D','m'],['L','X','Q']],[['F','B','D'],['L','X']]).
p([['N','l','Q'],['S','F','S','o']],[['N','l'],['S','F','S']]).
p([['b','a','T'],['P','k','t','u'],['z','j','G','r'],['F','N','s','e']],[['b','a'],['P','k','t'],['z','j','G'],['F','N','s']]).
p([['u','T','o'],['X','c','t'],['r','C','C','P'],['r','H','K','t']],[['u','T'],['X','c'],['r','C','C'],['r','H','K']]).
p([['r','q','W'],['f','h','h','U'],['Q','H','C','r']],[['r','q'],['f','h','h'],['Q','H','C']]).
q([['N','O','I'],['H','n','p']],[['N','O','I'],['H','n']]).
q([['w','r','N'],['u','l','I'],['N','P','P'],['o','a','W']],[['w','r','N'],['u','l','I'],['N','P'],['o','a','W']]).
q([['u','d','Z','d'],['f','u','y'],['v','y','U','D']],[['u','d','Z'],['f','u','y'],['v','y','U','D']]).
q([['M','h','Q'],['L','n','N','i'],['X','q','L']],[['M','h','Q'],['L','n','N','i'],['X','q']]).
q([['Q','r','J','w'],['k','b','G']],[['Q','r','J','w'],['k','b']]).
