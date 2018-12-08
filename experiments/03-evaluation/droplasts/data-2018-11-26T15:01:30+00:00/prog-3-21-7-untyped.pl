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
my_double4(N,M):-M is 2*N,M =< 10.
my_list_to_set5(A,B):-list_to_set(A,B).
my_set6(A):-list_to_set(A,A).
my_lowercase7(A):-downcase_atom(A,A).

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
my_even10(A):-0 is A mod 2.
my_msort11(A,B):-msort(A,B).
my_element12(A,B):-member(B,A).
my_len13(A,B):-length(A,B).
my_toupper14(A,B):-upcase_atom(A,B).
my_pred15(A,B):-succ(B,A),A > 0.
my_head16([H|_],H).
my_uppercase17(A):-upcase_atom(A,A).
my_succ18(A,B):-succ(A,B),B =< 10.
my_min_list19(A,B):-min_list(A,B).
my_last20(A,B):-last(A,B).
my_odd21(A):-1 is A mod 2.
my_tolower22(A,B):-downcase_atom(A,B).
my_flatten23(A,B):-flatten(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_max_list3/2).
prim(my_double4/2).
prim(my_list_to_set5/2).
prim(my_set6/1).
prim(my_lowercase7/1).
prim(my_sumlist9/2).
prim(my_even10/1).
prim(my_msort11/2).
prim(my_element12/2).
prim(my_len13/2).
prim(my_toupper14/2).
prim(my_pred15/2).
prim(my_head16/2).
prim(my_uppercase17/1).
prim(my_succ18/2).
prim(my_min_list19/2).
prim(my_last20/2).
prim(my_odd21/1).
prim(my_tolower22/2).
prim(my_flatten23/2).
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
p([['C','L','p','H'],['l','p','F','E'],['v','j','l']],[['C','L','p'],['l','p','F'],['v','j']]).
p([['z','M','e'],['L','e','S'],['f','M','N']],[['z','M'],['L','e'],['f','M']]).
p([['O','S','E'],['W','Q','f']],[['O','S'],['W','Q']]).
p([['Z','G','s'],['T','z','y','t'],['P','k','F','r'],['m','n','H','y']],[['Z','G'],['T','z','y'],['P','k','F'],['m','n','H']]).
p([['b','O','x','N'],['d','r','U','N']],[['b','O','x'],['d','r','U']]).
q([['a','m','Z'],['m','m','X']],[['a','m','Z'],['m','m']]).
q([['T','I','d','J'],['e','m','R','B']],[['T','I','d','J'],['e','m','R']]).
q([['L','u','E','G'],['Z','n','O','f'],['g','r','I']],[['L','u','E'],['Z','n','O','f'],['g','r','I']]).
q([['V','o','Z'],['Y','d','z','u'],['G','J','F']],[['V','o','Z'],['Y','d','z'],['G','J','F']]).
q([['c','q','m','K'],['q','m','x'],['s','l','q','i'],['c','I','g','U']],[['c','q','m','K'],['q','m'],['s','l','q','i'],['c','I','g','U']]).
