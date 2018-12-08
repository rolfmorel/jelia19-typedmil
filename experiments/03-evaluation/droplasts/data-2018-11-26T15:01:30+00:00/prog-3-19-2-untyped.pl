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
my_flatten4(A,B):-flatten(A,B).
my_odd5(A):-1 is A mod 2.
my_min_list6(A,B):-min_list(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_tolower8(A,B):-downcase_atom(A,B).
my_last9(A,B):-last(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_double11(N,M):-M is 2*N,M =< 10.
my_sumlist12(A,B):-sumlist(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_max_list14(A,B):-max_list(A,B).
my_head15([H|_],H).
my_uppercase16(A):-upcase_atom(A,A).
my_lowercase17(A):-downcase_atom(A,A).
my_set18(A):-list_to_set(A,A).
my_even19(A):-0 is A mod 2.

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

my_len21(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_succ3/2).
prim(my_flatten4/2).
prim(my_odd5/1).
prim(my_min_list6/2).
prim(my_list_to_set7/2).
prim(my_tolower8/2).
prim(my_last9/2).
prim(my_pred10/2).
prim(my_double11/2).
prim(my_sumlist12/2).
prim(my_toupper13/2).
prim(my_max_list14/2).
prim(my_head15/2).
prim(my_uppercase16/1).
prim(my_lowercase17/1).
prim(my_set18/1).
prim(my_even19/1).
prim(my_len21/2).
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
p([['c','i','d'],['Z','k','c','B']],[['c','i'],['Z','k','c']]).
p([['j','A','I'],['L','b','F']],[['j','A'],['L','b']]).
p([['d','E','k','r'],['j','X','V'],['s','v','I']],[['d','E','k'],['j','X'],['s','v']]).
p([['K','M','L','C'],['A','G','b']],[['K','M','L'],['A','G']]).
p([['g','J','S'],['v','K','S','R'],['i','N','Q']],[['g','J'],['v','K','S'],['i','N']]).
q([['b','q','b','M'],['c','M','U']],[['b','q','b','M'],['c','M']]).
q([['l','I','e'],['l','b','F']],[['l','I','e'],['l','b']]).
q([['Q','f','M','j'],['E','P','Z','N'],['r','e','C','z'],['J','y','n']],[['Q','f','M','j'],['E','P','Z'],['r','e','C','z'],['J','y','n']]).
q([['k','R','y','T'],['n','K','O']],[['k','R','y'],['n','K','O']]).
q([['f','B','b','f'],['F','z','X','G'],['m','i','G']],[['f','B','b','f'],['F','z','X','G'],['m','i']]).
