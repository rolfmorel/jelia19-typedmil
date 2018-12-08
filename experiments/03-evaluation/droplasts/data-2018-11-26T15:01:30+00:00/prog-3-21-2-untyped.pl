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
my_list_to_set4(A,B):-list_to_set(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_head7([H|_],H).
my_element8(A,B):-member(B,A).
my_toupper9(A,B):-upcase_atom(A,B).
my_min_list10(A,B):-min_list(A,B).
my_last11(A,B):-last(A,B).
my_msort12(A,B):-msort(A,B).
my_set13(A):-list_to_set(A,A).
my_tolower14(A,B):-downcase_atom(A,B).
my_even15(A):-0 is A mod 2.
my_odd16(A):-1 is A mod 2.
my_uppercase17(A):-upcase_atom(A,A).

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

my_succ19(A,B):-succ(A,B),B =< 10.
my_len20(A,B):-length(A,B).
my_max_list21(A,B):-max_list(A,B).
my_flatten22(A,B):-flatten(A,B).
my_double23(N,M):-M is 2*N,M =< 10.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_lowercase3/1).
prim(my_list_to_set4/2).
prim(my_sumlist5/2).
prim(my_pred6/2).
prim(my_head7/2).
prim(my_element8/2).
prim(my_toupper9/2).
prim(my_min_list10/2).
prim(my_last11/2).
prim(my_msort12/2).
prim(my_set13/1).
prim(my_tolower14/2).
prim(my_even15/1).
prim(my_odd16/1).
prim(my_uppercase17/1).
prim(my_succ19/2).
prim(my_len20/2).
prim(my_max_list21/2).
prim(my_flatten22/2).
prim(my_double23/2).
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
p([['G','G','h','X'],['T','V','A','j'],['l','J','R']],[['G','G','h'],['T','V','A'],['l','J']]).
p([['A','t','V','G'],['c','L','A','d'],['p','N','V'],['i','d','M','B']],[['A','t','V'],['c','L','A'],['p','N'],['i','d','M']]).
p([['F','J','e'],['E','d','u','k']],[['F','J'],['E','d','u']]).
p([['Z','F','L','m'],['E','Q','v'],['D','f','X','y']],[['Z','F','L'],['E','Q'],['D','f','X']]).
p([['L','p','F'],['x','d','p'],['q','O','C','U']],[['L','p'],['x','d'],['q','O','C']]).
q([['y','b','N'],['Y','v','L'],['t','w','y'],['O','y','S']],[['y','b','N'],['Y','v'],['t','w','y'],['O','y','S']]).
q([['r','J','E'],['i','u','v','F']],[['r','J','E'],['i','u','v']]).
q([['D','z','o'],['Q','U','s','Q'],['l','k','B','O']],[['D','z','o'],['Q','U','s','Q'],['l','k','B']]).
q([['A','J','S'],['p','q','j']],[['A','J','S'],['p','q']]).
q([['Q','Z','w'],['J','F','b'],['I','x','e'],['X','e','l']],[['Q','Z','w'],['J','F','b'],['I','x'],['X','e','l']]).
