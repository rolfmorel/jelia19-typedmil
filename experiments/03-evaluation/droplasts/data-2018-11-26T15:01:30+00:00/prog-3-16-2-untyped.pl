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

my_element3(A,B):-member(B,A).
my_sumlist4(A,B):-sumlist(A,B).
my_len5(A,B):-length(A,B).
my_toupper6(A,B):-upcase_atom(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_min_list8(A,B):-min_list(A,B).
my_odd9(A):-1 is A mod 2.
my_head10([H|_],H).
my_flatten11(A,B):-flatten(A,B).
my_uppercase12(A):-upcase_atom(A,A).
my_pred13(A,B):-succ(B,A),A > 0.
my_last14(A,B):-last(A,B).

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

my_max_list16(A,B):-max_list(A,B).
my_msort17(A,B):-msort(A,B).
my_even18(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_element3/2).
prim(my_sumlist4/2).
prim(my_len5/2).
prim(my_toupper6/2).
prim(my_double7/2).
prim(my_min_list8/2).
prim(my_odd9/1).
prim(my_head10/2).
prim(my_flatten11/2).
prim(my_uppercase12/1).
prim(my_pred13/2).
prim(my_last14/2).
prim(my_max_list16/2).
prim(my_msort17/2).
prim(my_even18/1).
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
p([['B','L','G'],['V','e','g','Q'],['J','F','V','B']],[['B','L'],['V','e','g'],['J','F','V']]).
p([['C','D','v'],['p','f','o','r'],['S','R','V'],['e','R','o']],[['C','D'],['p','f','o'],['S','R'],['e','R']]).
p([['P','b','P','h'],['W','R','I'],['R','n','b','j']],[['P','b','P'],['W','R'],['R','n','b']]).
p([['u','O','W'],['h','C','Q','X']],[['u','O'],['h','C','Q']]).
p([['c','B','b'],['b','U','d','T'],['C','f','T','l']],[['c','B'],['b','U','d'],['C','f','T']]).
q([['x','j','R'],['O','V','G','t'],['n','q','N'],['I','k','K','m']],[['x','j'],['O','V','G'],['n','q','N'],['I','k','K','m']]).
q([['l','m','I','k'],['R','D','E','v'],['l','c','o']],[['l','m','I','k'],['R','D','E'],['l','c','o']]).
q([['s','g','N','B'],['L','n','F'],['e','N','h','M'],['O','Z','s']],[['s','g','N','B'],['L','n','F'],['e','N','h','M'],['O','Z']]).
q([['r','H','y','S'],['E','t','u'],['l','Z','G','d']],[['r','H','y','S'],['E','t'],['l','Z','G','d']]).
q([['Z','v','d','X'],['v','A','I'],['I','X','m'],['M','L','I','l']],[['Z','v','d'],['v','A'],['I','X','m'],['M','L','I','l']]).
