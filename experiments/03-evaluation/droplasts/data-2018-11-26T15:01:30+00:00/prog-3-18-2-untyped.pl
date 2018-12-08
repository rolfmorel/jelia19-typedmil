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

my_msort3(A,B):-msort(A,B).
my_lowercase4(A):-downcase_atom(A,A).
my_list_to_set5(A,B):-list_to_set(A,B).
my_min_list6(A,B):-min_list(A,B).
my_flatten7(A,B):-flatten(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_len10(A,B):-length(A,B).
my_double11(N,M):-M is 2*N,M =< 10.
my_pred12(A,B):-succ(B,A),A > 0.
my_even13(A):-0 is A mod 2.
my_last14(A,B):-last(A,B).
my_set15(A):-list_to_set(A,A).
my_head16([H|_],H).
my_odd17(A):-1 is A mod 2.

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

my_element19(A,B):-member(B,A).
my_tolower20(A,B):-downcase_atom(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_msort3/2).
prim(my_lowercase4/1).
prim(my_list_to_set5/2).
prim(my_min_list6/2).
prim(my_flatten7/2).
prim(my_sumlist8/2).
prim(my_succ9/2).
prim(my_len10/2).
prim(my_double11/2).
prim(my_pred12/2).
prim(my_even13/1).
prim(my_last14/2).
prim(my_set15/1).
prim(my_head16/2).
prim(my_odd17/1).
prim(my_element19/2).
prim(my_tolower20/2).
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
p([['T','N','F'],['y','Q','a'],['s','a','f'],['J','c','G','Q']],[['T','N'],['y','Q'],['s','a'],['J','c','G']]).
p([['e','y','v'],['P','B','F','L'],['J','J','b'],['e','c','I','t']],[['e','y'],['P','B','F'],['J','J'],['e','c','I']]).
p([['x','T','W','Z'],['a','e','S','M'],['o','f','m','n']],[['x','T','W'],['a','e','S'],['o','f','m']]).
p([['s','X','J'],['N','W','V']],[['s','X'],['N','W']]).
p([['I','B','g','X'],['d','f','Y','P'],['o','T','O'],['h','w','k']],[['I','B','g'],['d','f','Y'],['o','T'],['h','w']]).
q([['W','R','p'],['P','J','H']],[['W','R','p'],['P','J']]).
q([['u','h','q','Z'],['Y','z','Q'],['C','z','i','r']],[['u','h','q','Z'],['Y','z'],['C','z','i','r']]).
q([['d','f','e'],['p','D','U'],['z','T','h'],['k','L','I','E']],[['d','f'],['p','D','U'],['z','T'],['k','L','I','E']]).
q([['r','a','w'],['r','b','l']],[['r','a'],['r','b','l']]).
q([['q','O','t'],['g','H','y']],[['q','O'],['g','H','y']]).
