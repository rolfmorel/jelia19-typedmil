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

my_even3(A):-0 is A mod 2.
my_odd4(A):-1 is A mod 2.
my_flatten5(A,B):-flatten(A,B).
my_msort6(A,B):-msort(A,B).
my_set7(A):-list_to_set(A,A).
my_element8(A,B):-member(B,A).
my_pred9(A,B):-succ(B,A),A > 0.
my_head10([H|_],H).
my_lowercase11(A):-downcase_atom(A,A).
my_toupper12(A,B):-upcase_atom(A,B).
my_uppercase13(A):-upcase_atom(A,A).
my_succ14(A,B):-succ(A,B),B =< 10.
my_last15(A,B):-last(A,B).
my_max_list16(A,B):-max_list(A,B).
my_double17(N,M):-M is 2*N,M =< 10.
my_list_to_set18(A,B):-list_to_set(A,B).

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

my_sumlist20(A,B):-sumlist(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_even3/1).
prim(my_odd4/1).
prim(my_flatten5/2).
prim(my_msort6/2).
prim(my_set7/1).
prim(my_element8/2).
prim(my_pred9/2).
prim(my_head10/2).
prim(my_lowercase11/1).
prim(my_toupper12/2).
prim(my_uppercase13/1).
prim(my_succ14/2).
prim(my_last15/2).
prim(my_max_list16/2).
prim(my_double17/2).
prim(my_list_to_set18/2).
prim(my_sumlist20/2).
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
p([['X','m','Y'],['n','o','y'],['r','z','Q']],[['X','m'],['n','o'],['r','z']]).
p([['K','B','e','N'],['A','A','A','I'],['q','Z','Z','e']],[['K','B','e'],['A','A','A'],['q','Z','Z']]).
p([['r','N','T'],['U','b','h']],[['r','N'],['U','b']]).
p([['k','c','t'],['C','Q','J','N'],['g','D','G'],['K','U','A']],[['k','c'],['C','Q','J'],['g','D'],['K','U']]).
p([['N','a','M','F'],['n','w','k','H'],['i','K','H','y'],['e','w','a','B']],[['N','a','M'],['n','w','k'],['i','K','H'],['e','w','a']]).
q([['D','j','h','E'],['U','Q','z','S'],['M','f','f','A']],[['D','j','h'],['U','Q','z','S'],['M','f','f','A']]).
q([['s','w','K'],['V','K','V','F'],['I','d','y'],['m','D','p']],[['s','w'],['V','K','V','F'],['I','d'],['m','D','p']]).
q([['Q','w','Q'],['M','I','H']],[['Q','w'],['M','I','H']]).
q([['w','v','f'],['q','B','S']],[['w','v'],['q','B','S']]).
q([['A','y','N'],['e','o','M','p'],['R','p','i']],[['A','y','N'],['e','o','M','p'],['R','p']]).
