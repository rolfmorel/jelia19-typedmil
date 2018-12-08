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

my_double3(N,M):-M is 2*N,M =< 10.
my_element4(A,B):-member(B,A).
my_head5([H|_],H).
my_len6(A,B):-length(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_lowercase8(A):-downcase_atom(A,A).

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

my_odd10(A):-1 is A mod 2.
my_max_list11(A,B):-max_list(A,B).
my_flatten12(A,B):-flatten(A,B).
my_list_to_set13(A,B):-list_to_set(A,B).
my_last14(A,B):-last(A,B).
my_uppercase15(A):-upcase_atom(A,A).
my_set16(A):-list_to_set(A,A).
my_msort17(A,B):-msort(A,B).
my_min_list18(A,B):-min_list(A,B).
my_tolower19(A,B):-downcase_atom(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_pred21(A,B):-succ(B,A),A > 0.
my_succ22(A,B):-succ(A,B),B =< 10.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_double3/2).
prim(my_element4/2).
prim(my_head5/2).
prim(my_len6/2).
prim(my_toupper7/2).
prim(my_lowercase8/1).
prim(my_odd10/1).
prim(my_max_list11/2).
prim(my_flatten12/2).
prim(my_list_to_set13/2).
prim(my_last14/2).
prim(my_uppercase15/1).
prim(my_set16/1).
prim(my_msort17/2).
prim(my_min_list18/2).
prim(my_tolower19/2).
prim(my_sumlist20/2).
prim(my_pred21/2).
prim(my_succ22/2).
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
p([['g','q','J','p'],['u','C','p','V'],['u','m','L'],['R','W','t','S']],[['g','q','J'],['u','C','p'],['u','m'],['R','W','t']]).
p([['c','S','p','A'],['l','X','r','H'],['J','w','f'],['h','M','S']],[['c','S','p'],['l','X','r'],['J','w'],['h','M']]).
p([['v','w','U','B'],['C','L','k']],[['v','w','U'],['C','L']]).
p([['E','O','D'],['n','I','f'],['c','k','u','F']],[['E','O'],['n','I'],['c','k','u']]).
p([['M','n','K','k'],['L','i','a','c']],[['M','n','K'],['L','i','a']]).
q([['O','t','G'],['w','v','J']],[['O','t','G'],['w','v']]).
q([['n','g','Q'],['b','y','L']],[['n','g','Q'],['b','y']]).
q([['z','a','r','I'],['p','S','z'],['t','V','Z']],[['z','a','r'],['p','S','z'],['t','V','Z']]).
q([['V','P','c'],['K','F','u']],[['V','P','c'],['K','F']]).
q([['T','y','k'],['k','C','n'],['W','r','W','n'],['w','s','r']],[['T','y','k'],['k','C'],['W','r','W','n'],['w','s']]).
