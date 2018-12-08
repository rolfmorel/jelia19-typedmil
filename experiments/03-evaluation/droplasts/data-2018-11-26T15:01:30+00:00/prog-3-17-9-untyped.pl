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

my_toupper3(A,B):-upcase_atom(A,B).
my_min_list4(A,B):-min_list(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_flatten6(A,B):-flatten(A,B).
my_head7([H|_],H).
my_sumlist8(A,B):-sumlist(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_msort10(A,B):-msort(A,B).
my_even11(A):-0 is A mod 2.
my_tolower12(A,B):-downcase_atom(A,B).
my_last13(A,B):-last(A,B).
my_max_list14(A,B):-max_list(A,B).
my_odd15(A):-1 is A mod 2.
my_element16(A,B):-member(B,A).
my_set17(A):-list_to_set(A,A).
my_succ18(A,B):-succ(A,B),B =< 10.

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

prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_toupper3/2).
prim(my_min_list4/2).
prim(my_list_to_set5/2).
prim(my_flatten6/2).
prim(my_head7/2).
prim(my_sumlist8/2).
prim(my_pred9/2).
prim(my_msort10/2).
prim(my_even11/1).
prim(my_tolower12/2).
prim(my_last13/2).
prim(my_max_list14/2).
prim(my_odd15/1).
prim(my_element16/2).
prim(my_set17/1).
prim(my_succ18/2).
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
p([['k','Q','D'],['m','y','R'],['T','d','m','d'],['D','g','z']],[['k','Q'],['m','y'],['T','d','m'],['D','g']]).
p([['C','W','N'],['U','E','k','i']],[['C','W'],['U','E','k']]).
p([['e','G','w','Q'],['m','Y','y'],['v','E','U']],[['e','G','w'],['m','Y'],['v','E']]).
p([['a','v','L'],['N','f','Y'],['b','P','p','m'],['E','T','s']],[['a','v'],['N','f'],['b','P','p'],['E','T']]).
p([['x','u','J','D'],['A','a','U'],['K','d','U'],['M','d','H']],[['x','u','J'],['A','a'],['K','d'],['M','d']]).
q([['k','e','a'],['W','F','m','d'],['K','L','c','r'],['W','N','p','q']],[['k','e','a'],['W','F','m','d'],['K','L','c','r'],['W','N','p']]).
q([['J','r','O'],['f','D','G'],['h','u','J'],['R','m','a','v']],[['J','r','O'],['f','D','G'],['h','u'],['R','m','a','v']]).
q([['e','R','T'],['X','G','S','s'],['D','G','m','I']],[['e','R','T'],['X','G','S'],['D','G','m','I']]).
q([['h','H','j','n'],['s','m','K','E'],['N','u','X','u'],['f','E','S']],[['h','H','j'],['s','m','K'],['N','u','X','u'],['f','E','S']]).
q([['E','d','w'],['E','n','S']],[['E','d','w'],['E','n']]).
