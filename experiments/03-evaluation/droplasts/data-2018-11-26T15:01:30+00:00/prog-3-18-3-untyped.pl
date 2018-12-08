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

my_set3(A):-list_to_set(A,A).
my_list_to_set4(A,B):-list_to_set(A,B).
my_msort5(A,B):-msort(A,B).
my_toupper6(A,B):-upcase_atom(A,B).
my_flatten7(A,B):-flatten(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_last9(A,B):-last(A,B).

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

my_double11(N,M):-M is 2*N,M =< 10.
my_odd12(A):-1 is A mod 2.
my_succ13(A,B):-succ(A,B),B =< 10.
my_uppercase14(A):-upcase_atom(A,A).
my_element15(A,B):-member(B,A).
my_min_list16(A,B):-min_list(A,B).
my_len17(A,B):-length(A,B).
my_tolower18(A,B):-downcase_atom(A,B).
my_lowercase19(A):-downcase_atom(A,A).
my_head20([H|_],H).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_set3/1).
prim(my_list_to_set4/2).
prim(my_msort5/2).
prim(my_toupper6/2).
prim(my_flatten7/2).
prim(my_sumlist8/2).
prim(my_last9/2).
prim(my_double11/2).
prim(my_odd12/1).
prim(my_succ13/2).
prim(my_uppercase14/1).
prim(my_element15/2).
prim(my_min_list16/2).
prim(my_len17/2).
prim(my_tolower18/2).
prim(my_lowercase19/1).
prim(my_head20/2).
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
p([['q','W','e','s'],['N','W','z'],['z','i','h']],[['q','W','e'],['N','W'],['z','i']]).
p([['x','h','I'],['X','S','s'],['h','u','V']],[['x','h'],['X','S'],['h','u']]).
p([['W','w','U','J'],['o','Z','d'],['n','P','I','f'],['P','o','I']],[['W','w','U'],['o','Z'],['n','P','I'],['P','o']]).
p([['h','J','E','N'],['R','F','g','G']],[['h','J','E'],['R','F','g']]).
p([['r','F','x','C'],['C','q','w','T'],['C','j','i','w']],[['r','F','x'],['C','q','w'],['C','j','i']]).
q([['e','S','b','b'],['i','c','X']],[['e','S','b','b'],['i','c']]).
q([['g','j','O'],['U','w','q','a'],['M','X','H','p'],['a','F','a','m']],[['g','j','O'],['U','w','q','a'],['M','X','H'],['a','F','a','m']]).
q([['i','B','d','e'],['Z','a','P','o']],[['i','B','d','e'],['Z','a','P']]).
q([['a','H','G','q'],['T','n','R']],[['a','H','G','q'],['T','n']]).
q([['R','z','Z'],['M','d','t'],['x','m','t'],['P','n','E']],[['R','z','Z'],['M','d','t'],['x','m','t'],['P','n']]).
