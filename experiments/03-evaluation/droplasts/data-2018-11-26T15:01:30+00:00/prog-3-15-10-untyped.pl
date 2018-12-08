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
my_odd4(A):-1 is A mod 2.
my_head5([H|_],H).
my_tolower6(A,B):-downcase_atom(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_pred9(A,B):-succ(B,A),A > 0.
my_flatten10(A,B):-flatten(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_lowercase13(A):-downcase_atom(A,A).
my_len14(A,B):-length(A,B).
my_succ15(A,B):-succ(A,B),B =< 10.
my_msort16(A,B):-msort(A,B).
my_uppercase17(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_max_list3/2).
prim(my_odd4/1).
prim(my_head5/2).
prim(my_tolower6/2).
prim(my_toupper7/2).
prim(my_double8/2).
prim(my_pred9/2).
prim(my_flatten10/2).
prim(my_list_to_set11/2).
prim(my_sumlist12/2).
prim(my_lowercase13/1).
prim(my_len14/2).
prim(my_succ15/2).
prim(my_msort16/2).
prim(my_uppercase17/1).
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
p([['z','D','S'],['V','q','L'],['Q','L','h']],[['z','D'],['V','q'],['Q','L']]).
p([['L','K','H'],['S','O','H','g'],['V','m','G'],['H','l','y']],[['L','K'],['S','O','H'],['V','m'],['H','l']]).
p([['X','P','A','N'],['A','X','l','O']],[['X','P','A'],['A','X','l']]).
p([['N','G','u','y'],['N','L','N','Q'],['r','R','F','t']],[['N','G','u'],['N','L','N'],['r','R','F']]).
p([['Q','g','v'],['z','A','O'],['p','M','S'],['R','D','z','O']],[['Q','g'],['z','A'],['p','M'],['R','D','z']]).
q([['u','Q','o'],['Y','F','k','T'],['o','Z','O'],['W','A','l','v']],[['u','Q'],['Y','F','k'],['o','Z','O'],['W','A','l','v']]).
q([['v','E','X'],['Y','U','i']],[['v','E'],['Y','U','i']]).
q([['P','n','f'],['m','K','q','d'],['d','S','w'],['e','E','I']],[['P','n','f'],['m','K','q','d'],['d','S'],['e','E','I']]).
q([['l','d','F'],['x','r','d','z'],['w','C','B','Y'],['x','L','n','Q']],[['l','d','F'],['x','r','d','z'],['w','C','B','Y'],['x','L','n']]).
q([['x','r','A','N'],['g','n','L','P'],['l','H','F'],['v','W','r']],[['x','r','A','N'],['g','n','L','P'],['l','H'],['v','W']]).
