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

my_double4(N,M):-M is 2*N,M =< 10.
my_flatten5(A,B):-flatten(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
my_min_list9(A,B):-min_list(A,B).
my_msort10(A,B):-msort(A,B).
my_odd11(A):-1 is A mod 2.
my_tolower12(A,B):-downcase_atom(A,B).
my_lowercase13(A):-downcase_atom(A,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_double4/2).
prim(my_flatten5/2).
prim(my_list_to_set6/2).
prim(my_sumlist7/2).
prim(my_toupper8/2).
prim(my_min_list9/2).
prim(my_msort10/2).
prim(my_odd11/1).
prim(my_tolower12/2).
prim(my_lowercase13/1).
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
p([['d','o','M','k'],['U','Y','U','q'],['f','b','a','x']],[['d','o','M'],['U','Y','U'],['f','b','a']]).
p([['L','L','T'],['C','w','F','M']],[['L','L'],['C','w','F']]).
p([['U','O','y','j'],['i','W','u','Y']],[['U','O','y'],['i','W','u']]).
p([['m','G','j'],['N','m','W','w'],['J','f','T']],[['m','G'],['N','m','W'],['J','f']]).
p([['o','O','q'],['d','b','Q'],['H','C','Q','w'],['l','p','z','i']],[['o','O'],['d','b'],['H','C','Q'],['l','p','z']]).
q([['l','S','U','P'],['w','p','g'],['e','K','S','w']],[['l','S','U'],['w','p','g'],['e','K','S','w']]).
q([['g','d','V'],['a','k','i'],['n','F','A','H'],['a','m','i','U']],[['g','d'],['a','k'],['n','F','A','H'],['a','m','i','U']]).
q([['O','H','r'],['n','W','b','H'],['U','A','a','d']],[['O','H'],['n','W','b','H'],['U','A','a','d']]).
q([['Z','I','X','F'],['e','X','Q','g']],[['Z','I','X','F'],['e','X','Q']]).
q([['u','S','h','N'],['t','t','r'],['P','o','p','L']],[['u','S','h'],['t','t','r'],['P','o','p','L']]).
