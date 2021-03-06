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
my_pred4(A,B):-succ(B,A),A > 0.

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

my_tolower6(A,B):-downcase_atom(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_even8(A):-0 is A mod 2.
my_succ9(A,B):-succ(A,B),B =< 10.
my_odd10(A):-1 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_lowercase3/1).
prim(my_pred4/2).
prim(my_tolower6/2).
prim(my_toupper7/2).
prim(my_even8/1).
prim(my_succ9/2).
prim(my_odd10/1).
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
p([['w','M','u'],['v','q','o']],[['w','M'],['v','q']]).
p([['E','i','n','g'],['X','a','s','V']],[['E','i','n'],['X','a','s']]).
p([['s','h','Q'],['m','U','g'],['h','A','T','N']],[['s','h'],['m','U'],['h','A','T']]).
p([['x','L','C'],['F','n','P','C'],['Y','B','U'],['n','C','L']],[['x','L'],['F','n','P'],['Y','B'],['n','C']]).
p([['U','W','U'],['t','k','S'],['Y','A','w'],['y','t','R','g']],[['U','W'],['t','k'],['Y','A'],['y','t','R']]).
q([['I','I','O'],['d','F','k'],['k','k','n','h']],[['I','I'],['d','F','k'],['k','k','n','h']]).
q([['H','P','t','v'],['g','f','K'],['l','J','O','I'],['q','J','q']],[['H','P','t','v'],['g','f'],['l','J','O'],['q','J','q']]).
q([['e','s','m'],['R','S','m'],['m','p','h'],['w','b','Z']],[['e','s','m'],['R','S'],['m','p','h'],['w','b','Z']]).
q([['b','O','B','u'],['H','A','V','h']],[['b','O','B'],['H','A','V','h']]).
q([['x','h','C','V'],['t','j','P'],['U','n','o'],['i','e','G','V']],[['x','h','C','V'],['t','j'],['U','n','o'],['i','e','G','V']]).
