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
my_pred4(A,B):-succ(B,A),A > 0.
my_flatten5(A,B):-flatten(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_odd7(A):-1 is A mod 2.
my_succ8(A,B):-succ(A,B),B =< 10.
my_even9(A):-0 is A mod 2.
my_msort10(A,B):-msort(A,B).
my_lowercase11(A):-downcase_atom(A,A).

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

my_sumlist13(A,B):-sumlist(A,B).
my_uppercase14(A):-upcase_atom(A,A).
my_max_list15(A,B):-max_list(A,B).
my_tolower16(A,B):-downcase_atom(A,B).
my_set17(A):-list_to_set(A,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_double3/2).
prim(my_pred4/2).
prim(my_flatten5/2).
prim(my_list_to_set6/2).
prim(my_odd7/1).
prim(my_succ8/2).
prim(my_even9/1).
prim(my_msort10/2).
prim(my_lowercase11/1).
prim(my_sumlist13/2).
prim(my_uppercase14/1).
prim(my_max_list15/2).
prim(my_tolower16/2).
prim(my_set17/1).
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
p([['C','v','d','W'],['N','x','l','y'],['J','w','X']],[['C','v','d'],['N','x','l'],['J','w']]).
p([['X','M','Q'],['M','y','o','z']],[['X','M'],['M','y','o']]).
p([['k','u','n'],['t','I','o'],['r','r','r'],['Z','C','J']],[['k','u'],['t','I'],['r','r'],['Z','C']]).
p([['e','l','g','v'],['Q','D','O','c'],['A','q','X','S'],['f','J','e']],[['e','l','g'],['Q','D','O'],['A','q','X'],['f','J']]).
p([['U','G','J'],['z','n','O'],['t','T','M']],[['U','G'],['z','n'],['t','T']]).
q([['F','I','M','X'],['V','e','z'],['e','l','k'],['L','T','K','q']],[['F','I','M','X'],['V','e','z'],['e','l'],['L','T','K','q']]).
q([['A','i','y'],['r','l','M'],['c','L','X']],[['A','i','y'],['r','l','M'],['c','L']]).
q([['x','D','c','b'],['D','I','G','d'],['y','q','z','g']],[['x','D','c','b'],['D','I','G','d'],['y','q','z']]).
q([['a','D','Y','u'],['U','z','s'],['u','z','j']],[['a','D','Y'],['U','z','s'],['u','z','j']]).
q([['p','f','f'],['m','P','c','b'],['S','b','u']],[['p','f','f'],['m','P','c','b'],['S','b']]).
