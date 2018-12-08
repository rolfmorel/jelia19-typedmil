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

my_list_to_set3(A,B):-list_to_set(A,B).

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

my_lowercase5(A):-downcase_atom(A,A).
my_sumlist6(A,B):-sumlist(A,B).
my_odd7(A):-1 is A mod 2.
my_tolower8(A,B):-downcase_atom(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_toupper10(A,B):-upcase_atom(A,B).
my_msort11(A,B):-msort(A,B).
my_uppercase12(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_list_to_set3/2).
prim(my_lowercase5/1).
prim(my_sumlist6/2).
prim(my_odd7/1).
prim(my_tolower8/2).
prim(my_pred9/2).
prim(my_toupper10/2).
prim(my_msort11/2).
prim(my_uppercase12/1).
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
p([['X','d','J','E'],['J','Y','o'],['h','A','G','w']],[['X','d','J'],['J','Y'],['h','A','G']]).
p([['X','k','o','z'],['G','K','r'],['L','x','d']],[['X','k','o'],['G','K'],['L','x']]).
p([['v','I','y'],['D','G','T','u']],[['v','I'],['D','G','T']]).
p([['d','J','p'],['c','c','v'],['M','T','h','d'],['v','r','C']],[['d','J'],['c','c'],['M','T','h'],['v','r']]).
p([['z','t','m','L'],['b','v','Y'],['X','n','G']],[['z','t','m'],['b','v'],['X','n']]).
q([['J','h','r'],['b','k','b','Z']],[['J','h'],['b','k','b','Z']]).
q([['B','g','S'],['v','E','S']],[['B','g','S'],['v','E']]).
q([['q','C','k'],['H','M','R'],['R','c','e','y']],[['q','C'],['H','M','R'],['R','c','e','y']]).
q([['X','r','S','q'],['r','q','F'],['P','e','i'],['z','P','h']],[['X','r','S','q'],['r','q'],['P','e','i'],['z','P']]).
q([['X','L','y'],['U','l','L','w'],['b','N','f'],['D','Z','s','H']],[['X','L','y'],['U','l','L','w'],['b','N','f'],['D','Z','s']]).
