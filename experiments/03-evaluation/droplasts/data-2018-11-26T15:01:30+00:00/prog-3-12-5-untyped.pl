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

my_odd3(A):-1 is A mod 2.
my_len4(A,B):-length(A,B).
my_flatten5(A,B):-flatten(A,B).
my_msort6(A,B):-msort(A,B).
my_min_list7(A,B):-min_list(A,B).
my_lowercase8(A):-downcase_atom(A,A).
my_max_list9(A,B):-max_list(A,B).
my_list_to_set10(A,B):-list_to_set(A,B).
my_set11(A):-list_to_set(A,A).

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

my_tolower13(A,B):-downcase_atom(A,B).
my_sumlist14(A,B):-sumlist(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_odd3/1).
prim(my_len4/2).
prim(my_flatten5/2).
prim(my_msort6/2).
prim(my_min_list7/2).
prim(my_lowercase8/1).
prim(my_max_list9/2).
prim(my_list_to_set10/2).
prim(my_set11/1).
prim(my_tolower13/2).
prim(my_sumlist14/2).
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
p([['l','N','h'],['G','g','f','J'],['p','Z','F'],['z','U','Y']],[['l','N'],['G','g','f'],['p','Z'],['z','U']]).
p([['i','c','M'],['F','J','p','z'],['n','f','g','Y'],['k','i','t','n']],[['i','c'],['F','J','p'],['n','f','g'],['k','i','t']]).
p([['t','b','M'],['m','z','f'],['i','V','E','Y']],[['t','b'],['m','z'],['i','V','E']]).
p([['v','f','m','t'],['Z','j','S','j'],['C','T','d','V']],[['v','f','m'],['Z','j','S'],['C','T','d']]).
p([['k','n','F'],['y','Z','M'],['O','A','i'],['j','O','Y']],[['k','n'],['y','Z'],['O','A'],['j','O']]).
q([['t','t','v'],['s','e','E','W']],[['t','t'],['s','e','E','W']]).
q([['Y','h','u','L'],['f','r','d']],[['Y','h','u'],['f','r','d']]).
q([['i','U','m'],['R','t','s','p'],['O','c','P','M'],['P','p','i','M']],[['i','U','m'],['R','t','s'],['O','c','P','M'],['P','p','i']]).
q([['k','C','F'],['y','w','D','V'],['q','G','R','e']],[['k','C','F'],['y','w','D','V'],['q','G','R']]).
q([['C','L','m','r'],['L','Z','U']],[['C','L','m'],['L','Z','U']]).
