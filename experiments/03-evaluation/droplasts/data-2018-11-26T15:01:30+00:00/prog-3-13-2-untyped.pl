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

my_toupper5(A,B):-upcase_atom(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_list_to_set7(A,B):-list_to_set(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_odd9(A):-1 is A mod 2.
my_len10(A,B):-length(A,B).
my_flatten11(A,B):-flatten(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_msort13(A,B):-msort(A,B).
my_tolower14(A,B):-downcase_atom(A,B).
my_even15(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_max_list3/2).
prim(my_toupper5/2).
prim(my_uppercase6/1).
prim(my_list_to_set7/2).
prim(my_double8/2).
prim(my_odd9/1).
prim(my_len10/2).
prim(my_flatten11/2).
prim(my_sumlist12/2).
prim(my_msort13/2).
prim(my_tolower14/2).
prim(my_even15/1).
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
p([['A','y','H','m'],['L','G','X']],[['A','y','H'],['L','G']]).
p([['M','f','O','p'],['d','v','m'],['N','k','l','k']],[['M','f','O'],['d','v'],['N','k','l']]).
p([['q','W','E','x'],['L','M','X'],['M','t','z','x'],['Y','F','K']],[['q','W','E'],['L','M'],['M','t','z'],['Y','F']]).
p([['C','V','V','U'],['i','L','q','H'],['l','Z','E','u'],['E','v','P','v']],[['C','V','V'],['i','L','q'],['l','Z','E'],['E','v','P']]).
p([['n','K','Q'],['y','x','u'],['U','N','i'],['m','B','w']],[['n','K'],['y','x'],['U','N'],['m','B']]).
q([['h','K','R','Y'],['b','U','q','V']],[['h','K','R','Y'],['b','U','q']]).
q([['c','I','A'],['J','C','h'],['B','T','D','O'],['h','S','A','O']],[['c','I','A'],['J','C','h'],['B','T','D','O'],['h','S','A']]).
q([['a','e','P','R'],['c','R','q','l'],['K','y','j'],['P','e','H','o']],[['a','e','P'],['c','R','q','l'],['K','y','j'],['P','e','H','o']]).
q([['R','w','M'],['o','T','l'],['G','p','a']],[['R','w','M'],['o','T'],['G','p','a']]).
q([['O','g','H'],['F','i','o'],['B','L','V','V']],[['O','g','H'],['F','i'],['B','L','V','V']]).
