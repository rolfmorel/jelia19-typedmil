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

my_pred3(A,B):-succ(B,A),A > 0.
my_tolower4(A,B):-downcase_atom(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_msort6(A,B):-msort(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
my_odd9(A):-1 is A mod 2.

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

my_flatten11(A,B):-flatten(A,B).
my_min_list12(A,B):-min_list(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_pred3/2).
prim(my_tolower4/2).
prim(my_double5/2).
prim(my_msort6/2).
prim(my_list_to_set7/2).
prim(my_toupper8/2).
prim(my_odd9/1).
prim(my_flatten11/2).
prim(my_min_list12/2).
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
p([['o','u','c','X'],['H','E','P','t'],['E','d','M','r'],['S','k','w']],[['o','u','c'],['H','E','P'],['E','d','M'],['S','k']]).
p([['D','g','e','t'],['q','g','s']],[['D','g','e'],['q','g']]).
p([['Y','o','d','r'],['p','c','v'],['T','f','Y'],['R','Z','k','o']],[['Y','o','d'],['p','c'],['T','f'],['R','Z','k']]).
p([['v','p','y'],['G','l','C','G'],['i','s','e'],['W','G','A','N']],[['v','p'],['G','l','C'],['i','s'],['W','G','A']]).
p([['F','t','R'],['P','O','r']],[['F','t'],['P','O']]).
q([['o','h','Y','E'],['d','u','K'],['t','d','r','b'],['H','Z','S','J']],[['o','h','Y','E'],['d','u'],['t','d','r','b'],['H','Z','S','J']]).
q([['f','R','V'],['Y','J','r']],[['f','R'],['Y','J','r']]).
q([['g','C','C','d'],['f','r','l','I']],[['g','C','C','d'],['f','r','l']]).
q([['h','o','X'],['p','e','k'],['x','P','A'],['K','d','t','S']],[['h','o','X'],['p','e','k'],['x','P'],['K','d','t','S']]).
q([['f','j','R','o'],['d','m','m'],['r','q','d'],['U','X','p']],[['f','j','R'],['d','m','m'],['r','q','d'],['U','X','p']]).
