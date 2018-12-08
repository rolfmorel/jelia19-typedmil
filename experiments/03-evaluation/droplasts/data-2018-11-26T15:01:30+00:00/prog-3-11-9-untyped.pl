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

my_element3(A,B):-member(B,A).
my_max_list4(A,B):-max_list(A,B).
my_tolower5(A,B):-downcase_atom(A,B).
my_even6(A):-0 is A mod 2.
my_pred7(A,B):-succ(B,A),A > 0.
my_sumlist8(A,B):-sumlist(A,B).

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

my_succ10(A,B):-succ(A,B),B =< 10.
my_uppercase11(A):-upcase_atom(A,A).
my_toupper12(A,B):-upcase_atom(A,B).
my_head13([H|_],H).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_element3/2).
prim(my_max_list4/2).
prim(my_tolower5/2).
prim(my_even6/1).
prim(my_pred7/2).
prim(my_sumlist8/2).
prim(my_succ10/2).
prim(my_uppercase11/1).
prim(my_toupper12/2).
prim(my_head13/2).
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
p([['b','N','x'],['U','B','g'],['f','N','R','U'],['g','v','r']],[['b','N'],['U','B'],['f','N','R'],['g','v']]).
p([['s','A','e','j'],['V','X','L','F'],['c','Z','L','N']],[['s','A','e'],['V','X','L'],['c','Z','L']]).
p([['A','x','H'],['N','v','Q']],[['A','x'],['N','v']]).
p([['d','d','b','C'],['q','Q','A','y'],['m','T','h','p'],['T','U','L']],[['d','d','b'],['q','Q','A'],['m','T','h'],['T','U']]).
p([['d','O','y'],['S','s','z','N'],['U','Q','B'],['l','O','R','R']],[['d','O'],['S','s','z'],['U','Q'],['l','O','R']]).
q([['B','o','A'],['L','U','E','i'],['R','H','w']],[['B','o','A'],['L','U','E'],['R','H','w']]).
q([['y','Y','K'],['U','H','M'],['Z','d','m','V'],['a','J','p','e']],[['y','Y','K'],['U','H','M'],['Z','d','m'],['a','J','p','e']]).
q([['U','A','J'],['p','w','Y']],[['U','A'],['p','w','Y']]).
q([['Y','m','z'],['T','u','p','I'],['K','p','N']],[['Y','m','z'],['T','u','p','I'],['K','p']]).
q([['o','q','r'],['B','C','s']],[['o','q'],['B','C','s']]).
