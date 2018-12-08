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

prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_double3/2).
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
p([['w','A','U'],['q','H','U'],['C','A','B']],[['w','A'],['q','H'],['C','A']]).
p([['p','Y','F'],['K','w','K','V'],['b','I','t']],[['p','Y'],['K','w','K'],['b','I']]).
p([['i','b','x'],['y','y','O','J'],['w','N','i']],[['i','b'],['y','y','O'],['w','N']]).
p([['H','t','U','j'],['A','M','k'],['l','L','C']],[['H','t','U'],['A','M'],['l','L']]).
p([['y','Z','Z','u'],['q','U','O','N']],[['y','Z','Z'],['q','U','O']]).
q([['r','T','G'],['Q','l','g']],[['r','T','G'],['Q','l']]).
q([['w','Q','X','j'],['d','s','D','P']],[['w','Q','X'],['d','s','D','P']]).
q([['N','r','m','Y'],['h','U','w','J'],['k','l','w']],[['N','r','m'],['h','U','w','J'],['k','l','w']]).
q([['S','Q','l'],['T','N','D'],['C','A','r']],[['S','Q','l'],['T','N'],['C','A','r']]).
q([['R','h','g','i'],['U','J','E','t'],['S','b','P','b']],[['R','h','g','i'],['U','J','E','t'],['S','b','P']]).
