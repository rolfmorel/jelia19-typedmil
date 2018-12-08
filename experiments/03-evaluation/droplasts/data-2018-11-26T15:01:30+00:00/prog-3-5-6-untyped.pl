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
my_msort4(A,B):-msort(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_element6(A,B):-member(B,A).
my_head7([H|_],H).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_pred3/2).
prim(my_msort4/2).
prim(my_succ5/2).
prim(my_element6/2).
prim(my_head7/2).
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
p([['B','f','e'],['M','v','G','S']],[['B','f'],['M','v','G']]).
p([['E','q','E','q'],['L','L','p','x'],['B','l','v'],['N','L','l','A']],[['E','q','E'],['L','L','p'],['B','l'],['N','L','l']]).
p([['u','K','C','H'],['k','k','M']],[['u','K','C'],['k','k']]).
p([['T','P','B','X'],['d','Z','S'],['s','O','J','M']],[['T','P','B'],['d','Z'],['s','O','J']]).
p([['m','s','N','U'],['L','y','W','H']],[['m','s','N'],['L','y','W']]).
q([['N','F','m','E'],['C','D','k'],['I','U','m','E']],[['N','F','m'],['C','D','k'],['I','U','m','E']]).
q([['a','z','z','G'],['M','N','R']],[['a','z','z'],['M','N','R']]).
q([['T','O','x','Q'],['X','B','h'],['h','E','r','C']],[['T','O','x','Q'],['X','B','h'],['h','E','r']]).
q([['V','B','F'],['X','J','Y','z']],[['V','B','F'],['X','J','Y']]).
q([['Z','z','W'],['u','r','H']],[['Z','z'],['u','r','H']]).
