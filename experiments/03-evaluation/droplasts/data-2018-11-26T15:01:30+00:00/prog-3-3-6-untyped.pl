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
my_succ4(A,B):-succ(A,B),B =< 10.
my_odd5(A):-1 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_list_to_set3/2).
prim(my_succ4/2).
prim(my_odd5/1).
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
p([['t','R','T','o'],['b','l','R','r']],[['t','R','T'],['b','l','R']]).
p([['x','K','A','M'],['R','h','b','r'],['E','W','P','U'],['g','m','T']],[['x','K','A'],['R','h','b'],['E','W','P'],['g','m']]).
p([['Q','K','C'],['S','b','w','d']],[['Q','K'],['S','b','w']]).
p([['g','k','S','Q'],['u','O','T']],[['g','k','S'],['u','O']]).
p([['j','z','c'],['z','T','F']],[['j','z'],['z','T']]).
q([['x','y','W','H'],['j','X','j']],[['x','y','W','H'],['j','X']]).
q([['H','w','K','b'],['X','n','d'],['A','J','y','g'],['v','B','M','i']],[['H','w','K'],['X','n','d'],['A','J','y'],['v','B','M','i']]).
q([['p','h','j'],['b','O','L']],[['p','h','j'],['b','O']]).
q([['V','q','p'],['e','a','a']],[['V','q'],['e','a','a']]).
q([['q','u','b'],['u','T','n','N'],['B','m','j','L']],[['q','u'],['u','T','n','N'],['B','m','j','L']]).
