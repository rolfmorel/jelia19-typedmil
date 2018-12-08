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

my_uppercase3(A):-upcase_atom(A,A).
my_succ4(A,B):-succ(A,B),B =< 10.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_uppercase3/1).
prim(my_succ4/2).
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
p([['k','O','r'],['B','g','o','J'],['L','w','z','I']],[['k','O'],['B','g','o'],['L','w','z']]).
p([['H','l','J','B'],['T','s','y','o']],[['H','l','J'],['T','s','y']]).
p([['u','H','p'],['h','U','K'],['k','N','R','i'],['I','G','y']],[['u','H'],['h','U'],['k','N','R'],['I','G']]).
p([['J','W','J'],['P','E','g'],['s','q','i','i'],['T','D','f']],[['J','W'],['P','E'],['s','q','i'],['T','D']]).
p([['K','v','P','W'],['e','Y','I'],['t','I','c']],[['K','v','P'],['e','Y'],['t','I']]).
q([['c','H','a','B'],['J','b','W','c'],['V','S','B','D']],[['c','H','a'],['J','b','W','c'],['V','S','B','D']]).
q([['k','i','k'],['h','Y','s','g'],['j','B','L']],[['k','i','k'],['h','Y','s','g'],['j','B']]).
q([['w','X','t'],['J','E','u','T'],['J','z','t','c']],[['w','X','t'],['J','E','u'],['J','z','t','c']]).
q([['X','j','a'],['z','t','z'],['a','I','u','X']],[['X','j','a'],['z','t','z'],['a','I','u']]).
q([['r','l','e','S'],['X','g','p','l'],['G','Y','Z','f']],[['r','l','e','S'],['X','g','p','l'],['G','Y','Z']]).
