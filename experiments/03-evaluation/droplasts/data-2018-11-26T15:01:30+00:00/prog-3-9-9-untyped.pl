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

my_lowercase3(A):-downcase_atom(A,A).
my_min_list4(A,B):-min_list(A,B).
my_head5([H|_],H).
my_flatten6(A,B):-flatten(A,B).
my_set7(A):-list_to_set(A,A).
my_len8(A,B):-length(A,B).
my_toupper9(A,B):-upcase_atom(A,B).
my_double10(N,M):-M is 2*N,M =< 10.
my_tolower11(A,B):-downcase_atom(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_lowercase3/1).
prim(my_min_list4/2).
prim(my_head5/2).
prim(my_flatten6/2).
prim(my_set7/1).
prim(my_len8/2).
prim(my_toupper9/2).
prim(my_double10/2).
prim(my_tolower11/2).
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
p([['h','y','U'],['u','U','U'],['I','f','Z','b']],[['h','y'],['u','U'],['I','f','Z']]).
p([['M','X','s','C'],['k','C','w','D'],['w','P','O'],['J','S','N','A']],[['M','X','s'],['k','C','w'],['w','P'],['J','S','N']]).
p([['V','S','B','N'],['H','F','p'],['s','c','d']],[['V','S','B'],['H','F'],['s','c']]).
p([['j','o','E','j'],['l','y','N'],['k','S','P']],[['j','o','E'],['l','y'],['k','S']]).
p([['e','J','Q','J'],['j','l','I']],[['e','J','Q'],['j','l']]).
q([['s','e','w'],['K','S','V','r']],[['s','e','w'],['K','S','V']]).
q([['S','K','H','g'],['A','l','I'],['R','s','R'],['q','F','A','E']],[['S','K','H','g'],['A','l','I'],['R','s'],['q','F','A','E']]).
q([['P','l','z'],['s','B','f'],['d','S','P']],[['P','l'],['s','B','f'],['d','S','P']]).
q([['K','E','r','m'],['z','M','z','R']],[['K','E','r','m'],['z','M','z']]).
q([['P','A','g'],['Y','W','L'],['b','I','D','G'],['z','X','y']],[['P','A','g'],['Y','W','L'],['b','I','D','G'],['z','X']]).
