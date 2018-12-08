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
my_even4(A):-0 is A mod 2.
my_msort5(A,B):-msort(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_odd7(A):-1 is A mod 2.
my_succ8(A,B):-succ(A,B),B =< 10.
my_flatten9(A,B):-flatten(A,B).
my_uppercase10(A):-upcase_atom(A,A).
my_list_to_set11(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_lowercase3/1).
prim(my_even4/1).
prim(my_msort5/2).
prim(my_double6/2).
prim(my_odd7/1).
prim(my_succ8/2).
prim(my_flatten9/2).
prim(my_uppercase10/1).
prim(my_list_to_set11/2).
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
p([['J','c','z','f'],['I','L','M','o'],['r','I','X','Q'],['U','O','i']],[['J','c','z'],['I','L','M'],['r','I','X'],['U','O']]).
p([['Q','U','s'],['I','t','D'],['F','Z','d']],[['Q','U'],['I','t'],['F','Z']]).
p([['X','o','A','c'],['T','Z','w'],['P','r','Y','g'],['I','i','O','u']],[['X','o','A'],['T','Z'],['P','r','Y'],['I','i','O']]).
p([['R','e','N'],['T','u','L','M'],['K','U','o']],[['R','e'],['T','u','L'],['K','U']]).
p([['r','V','w'],['A','j','H','f'],['R','F','Z','Y']],[['r','V'],['A','j','H'],['R','F','Z']]).
q([['g','U','I','R'],['V','q','N','L'],['h','c','Y']],[['g','U','I'],['V','q','N','L'],['h','c','Y']]).
q([['r','f','H','E'],['t','Y','J','f'],['c','Z','C']],[['r','f','H'],['t','Y','J','f'],['c','Z','C']]).
q([['X','F','J','h'],['Q','Q','e','P'],['E','l','B'],['j','D','b','Y']],[['X','F','J'],['Q','Q','e','P'],['E','l'],['j','D','b','Y']]).
q([['l','X','n','b'],['v','H','t','G'],['T','h','y'],['n','y','L']],[['l','X','n','b'],['v','H','t'],['T','h','y'],['n','y']]).
q([['Y','p','j'],['Q','l','O'],['t','p','u','N']],[['Y','p','j'],['Q','l'],['t','p','u','N']]).
