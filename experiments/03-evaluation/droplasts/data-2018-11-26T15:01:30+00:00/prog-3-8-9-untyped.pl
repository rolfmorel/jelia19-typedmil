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

my_toupper3(A,B):-upcase_atom(A,B).
my_uppercase4(A):-upcase_atom(A,A).
my_element5(A,B):-member(B,A).
my_double6(N,M):-M is 2*N,M =< 10.
my_last7(A,B):-last(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_odd9(A):-1 is A mod 2.
my_lowercase10(A):-downcase_atom(A,A).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_toupper3/2).
prim(my_uppercase4/1).
prim(my_element5/2).
prim(my_double6/2).
prim(my_last7/2).
prim(my_pred8/2).
prim(my_odd9/1).
prim(my_lowercase10/1).
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
p([['b','q','E'],['Z','z','C']],[['b','q'],['Z','z']]).
p([['W','j','o'],['z','f','L'],['S','r','r','N'],['L','X','W','Y']],[['W','j'],['z','f'],['S','r','r'],['L','X','W']]).
p([['k','p','a','i'],['z','s','i']],[['k','p','a'],['z','s']]).
p([['D','H','R','H'],['j','F','w','u']],[['D','H','R'],['j','F','w']]).
p([['F','s','C','z'],['I','m','g'],['m','p','E'],['B','c','e']],[['F','s','C'],['I','m'],['m','p'],['B','c']]).
q([['W','S','z'],['t','W','I','p'],['L','z','g'],['w','u','s','a']],[['W','S'],['t','W','I','p'],['L','z','g'],['w','u','s','a']]).
q([['P','V','j','F'],['A','C','l'],['j','M','o'],['C','L','d']],[['P','V','j','F'],['A','C'],['j','M','o'],['C','L','d']]).
q([['J','R','n','C'],['n','H','L','c']],[['J','R','n'],['n','H','L','c']]).
q([['p','m','w','P'],['I','N','H'],['a','h','C']],[['p','m','w'],['I','N','H'],['a','h','C']]).
q([['i','Q','b','R'],['M','j','y','M'],['J','O','h'],['b','O','P']],[['i','Q','b','R'],['M','j','y','M'],['J','O','h'],['b','O']]).
