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
my_uppercase4(A):-upcase_atom(A,A).
my_flatten5(A,B):-flatten(A,B).
my_set6(A):-list_to_set(A,A).
my_tolower7(A,B):-downcase_atom(A,B).
my_even8(A):-0 is A mod 2.
my_toupper9(A,B):-upcase_atom(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_min_list11(A,B):-min_list(A,B).
my_odd12(A):-1 is A mod 2.
my_max_list13(A,B):-max_list(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_list_to_set3/2).
prim(my_uppercase4/1).
prim(my_flatten5/2).
prim(my_set6/1).
prim(my_tolower7/2).
prim(my_even8/1).
prim(my_toupper9/2).
prim(my_pred10/2).
prim(my_min_list11/2).
prim(my_odd12/1).
prim(my_max_list13/2).
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
p([['E','t','x'],['u','j','u']],[['E','t'],['u','j']]).
p([['P','U','e'],['f','r','n','v'],['f','b','N'],['Q','d','m']],[['P','U'],['f','r','n'],['f','b'],['Q','d']]).
p([['P','y','E','P'],['r','A','y','N'],['m','d','N'],['J','w','h']],[['P','y','E'],['r','A','y'],['m','d'],['J','w']]).
p([['U','Y','H'],['V','v','w','g']],[['U','Y'],['V','v','w']]).
p([['r','Q','c','c'],['p','f','t'],['R','m','v']],[['r','Q','c'],['p','f'],['R','m']]).
q([['w','i','A','q'],['e','a','f','Y'],['f','o','Z','P'],['b','p','F','B']],[['w','i','A'],['e','a','f','Y'],['f','o','Z','P'],['b','p','F','B']]).
q([['h','p','u','g'],['w','v','W']],[['h','p','u'],['w','v','W']]).
q([['l','A','Q'],['E','n','B','U']],[['l','A','Q'],['E','n','B']]).
q([['Z','L','n'],['m','L','b']],[['Z','L','n'],['m','L']]).
q([['q','g','v','T'],['W','W','y','k']],[['q','g','v','T'],['W','W','y']]).
