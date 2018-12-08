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

my_even3(A):-0 is A mod 2.
my_max_list4(A,B):-max_list(A,B).
my_flatten5(A,B):-flatten(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_len7(A,B):-length(A,B).
my_tolower8(A,B):-downcase_atom(A,B).
my_odd9(A):-1 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_even3/1).
prim(my_max_list4/2).
prim(my_flatten5/2).
prim(my_lowercase6/1).
prim(my_len7/2).
prim(my_tolower8/2).
prim(my_odd9/1).
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
p([['V','Y','e'],['e','v','g','B']],[['V','Y'],['e','v','g']]).
p([['z','B','f','i'],['c','b','C','v'],['n','g','N']],[['z','B','f'],['c','b','C'],['n','g']]).
p([['p','t','a','n'],['z','H','T','c'],['Y','n','P'],['i','x','b','F']],[['p','t','a'],['z','H','T'],['Y','n'],['i','x','b']]).
p([['Y','A','R'],['i','Q','Y','U'],['n','a','S'],['r','F','t','N']],[['Y','A'],['i','Q','Y'],['n','a'],['r','F','t']]).
p([['q','H','d','C'],['w','f','R','f'],['v','r','K','M'],['i','A','m']],[['q','H','d'],['w','f','R'],['v','r','K'],['i','A']]).
q([['c','d','S'],['o','q','U','V']],[['c','d','S'],['o','q','U']]).
q([['c','O','o','z'],['y','E','t'],['u','H','L','c']],[['c','O','o','z'],['y','E'],['u','H','L','c']]).
q([['z','G','l','M'],['B','x','X'],['c','w','k','U'],['F','d','e','P']],[['z','G','l','M'],['B','x'],['c','w','k','U'],['F','d','e']]).
q([['C','D','K'],['K','f','e'],['n','U','t','z']],[['C','D','K'],['K','f'],['n','U','t','z']]).
q([['n','g','c'],['P','X','Z'],['x','i','Z']],[['n','g','c'],['P','X','Z'],['x','i']]).
