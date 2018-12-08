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
my_tolower4(A,B):-downcase_atom(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_uppercase3/1).
prim(my_tolower4/2).
prim(my_pred5/2).
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
p([['J','o','i','U'],['k','U','w','Z']],[['J','o','i'],['k','U','w']]).
p([['V','t','P','b'],['D','J','e','m'],['u','B','V','w']],[['V','t','P'],['D','J','e'],['u','B','V']]).
p([['G','f','P'],['O','r','S'],['S','G','E','h'],['E','Q','B']],[['G','f'],['O','r'],['S','G','E'],['E','Q']]).
p([['x','G','x'],['H','m','M']],[['x','G'],['H','m']]).
p([['f','u','o','p'],['r','W','J','Y'],['T','s','H']],[['f','u','o'],['r','W','J'],['T','s']]).
q([['P','o','Z','U'],['y','x','l'],['y','r','G','j']],[['P','o','Z','U'],['y','x'],['y','r','G','j']]).
q([['k','z','O','e'],['d','c','S','Z'],['a','g','S','n']],[['k','z','O','e'],['d','c','S'],['a','g','S','n']]).
q([['G','s','E','A'],['j','p','e','j'],['Z','k','N'],['U','T','m','f']],[['G','s','E'],['j','p','e','j'],['Z','k','N'],['U','T','m']]).
q([['T','w','F','M'],['K','q','S','D']],[['T','w','F'],['K','q','S','D']]).
q([['l','G','j','l'],['k','Z','k','U'],['A','r','U','R']],[['l','G','j'],['k','Z','k','U'],['A','r','U','R']]).
