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

my_len3(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_len3/2).
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
p([['y','D','R'],['C','H','o','e']],[['y','D'],['C','H','o']]).
p([['U','V','N'],['S','a','M','Z'],['U','O','t']],[['U','V'],['S','a','M'],['U','O']]).
p([['O','u','d'],['K','f','e']],[['O','u'],['K','f']]).
p([['B','j','z','v'],['x','q','h'],['g','S','p'],['K','g','x']],[['B','j','z'],['x','q'],['g','S'],['K','g']]).
p([['g','u','r','X'],['c','B','o','B'],['a','V','s']],[['g','u','r'],['c','B','o'],['a','V']]).
q([['Z','E','U'],['Z','K','N']],[['Z','E'],['Z','K','N']]).
q([['B','T','P','N'],['p','x','f']],[['B','T','P','N'],['p','x']]).
q([['E','F','K','G'],['m','s','y','U'],['m','E','g'],['z','M','U','g']],[['E','F','K','G'],['m','s','y','U'],['m','E','g'],['z','M','U']]).
q([['j','A','p','T'],['V','B','h','x'],['H','z','x','C'],['K','q','J','T']],[['j','A','p'],['V','B','h','x'],['H','z','x','C'],['K','q','J']]).
q([['C','U','T'],['y','F','E'],['U','h','i','s']],[['C','U'],['y','F','E'],['U','h','i','s']]).
