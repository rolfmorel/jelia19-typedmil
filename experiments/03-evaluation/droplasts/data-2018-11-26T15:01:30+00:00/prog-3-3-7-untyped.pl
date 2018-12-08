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

my_head3([H|_],H).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).

my_tolower5(A,B):-downcase_atom(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_head3/2).
prim(my_tolower5/2).
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
p([['Y','G','n','b'],['v','v','P'],['T','A','j']],[['Y','G','n'],['v','v'],['T','A']]).
p([['g','s','o'],['O','A','x','g'],['y','U','j','H']],[['g','s'],['O','A','x'],['y','U','j']]).
p([['f','e','r'],['o','d','R'],['n','l','A','X']],[['f','e'],['o','d'],['n','l','A']]).
p([['n','b','p'],['e','d','b'],['j','k','b','N'],['q','O','b','L']],[['n','b'],['e','d'],['j','k','b'],['q','O','b']]).
p([['c','A','a','f'],['K','s','N','g'],['q','U','b','v'],['C','i','u','R']],[['c','A','a'],['K','s','N'],['q','U','b'],['C','i','u']]).
q([['a','L','q','s'],['w','Z','r','P'],['R','R','J'],['h','w','S','t']],[['a','L','q','s'],['w','Z','r','P'],['R','R'],['h','w','S']]).
q([['L','e','c','V'],['n','t','Q','q'],['C','c','e'],['c','n','e','E']],[['L','e','c'],['n','t','Q','q'],['C','c','e'],['c','n','e']]).
q([['v','F','N'],['G','D','I']],[['v','F','N'],['G','D']]).
q([['S','h','B','j'],['P','T','U']],[['S','h','B'],['P','T','U']]).
q([['U','J','h'],['B','C','D']],[['U','J','h'],['B','C']]).
