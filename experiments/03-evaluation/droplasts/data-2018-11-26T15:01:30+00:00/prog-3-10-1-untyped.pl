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

my_flatten3(A,B):-flatten(A,B).
my_pred4(A,B):-succ(B,A),A > 0.
my_odd5(A):-1 is A mod 2.
my_head6([H|_],H).
my_uppercase7(A):-upcase_atom(A,A).
my_sumlist8(A,B):-sumlist(A,B).
my_last9(A,B):-last(A,B).
my_len10(A,B):-length(A,B).

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

my_even12(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_flatten3/2).
prim(my_pred4/2).
prim(my_odd5/1).
prim(my_head6/2).
prim(my_uppercase7/1).
prim(my_sumlist8/2).
prim(my_last9/2).
prim(my_len10/2).
prim(my_even12/1).
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
p([['q','T','D','S'],['v','q','B'],['R','H','m','f'],['e','D','C','D']],[['q','T','D'],['v','q'],['R','H','m'],['e','D','C']]).
p([['G','R','p'],['G','a','m','s']],[['G','R'],['G','a','m']]).
p([['v','J','i'],['n','F','p'],['i','o','K']],[['v','J'],['n','F'],['i','o']]).
p([['e','U','f','U'],['x','r','W'],['Y','R','S','F'],['t','I','m']],[['e','U','f'],['x','r'],['Y','R','S'],['t','I']]).
p([['Z','l','d','S'],['X','T','j','u'],['Y','l','D','v'],['j','V','t']],[['Z','l','d'],['X','T','j'],['Y','l','D'],['j','V']]).
q([['p','c','b'],['r','q','m','Z']],[['p','c','b'],['r','q','m']]).
q([['j','V','D'],['P','e','w'],['J','e','A'],['F','s','F']],[['j','V','D'],['P','e','w'],['J','e'],['F','s']]).
q([['B','F','t','d'],['P','P','A']],[['B','F','t','d'],['P','P']]).
q([['u','M','j','S'],['t','t','R'],['p','V','Z'],['c','j','l']],[['u','M','j'],['t','t','R'],['p','V'],['c','j','l']]).
q([['S','k','H','s'],['l','B','n'],['X','r','r']],[['S','k','H','s'],['l','B','n'],['X','r']]).
