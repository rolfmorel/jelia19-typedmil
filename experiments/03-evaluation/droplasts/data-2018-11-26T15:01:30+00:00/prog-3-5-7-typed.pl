:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
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
my_list_to_set4(A,B):-list_to_set(A,B).
my_msort5(A,B):-msort(A,B).
my_max_list6(A,B):-max_list(A,B).

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

prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_head3,[list(T),T]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_msort5,[list(int),list(int)]).
prim(my_max_list6,[list(int),int]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(list(char)),list(list(char))],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([['D','J','a'],['D','V','X'],['A','J','l'],['l','k','w']],[['D','J'],['D','V'],['A','J'],['l','k']]).
p([['y','g','E'],['u','F','d','x']],[['y','g'],['u','F','d']]).
p([['T','L','F'],['M','L','G','O'],['d','m','y'],['F','z','n','c']],[['T','L'],['M','L','G'],['d','m'],['F','z','n']]).
p([['s','Q','Q'],['W','Y','F'],['m','t','X']],[['s','Q'],['W','Y'],['m','t']]).
p([['U','x','q','B'],['n','m','i'],['S','y','G','W'],['D','a','j','R']],[['U','x','q'],['n','m'],['S','y','G'],['D','a','j']]).
q([['t','K','V','N'],['m','C','G']],[['t','K','V'],['m','C','G']]).
q([['K','U','c'],['d','w','z'],['A','u','l']],[['K','U','c'],['d','w','z'],['A','u']]).
q([['v','j','B','w'],['p','e','z','o'],['e','y','u','U']],[['v','j','B','w'],['p','e','z'],['e','y','u','U']]).
q([['s','o','X','s'],['Y','O','G'],['x','e','t','i']],[['s','o','X','s'],['Y','O'],['x','e','t','i']]).
q([['Z','Y','q','K'],['O','x','n'],['m','Q','y','s'],['a','F','y','h']],[['Z','Y','q','K'],['O','x'],['m','Q','y','s'],['a','F','y']]).
