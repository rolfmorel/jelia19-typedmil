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

my_tolower3(A,B):-downcase_atom(A,B).
my_succ4(A,B):-succ(A,B),B =< 10.

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

my_set6(A):-list_to_set(A,A).
my_lowercase7(A):-downcase_atom(A,A).
my_last8(A,B):-last(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_min_list10(A,B):-min_list(A,B).
my_toupper11(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_tolower3/2).
prim(my_succ4/2).
prim(my_set6/1).
prim(my_lowercase7/1).
prim(my_last8/2).
prim(my_pred9/2).
prim(my_min_list10/2).
prim(my_toupper11/2).
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
p([['R','b','W'],['z','f','c','A']],[['R','b'],['z','f','c']]).
p([['g','L','G','p'],['E','j','J','d'],['n','x','N'],['k','K','v']],[['g','L','G'],['E','j','J'],['n','x'],['k','K']]).
p([['Q','E','g','s'],['L','k','h']],[['Q','E','g'],['L','k']]).
p([['V','f','Z','b'],['R','R','w','S'],['f','f','Q']],[['V','f','Z'],['R','R','w'],['f','f']]).
p([['n','K','Q','e'],['u','p','S','p']],[['n','K','Q'],['u','p','S']]).
q([['P','J','B','t'],['i','L','r'],['x','c','u']],[['P','J','B','t'],['i','L','r'],['x','c']]).
q([['Y','l','q'],['I','C','O'],['W','L','E']],[['Y','l','q'],['I','C','O'],['W','L']]).
q([['G','Z','A'],['x','L','R'],['B','E','B','a']],[['G','Z','A'],['x','L'],['B','E','B','a']]).
q([['I','E','b'],['l','q','r','n'],['D','z','q','P']],[['I','E','b'],['l','q','r'],['D','z','q','P']]).
q([['j','R','G'],['L','q','y','m']],[['j','R'],['L','q','y','m']]).
