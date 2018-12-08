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

my_uppercase3(A):-upcase_atom(A,A).
my_tolower4(A,B):-downcase_atom(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
my_succ6(A,B):-succ(A,B),B =< 10.

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

my_element8(A,B):-member(B,A).
my_set9(A):-list_to_set(A,A).
my_max_list10(A,B):-max_list(A,B).
my_head11([H|_],H).
my_even12(A):-0 is A mod 2.
my_odd13(A):-1 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_uppercase3,[char]).
prim(my_tolower4,[char,char]).
prim(my_pred5,[int,int]).
prim(my_succ6,[int,int]).
prim(my_element8,[list(T),T]).
prim(my_set9,[list(_)]).
prim(my_max_list10,[list(int),int]).
prim(my_head11,[list(T),T]).
prim(my_even12,[int]).
prim(my_odd13,[int]).
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
p([['D','h','Z'],['p','q','p','c'],['I','x','a','r'],['O','Y','D','V']],[['D','h'],['p','q','p'],['I','x','a'],['O','Y','D']]).
p([['L','z','C','c'],['K','b','I','d']],[['L','z','C'],['K','b','I']]).
p([['E','h','n','q'],['Q','M','v','a'],['N','v','R']],[['E','h','n'],['Q','M','v'],['N','v']]).
p([['i','y','D'],['z','E','O','P'],['Q','B','J','P'],['s','w','P','q']],[['i','y'],['z','E','O'],['Q','B','J'],['s','w','P']]).
p([['B','y','f','Z'],['D','j','P'],['H','z','w','d']],[['B','y','f'],['D','j'],['H','z','w']]).
q([['A','L','L','J'],['X','F','B','U'],['m','H','W'],['p','O','P']],[['A','L','L','J'],['X','F','B'],['m','H','W'],['p','O','P']]).
q([['Y','W','S','m'],['D','j','w','r']],[['Y','W','S'],['D','j','w','r']]).
q([['v','G','Q'],['W','f','z']],[['v','G','Q'],['W','f']]).
q([['v','j','s'],['h','e','d'],['Q','Q','R']],[['v','j','s'],['h','e'],['Q','Q','R']]).
q([['M','b','O'],['s','b','U','A']],[['M','b','O'],['s','b','U']]).
