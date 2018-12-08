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
my_element4(A,B):-member(B,A).
my_pred5(A,B):-succ(B,A),A > 0.
my_msort6(A,B):-msort(A,B).

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
prim(my_uppercase3,[char]).
prim(my_element4,[list(T),T]).
prim(my_pred5,[int,int]).
prim(my_msort6,[list(int),list(int)]).
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
p([['B','f','r','l'],['w','R','L'],['K','B','D','i'],['Y','N','B']],[['B','f','r'],['w','R'],['K','B','D'],['Y','N']]).
p([['N','S','V','f'],['u','H','m','x'],['F','g','o','H']],[['N','S','V'],['u','H','m'],['F','g','o']]).
p([['X','T','P'],['A','k','L','C'],['u','W','J']],[['X','T'],['A','k','L'],['u','W']]).
p([['b','f','i'],['h','o','K','R'],['E','E','y']],[['b','f'],['h','o','K'],['E','E']]).
p([['l','d','A','E'],['o','C','B','i'],['X','x','I','V'],['O','k','H','Y']],[['l','d','A'],['o','C','B'],['X','x','I'],['O','k','H']]).
q([['L','i','P'],['V','K','P','t']],[['L','i','P'],['V','K','P']]).
q([['M','j','P','x'],['e','c','c']],[['M','j','P'],['e','c','c']]).
q([['c','Q','X'],['S','m','M','w'],['y','M','g','N'],['l','z','k','r']],[['c','Q','X'],['S','m','M','w'],['y','M','g'],['l','z','k','r']]).
q([['W','p','J'],['D','M','U'],['s','S','Z'],['V','k','H','W']],[['W','p'],['D','M','U'],['s','S','Z'],['V','k','H','W']]).
q([['O','V','L','l'],['U','p','N','m'],['P','f','E','d']],[['O','V','L','l'],['U','p','N'],['P','f','E','d']]).
