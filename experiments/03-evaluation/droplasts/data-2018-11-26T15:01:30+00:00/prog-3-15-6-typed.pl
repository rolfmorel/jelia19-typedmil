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

my_succ3(A,B):-succ(A,B),B =< 10.
my_toupper4(A,B):-upcase_atom(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
my_head6([H|_],H).
my_len7(A,B):-length(A,B).
my_last8(A,B):-last(A,B).
my_min_list9(A,B):-min_list(A,B).
my_odd10(A):-1 is A mod 2.
my_element11(A,B):-member(B,A).
my_sumlist12(A,B):-sumlist(A,B).
my_max_list13(A,B):-max_list(A,B).
my_uppercase14(A):-upcase_atom(A,A).
my_set15(A):-list_to_set(A,A).
my_even16(A):-0 is A mod 2.

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
prim(my_succ3,[int,int]).
prim(my_toupper4,[char,char]).
prim(my_pred5,[int,int]).
prim(my_head6,[list(T),T]).
prim(my_len7,[list(_),int]).
prim(my_last8,[list(T),T]).
prim(my_min_list9,[list(int),int]).
prim(my_odd10,[int]).
prim(my_element11,[list(T),T]).
prim(my_sumlist12,[list(int),int]).
prim(my_max_list13,[list(int),int]).
prim(my_uppercase14,[char]).
prim(my_set15,[list(_)]).
prim(my_even16,[int]).
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
p([['B','X','C'],['s','B','e']],[['B','X'],['s','B']]).
p([['S','J','Q'],['F','U','k','A']],[['S','J'],['F','U','k']]).
p([['H','K','Z'],['T','z','t','g'],['N','d','u','E'],['t','s','A']],[['H','K'],['T','z','t'],['N','d','u'],['t','s']]).
p([['u','D','Z'],['d','O','W','x']],[['u','D'],['d','O','W']]).
p([['n','O','N','B'],['l','G','f'],['e','q','Z','n'],['r','k','l','m']],[['n','O','N'],['l','G'],['e','q','Z'],['r','k','l']]).
q([['D','o','m'],['y','p','H','Q']],[['D','o','m'],['y','p','H']]).
q([['P','Z','X'],['d','x','F','C']],[['P','Z','X'],['d','x','F']]).
q([['k','I','m','O'],['J','g','z','S']],[['k','I','m','O'],['J','g','z']]).
q([['O','m','U'],['r','i','d'],['c','b','r'],['r','L','n','J']],[['O','m'],['r','i'],['c','b','r'],['r','L','n','J']]).
q([['v','L','W','D'],['g','Q','q'],['x','T','C']],[['v','L','W','D'],['g','Q','q'],['x','T']]).
