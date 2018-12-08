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

my_element3(A,B):-member(B,A).
my_last4(A,B):-last(A,B).
my_even5(A):-0 is A mod 2.
my_pred6(A,B):-succ(B,A),A > 0.
my_head7([H|_],H).
my_list_to_set8(A,B):-list_to_set(A,B).
my_odd9(A):-1 is A mod 2.
my_len10(A,B):-length(A,B).
my_double11(N,M):-M is 2*N,M =< 10.
my_msort12(A,B):-msort(A,B).
my_tolower13(A,B):-downcase_atom(A,B).
my_flatten14(A,B):-flatten(A,B).

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

my_max_list16(A,B):-max_list(A,B).
my_uppercase17(A):-upcase_atom(A,A).
my_toupper18(A,B):-upcase_atom(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_min_list20(A,B):-min_list(A,B).
my_set21(A):-list_to_set(A,A).
my_succ22(A,B):-succ(A,B),B =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_element3,[list(T),T]).
prim(my_last4,[list(T),T]).
prim(my_even5,[int]).
prim(my_pred6,[int,int]).
prim(my_head7,[list(T),T]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_odd9,[int]).
prim(my_len10,[list(_),int]).
prim(my_double11,[int,int]).
prim(my_msort12,[list(int),list(int)]).
prim(my_tolower13,[char,char]).
prim(my_flatten14,[list(list(T)),list(T)]).
prim(my_max_list16,[list(int),int]).
prim(my_uppercase17,[char]).
prim(my_toupper18,[char,char]).
prim(my_sumlist19,[list(int),int]).
prim(my_min_list20,[list(int),int]).
prim(my_set21,[list(_)]).
prim(my_succ22,[int,int]).
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
p([['M','r','Z','Y'],['c','A','U','M'],['a','x','f','m']],[['M','r','Z'],['c','A','U'],['a','x','f']]).
p([['r','c','D'],['C','J','A','d']],[['r','c'],['C','J','A']]).
p([['O','e','k'],['t','d','Q'],['d','I','L','m']],[['O','e'],['t','d'],['d','I','L']]).
p([['X','b','y'],['p','S','n','R'],['L','M','R']],[['X','b'],['p','S','n'],['L','M']]).
p([['o','q','P'],['U','p','E','t']],[['o','q'],['U','p','E']]).
q([['S','Z','U','y'],['t','K','h','W'],['u','W','d'],['y','L','w']],[['S','Z','U'],['t','K','h','W'],['u','W','d'],['y','L','w']]).
q([['P','s','N','N'],['W','a','B'],['L','W','R'],['g','n','x','L']],[['P','s','N','N'],['W','a','B'],['L','W'],['g','n','x']]).
q([['Q','b','B','l'],['w','X','P'],['d','T','O','K'],['i','o','O','Q']],[['Q','b','B'],['w','X','P'],['d','T','O','K'],['i','o','O']]).
q([['N','L','x'],['E','A','J','n']],[['N','L','x'],['E','A','J']]).
q([['D','z','Y','q'],['M','B','i'],['n','r','D']],[['D','z','Y'],['M','B','i'],['n','r','D']]).
