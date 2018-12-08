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

my_odd3(A):-1 is A mod 2.
my_succ4(A,B):-succ(A,B),B =< 10.
my_even5(A):-0 is A mod 2.
my_lowercase6(A):-downcase_atom(A,A).
my_last7(A,B):-last(A,B).
my_len8(A,B):-length(A,B).
my_head9([H|_],H).
my_double10(N,M):-M is 2*N,M =< 10.
my_min_list11(A,B):-min_list(A,B).
my_set12(A):-list_to_set(A,A).
my_sumlist13(A,B):-sumlist(A,B).
my_list_to_set14(A,B):-list_to_set(A,B).
my_max_list15(A,B):-max_list(A,B).
my_pred16(A,B):-succ(B,A),A > 0.
my_element17(A,B):-member(B,A).

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

my_toupper19(A,B):-upcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_odd3,[int]).
prim(my_succ4,[int,int]).
prim(my_even5,[int]).
prim(my_lowercase6,[char]).
prim(my_last7,[list(T),T]).
prim(my_len8,[list(_),int]).
prim(my_head9,[list(T),T]).
prim(my_double10,[int,int]).
prim(my_min_list11,[list(int),int]).
prim(my_set12,[list(_)]).
prim(my_sumlist13,[list(int),int]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_max_list15,[list(int),int]).
prim(my_pred16,[int,int]).
prim(my_element17,[list(T),T]).
prim(my_toupper19,[char,char]).
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
p([['a','i','i'],['N','A','d'],['A','I','W','F']],[['a','i'],['N','A'],['A','I','W']]).
p([['H','v','N'],['R','Q','i'],['Y','I','b']],[['H','v'],['R','Q'],['Y','I']]).
p([['E','Z','k','P'],['X','X','Q']],[['E','Z','k'],['X','X']]).
p([['c','a','u','o'],['p','u','E'],['k','A','G','S'],['B','i','m']],[['c','a','u'],['p','u'],['k','A','G'],['B','i']]).
p([['e','Z','s','E'],['M','V','T'],['X','C','T','O'],['D','l','C']],[['e','Z','s'],['M','V'],['X','C','T'],['D','l']]).
q([['Z','k','o'],['X','z','M','x'],['Q','r','o','g'],['f','f','l','w']],[['Z','k','o'],['X','z','M','x'],['Q','r','o'],['f','f','l','w']]).
q([['U','w','k'],['p','B','O']],[['U','w'],['p','B','O']]).
q([['r','W','H'],['B','G','v']],[['r','W'],['B','G','v']]).
q([['p','N','O','d'],['I','v','A'],['u','a','W'],['d','Q','L','H']],[['p','N','O'],['I','v','A'],['u','a','W'],['d','Q','L']]).
q([['P','p','B'],['O','T','L'],['D','W','a']],[['P','p','B'],['O','T'],['D','W','a']]).
