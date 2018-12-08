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
my_min_list4(A,B):-min_list(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_flatten6(A,B):-flatten(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_even8(A):-0 is A mod 2.
my_lowercase9(A):-downcase_atom(A,A).
my_last10(A,B):-last(A,B).
my_toupper11(A,B):-upcase_atom(A,B).

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

my_tolower13(A,B):-downcase_atom(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_set15(A):-list_to_set(A,A).
my_msort16(A,B):-msort(A,B).
my_element17(A,B):-member(B,A).
my_double18(N,M):-M is 2*N,M =< 10.
my_max_list19(A,B):-max_list(A,B).
my_odd20(A):-1 is A mod 2.
my_uppercase21(A):-upcase_atom(A,A).
my_pred22(A,B):-succ(B,A),A > 0.
my_len23(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_head3,[list(T),T]).
prim(my_min_list4,[list(int),int]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_succ7,[int,int]).
prim(my_even8,[int]).
prim(my_lowercase9,[char]).
prim(my_last10,[list(T),T]).
prim(my_toupper11,[char,char]).
prim(my_tolower13,[char,char]).
prim(my_sumlist14,[list(int),int]).
prim(my_set15,[list(_)]).
prim(my_msort16,[list(int),list(int)]).
prim(my_element17,[list(T),T]).
prim(my_double18,[int,int]).
prim(my_max_list19,[list(int),int]).
prim(my_odd20,[int]).
prim(my_uppercase21,[char]).
prim(my_pred22,[int,int]).
prim(my_len23,[list(_),int]).
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
p([['C','g','O'],['l','Y','Z','a'],['A','J','A']],[['C','g'],['l','Y','Z'],['A','J']]).
p([['x','E','r'],['I','X','X'],['A','u','e','L']],[['x','E'],['I','X'],['A','u','e']]).
p([['G','U','f','a'],['j','C','Z','o'],['Y','z','w','C'],['r','e','T']],[['G','U','f'],['j','C','Z'],['Y','z','w'],['r','e']]).
p([['w','w','P','I'],['d','U','Q']],[['w','w','P'],['d','U']]).
p([['W','q','k','f'],['Q','T','m','J']],[['W','q','k'],['Q','T','m']]).
q([['y','V','U','F'],['y','O','R'],['A','n','N','b'],['j','Z','w']],[['y','V','U','F'],['y','O','R'],['A','n','N'],['j','Z','w']]).
q([['c','V','l','Y'],['G','x','F'],['G','r','r'],['y','V','Z','w']],[['c','V','l'],['G','x','F'],['G','r','r'],['y','V','Z']]).
q([['b','p','V','u'],['o','r','p','Z'],['G','u','l'],['Z','u','z']],[['b','p','V'],['o','r','p','Z'],['G','u','l'],['Z','u','z']]).
q([['t','S','H'],['c','Y','u'],['J','D','f','D']],[['t','S'],['c','Y','u'],['J','D','f','D']]).
q([['u','d','s'],['J','T','F','x'],['p','G','O']],[['u','d','s'],['J','T','F'],['p','G','O']]).
