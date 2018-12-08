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

my_max_list3(A,B):-max_list(A,B).
my_double4(N,M):-M is 2*N,M =< 10.
my_list_to_set5(A,B):-list_to_set(A,B).
my_set6(A):-list_to_set(A,A).
my_lowercase7(A):-downcase_atom(A,A).

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

my_sumlist9(A,B):-sumlist(A,B).
my_even10(A):-0 is A mod 2.
my_msort11(A,B):-msort(A,B).
my_element12(A,B):-member(B,A).
my_len13(A,B):-length(A,B).
my_toupper14(A,B):-upcase_atom(A,B).
my_pred15(A,B):-succ(B,A),A > 0.
my_head16([H|_],H).
my_uppercase17(A):-upcase_atom(A,A).
my_succ18(A,B):-succ(A,B),B =< 10.
my_min_list19(A,B):-min_list(A,B).
my_last20(A,B):-last(A,B).
my_odd21(A):-1 is A mod 2.
my_tolower22(A,B):-downcase_atom(A,B).
my_flatten23(A,B):-flatten(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_max_list3,[list(int),int]).
prim(my_double4,[int,int]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_set6,[list(_)]).
prim(my_lowercase7,[char]).
prim(my_sumlist9,[list(int),int]).
prim(my_even10,[int]).
prim(my_msort11,[list(int),list(int)]).
prim(my_element12,[list(T),T]).
prim(my_len13,[list(_),int]).
prim(my_toupper14,[char,char]).
prim(my_pred15,[int,int]).
prim(my_head16,[list(T),T]).
prim(my_uppercase17,[char]).
prim(my_succ18,[int,int]).
prim(my_min_list19,[list(int),int]).
prim(my_last20,[list(T),T]).
prim(my_odd21,[int]).
prim(my_tolower22,[char,char]).
prim(my_flatten23,[list(list(T)),list(T)]).
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
p([['C','L','p','H'],['l','p','F','E'],['v','j','l']],[['C','L','p'],['l','p','F'],['v','j']]).
p([['z','M','e'],['L','e','S'],['f','M','N']],[['z','M'],['L','e'],['f','M']]).
p([['O','S','E'],['W','Q','f']],[['O','S'],['W','Q']]).
p([['Z','G','s'],['T','z','y','t'],['P','k','F','r'],['m','n','H','y']],[['Z','G'],['T','z','y'],['P','k','F'],['m','n','H']]).
p([['b','O','x','N'],['d','r','U','N']],[['b','O','x'],['d','r','U']]).
q([['a','m','Z'],['m','m','X']],[['a','m','Z'],['m','m']]).
q([['T','I','d','J'],['e','m','R','B']],[['T','I','d','J'],['e','m','R']]).
q([['L','u','E','G'],['Z','n','O','f'],['g','r','I']],[['L','u','E'],['Z','n','O','f'],['g','r','I']]).
q([['V','o','Z'],['Y','d','z','u'],['G','J','F']],[['V','o','Z'],['Y','d','z'],['G','J','F']]).
q([['c','q','m','K'],['q','m','x'],['s','l','q','i'],['c','I','g','U']],[['c','q','m','K'],['q','m'],['s','l','q','i'],['c','I','g','U']]).
