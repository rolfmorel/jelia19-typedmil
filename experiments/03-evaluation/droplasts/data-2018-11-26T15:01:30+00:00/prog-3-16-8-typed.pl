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

my_toupper3(A,B):-upcase_atom(A,B).

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

my_head5([H|_],H).
my_flatten6(A,B):-flatten(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_msort8(A,B):-msort(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_lowercase10(A):-downcase_atom(A,A).
my_set11(A):-list_to_set(A,A).
my_sumlist12(A,B):-sumlist(A,B).
my_pred13(A,B):-succ(B,A),A > 0.
my_min_list14(A,B):-min_list(A,B).
my_even15(A):-0 is A mod 2.
my_last16(A,B):-last(A,B).
my_uppercase17(A):-upcase_atom(A,A).
my_max_list18(A,B):-max_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_toupper3,[char,char]).
prim(my_head5,[list(T),T]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_msort8,[list(int),list(int)]).
prim(my_succ9,[int,int]).
prim(my_lowercase10,[char]).
prim(my_set11,[list(_)]).
prim(my_sumlist12,[list(int),int]).
prim(my_pred13,[int,int]).
prim(my_min_list14,[list(int),int]).
prim(my_even15,[int]).
prim(my_last16,[list(T),T]).
prim(my_uppercase17,[char]).
prim(my_max_list18,[list(int),int]).
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
p([['Q','f','W','U'],['Y','d','W']],[['Q','f','W'],['Y','d']]).
p([['A','A','P','s'],['b','q','n','V']],[['A','A','P'],['b','q','n']]).
p([['U','R','S','H'],['a','f','a'],['C','V','y']],[['U','R','S'],['a','f'],['C','V']]).
p([['U','y','Q'],['S','A','Q']],[['U','y'],['S','A']]).
p([['t','Z','l','L'],['o','n','G'],['y','Y','N','y'],['z','p','N','w']],[['t','Z','l'],['o','n'],['y','Y','N'],['z','p','N']]).
q([['S','l','C','y'],['Z','B','l'],['Z','v','l']],[['S','l','C'],['Z','B','l'],['Z','v','l']]).
q([['f','J','R'],['f','v','r']],[['f','J'],['f','v','r']]).
q([['p','U','V','B'],['Z','A','A'],['c','J','f','p'],['P','s','x','V']],[['p','U','V','B'],['Z','A','A'],['c','J','f','p'],['P','s','x']]).
q([['h','H','N'],['j','d','r'],['S','A','L','p']],[['h','H','N'],['j','d','r'],['S','A','L']]).
q([['J','f','V','m'],['k','J','b','G'],['q','b','R','x'],['S','C','W','c']],[['J','f','V'],['k','J','b','G'],['q','b','R','x'],['S','C','W','c']]).
