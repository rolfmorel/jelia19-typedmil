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

my_set4(A):-list_to_set(A,A).
my_even5(A):-0 is A mod 2.
my_lowercase6(A):-downcase_atom(A,A).
my_flatten7(A,B):-flatten(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_sumlist9(A,B):-sumlist(A,B).
my_last10(A,B):-last(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_element13(A,B):-member(B,A).
my_len14(A,B):-length(A,B).
my_tolower15(A,B):-downcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_set4,[list(_)]).
prim(my_even5,[int]).
prim(my_lowercase6,[char]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_pred8,[int,int]).
prim(my_sumlist9,[list(int),int]).
prim(my_last10,[list(T),T]).
prim(my_list_to_set11,[list(T),list(T)]).
prim(my_succ12,[int,int]).
prim(my_element13,[list(T),T]).
prim(my_len14,[list(_),int]).
prim(my_tolower15,[char,char]).
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
p([['P','P','m'],['E','K','H','w'],['k','t','D','M']],[['P','P'],['E','K','H'],['k','t','D']]).
p([['J','G','x','n'],['h','W','T'],['K','y','D'],['I','I','o','f']],[['J','G','x'],['h','W'],['K','y'],['I','I','o']]).
p([['b','p','x','D'],['H','M','C']],[['b','p','x'],['H','M']]).
p([['s','O','f','k'],['o','D','W'],['a','E','G','f']],[['s','O','f'],['o','D'],['a','E','G']]).
p([['X','p','j'],['q','N','v','R'],['D','c','U','n'],['R','t','i','i']],[['X','p'],['q','N','v'],['D','c','U'],['R','t','i']]).
q([['O','f','B','U'],['p','b','l'],['h','W','L']],[['O','f','B','U'],['p','b','l'],['h','W']]).
q([['H','X','P','H'],['u','n','k'],['j','J','y']],[['H','X','P','H'],['u','n'],['j','J','y']]).
q([['a','e','y'],['H','e','O','d']],[['a','e','y'],['H','e','O']]).
q([['y','y','C','w'],['Q','B','q','c'],['f','y','h','m']],[['y','y','C'],['Q','B','q','c'],['f','y','h','m']]).
q([['n','E','b','l'],['Q','H','x','Z'],['Q','U','N','U'],['A','v','l','W']],[['n','E','b','l'],['Q','H','x','Z'],['Q','U','N','U'],['A','v','l']]).
