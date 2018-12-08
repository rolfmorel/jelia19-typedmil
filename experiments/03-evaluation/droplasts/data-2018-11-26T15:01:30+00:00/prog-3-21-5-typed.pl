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

my_msort3(A,B):-msort(A,B).
my_list_to_set4(A,B):-list_to_set(A,B).
my_odd5(A):-1 is A mod 2.
my_len6(A,B):-length(A,B).
my_lowercase7(A):-downcase_atom(A,A).
my_tolower8(A,B):-downcase_atom(A,B).
my_uppercase9(A):-upcase_atom(A,A).
my_last10(A,B):-last(A,B).
my_flatten11(A,B):-flatten(A,B).
my_set12(A):-list_to_set(A,A).
my_even13(A):-0 is A mod 2.
my_element14(A,B):-member(B,A).
my_pred15(A,B):-succ(B,A),A > 0.
my_min_list16(A,B):-min_list(A,B).
my_head17([H|_],H).
my_toupper18(A,B):-upcase_atom(A,B).
my_succ19(A,B):-succ(A,B),B =< 10.
my_sumlist20(A,B):-sumlist(A,B).
my_double21(N,M):-M is 2*N,M =< 10.

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

my_max_list23(A,B):-max_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_msort3,[list(int),list(int)]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_odd5,[int]).
prim(my_len6,[list(_),int]).
prim(my_lowercase7,[char]).
prim(my_tolower8,[char,char]).
prim(my_uppercase9,[char]).
prim(my_last10,[list(T),T]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_set12,[list(_)]).
prim(my_even13,[int]).
prim(my_element14,[list(T),T]).
prim(my_pred15,[int,int]).
prim(my_min_list16,[list(int),int]).
prim(my_head17,[list(T),T]).
prim(my_toupper18,[char,char]).
prim(my_succ19,[int,int]).
prim(my_sumlist20,[list(int),int]).
prim(my_double21,[int,int]).
prim(my_max_list23,[list(int),int]).
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
p([['z','s','k'],['I','p','f','h'],['X','n','V','S']],[['z','s'],['I','p','f'],['X','n','V']]).
p([['X','x','Q'],['H','b','o'],['Q','v','W'],['D','F','s']],[['X','x'],['H','b'],['Q','v'],['D','F']]).
p([['l','J','H','r'],['j','x','n','y'],['w','T','e']],[['l','J','H'],['j','x','n'],['w','T']]).
p([['L','o','n'],['l','B','X'],['q','r','l']],[['L','o'],['l','B'],['q','r']]).
p([['X','y','b','s'],['s','l','R'],['s','N','D','n']],[['X','y','b'],['s','l'],['s','N','D']]).
q([['B','c','O'],['Z','J','t']],[['B','c','O'],['Z','J']]).
q([['w','C','R'],['g','I','L','E'],['o','r','i'],['d','y','M']],[['w','C','R'],['g','I','L','E'],['o','r'],['d','y','M']]).
q([['R','D','X'],['f','e','A','R'],['L','J','D','h'],['K','L','V']],[['R','D','X'],['f','e','A'],['L','J','D','h'],['K','L']]).
q([['M','A','y'],['Z','q','E','c'],['J','E','V','O']],[['M','A','y'],['Z','q','E'],['J','E','V','O']]).
q([['x','c','Q','a'],['s','W','k','z'],['q','n','s']],[['x','c','Q','a'],['s','W','k','z'],['q','n']]).
