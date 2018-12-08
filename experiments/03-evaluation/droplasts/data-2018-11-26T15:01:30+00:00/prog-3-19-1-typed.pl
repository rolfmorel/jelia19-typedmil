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

my_len3(A,B):-length(A,B).
my_tolower4(A,B):-downcase_atom(A,B).
my_set5(A):-list_to_set(A,A).
my_list_to_set6(A,B):-list_to_set(A,B).

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

my_even8(A):-0 is A mod 2.
my_msort9(A,B):-msort(A,B).
my_max_list10(A,B):-max_list(A,B).
my_succ11(A,B):-succ(A,B),B =< 10.
my_double12(N,M):-M is 2*N,M =< 10.
my_uppercase13(A):-upcase_atom(A,A).
my_flatten14(A,B):-flatten(A,B).
my_pred15(A,B):-succ(B,A),A > 0.
my_min_list16(A,B):-min_list(A,B).
my_last17(A,B):-last(A,B).
my_head18([H|_],H).
my_toupper19(A,B):-upcase_atom(A,B).
my_element20(A,B):-member(B,A).
my_odd21(A):-1 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_len3,[list(_),int]).
prim(my_tolower4,[char,char]).
prim(my_set5,[list(_)]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_even8,[int]).
prim(my_msort9,[list(int),list(int)]).
prim(my_max_list10,[list(int),int]).
prim(my_succ11,[int,int]).
prim(my_double12,[int,int]).
prim(my_uppercase13,[char]).
prim(my_flatten14,[list(list(T)),list(T)]).
prim(my_pred15,[int,int]).
prim(my_min_list16,[list(int),int]).
prim(my_last17,[list(T),T]).
prim(my_head18,[list(T),T]).
prim(my_toupper19,[char,char]).
prim(my_element20,[list(T),T]).
prim(my_odd21,[int]).
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
p([['h','x','R'],['O','p','G','p'],['L','j','Z'],['Q','o','H']],[['h','x'],['O','p','G'],['L','j'],['Q','o']]).
p([['a','g','j'],['Z','Q','Y','E']],[['a','g'],['Z','Q','Y']]).
p([['W','x','B'],['e','S','r','X'],['Z','b','b','C'],['L','y','t']],[['W','x'],['e','S','r'],['Z','b','b'],['L','y']]).
p([['f','O','B','s'],['Q','k','X'],['i','z','l','c']],[['f','O','B'],['Q','k'],['i','z','l']]).
p([['Q','f','b','q'],['G','k','a','W'],['i','S','B'],['U','I','r','l']],[['Q','f','b'],['G','k','a'],['i','S'],['U','I','r']]).
q([['Z','B','j','f'],['S','D','P']],[['Z','B','j'],['S','D','P']]).
q([['x','s','x','F'],['f','c','H','z'],['J','L','O','x'],['g','s','m']],[['x','s','x','F'],['f','c','H'],['J','L','O','x'],['g','s','m']]).
q([['C','C','a'],['j','s','L','c'],['X','n','c'],['x','j','n','v']],[['C','C','a'],['j','s','L','c'],['X','n','c'],['x','j','n']]).
q([['C','w','x'],['Q','i','A']],[['C','w','x'],['Q','i']]).
q([['b','x','o'],['E','b','B','I'],['Q','F','e','V']],[['b','x','o'],['E','b','B','I'],['Q','F','e']]).
