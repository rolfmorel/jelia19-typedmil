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

my_sumlist3(A,B):-sumlist(A,B).

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

my_odd5(A):-1 is A mod 2.
my_list_to_set6(A,B):-list_to_set(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_toupper8(A,B):-upcase_atom(A,B).
my_lowercase9(A):-downcase_atom(A,A).
my_last10(A,B):-last(A,B).
my_uppercase11(A):-upcase_atom(A,A).
my_double12(N,M):-M is 2*N,M =< 10.
my_msort13(A,B):-msort(A,B).
my_tolower14(A,B):-downcase_atom(A,B).
my_even15(A):-0 is A mod 2.
my_flatten16(A,B):-flatten(A,B).
my_pred17(A,B):-succ(B,A),A > 0.
my_head18([H|_],H).
my_max_list19(A,B):-max_list(A,B).
my_min_list20(A,B):-min_list(A,B).
my_element21(A,B):-member(B,A).
my_set22(A):-list_to_set(A,A).
my_len23(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_sumlist3,[list(int),int]).
prim(my_odd5,[int]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_succ7,[int,int]).
prim(my_toupper8,[char,char]).
prim(my_lowercase9,[char]).
prim(my_last10,[list(T),T]).
prim(my_uppercase11,[char]).
prim(my_double12,[int,int]).
prim(my_msort13,[list(int),list(int)]).
prim(my_tolower14,[char,char]).
prim(my_even15,[int]).
prim(my_flatten16,[list(list(T)),list(T)]).
prim(my_pred17,[int,int]).
prim(my_head18,[list(T),T]).
prim(my_max_list19,[list(int),int]).
prim(my_min_list20,[list(int),int]).
prim(my_element21,[list(T),T]).
prim(my_set22,[list(_)]).
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
p([['b','Z','d'],['c','T','n'],['Y','c','G','P']],[['b','Z'],['c','T'],['Y','c','G']]).
p([['O','P','j'],['d','m','w','l'],['e','O','N','b']],[['O','P'],['d','m','w'],['e','O','N']]).
p([['g','o','V','T'],['Y','W','E','q'],['Q','l','I']],[['g','o','V'],['Y','W','E'],['Q','l']]).
p([['z','G','t'],['g','T','b'],['z','b','x']],[['z','G'],['g','T'],['z','b']]).
p([['B','w','h'],['l','i','B'],['J','W','r','t'],['d','t','r']],[['B','w'],['l','i'],['J','W','r'],['d','t']]).
q([['b','q','Z'],['J','N','E','q']],[['b','q','Z'],['J','N','E']]).
q([['q','W','Z'],['i','v','x'],['d','d','l']],[['q','W'],['i','v','x'],['d','d','l']]).
q([['a','U','B'],['Z','O','d','p'],['t','h','c']],[['a','U','B'],['Z','O','d'],['t','h','c']]).
q([['L','m','a','L'],['d','a','R','j'],['Y','L','w']],[['L','m','a'],['d','a','R','j'],['Y','L','w']]).
q([['U','y','S','X'],['O','q','k'],['N','i','p','v'],['d','j','F','x']],[['U','y','S','X'],['O','q','k'],['N','i','p'],['d','j','F','x']]).
