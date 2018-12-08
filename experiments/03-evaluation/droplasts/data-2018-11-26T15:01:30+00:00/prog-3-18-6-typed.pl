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
my_tolower4(A,B):-downcase_atom(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_odd6(A):-1 is A mod 2.
my_last7(A,B):-last(A,B).
my_set8(A):-list_to_set(A,A).
my_element9(A,B):-member(B,A).
my_succ10(A,B):-succ(A,B),B =< 10.
my_pred11(A,B):-succ(B,A),A > 0.
my_toupper12(A,B):-upcase_atom(A,B).

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

my_even14(A):-0 is A mod 2.
my_uppercase15(A):-upcase_atom(A,A).
my_double16(N,M):-M is 2*N,M =< 10.
my_list_to_set17(A,B):-list_to_set(A,B).
my_len18(A,B):-length(A,B).
my_head19([H|_],H).
my_min_list20(A,B):-min_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_sumlist3,[list(int),int]).
prim(my_tolower4,[char,char]).
prim(my_lowercase5,[char]).
prim(my_odd6,[int]).
prim(my_last7,[list(T),T]).
prim(my_set8,[list(_)]).
prim(my_element9,[list(T),T]).
prim(my_succ10,[int,int]).
prim(my_pred11,[int,int]).
prim(my_toupper12,[char,char]).
prim(my_even14,[int]).
prim(my_uppercase15,[char]).
prim(my_double16,[int,int]).
prim(my_list_to_set17,[list(T),list(T)]).
prim(my_len18,[list(_),int]).
prim(my_head19,[list(T),T]).
prim(my_min_list20,[list(int),int]).
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
p([['P','e','t','w'],['p','T','Q'],['z','W','U','Z'],['P','C','h']],[['P','e','t'],['p','T'],['z','W','U'],['P','C']]).
p([['L','T','l'],['y','T','x'],['R','n','M','L']],[['L','T'],['y','T'],['R','n','M']]).
p([['u','E','D'],['j','n','a','L'],['A','B','m'],['h','U','q']],[['u','E'],['j','n','a'],['A','B'],['h','U']]).
p([['k','A','U'],['m','V','D'],['g','L','p'],['b','U','f','k']],[['k','A'],['m','V'],['g','L'],['b','U','f']]).
p([['I','m','e','N'],['I','s','P'],['n','Y','t','o'],['S','s','o']],[['I','m','e'],['I','s'],['n','Y','t'],['S','s']]).
q([['I','h','o','o'],['t','G','m'],['N','W','U'],['e','O','z']],[['I','h','o'],['t','G','m'],['N','W'],['e','O','z']]).
q([['Z','I','q'],['p','C','W','b']],[['Z','I'],['p','C','W','b']]).
q([['H','i','Z'],['U','L','S'],['M','D','Y'],['K','n','E','u']],[['H','i'],['U','L','S'],['M','D','Y'],['K','n','E','u']]).
q([['V','o','g','C'],['h','x','G','b'],['G','W','A']],[['V','o','g','C'],['h','x','G'],['G','W','A']]).
q([['t','C','T','d'],['K','Q','I'],['q','R','h']],[['t','C','T','d'],['K','Q'],['q','R','h']]).
