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

my_set5(A):-list_to_set(A,A).
my_head6([H|_],H).
my_flatten7(A,B):-flatten(A,B).
my_lowercase8(A):-downcase_atom(A,A).
my_min_list9(A,B):-min_list(A,B).
my_double10(N,M):-M is 2*N,M =< 10.
my_succ11(A,B):-succ(A,B),B =< 10.
my_even12(A):-0 is A mod 2.
my_last13(A,B):-last(A,B).
my_element14(A,B):-member(B,A).
my_max_list15(A,B):-max_list(A,B).
my_uppercase16(A):-upcase_atom(A,A).
my_toupper17(A,B):-upcase_atom(A,B).
my_list_to_set18(A,B):-list_to_set(A,B).
my_msort19(A,B):-msort(A,B).
my_odd20(A):-1 is A mod 2.
my_sumlist21(A,B):-sumlist(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_len3,[list(_),int]).
prim(my_set5,[list(_)]).
prim(my_head6,[list(T),T]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_lowercase8,[char]).
prim(my_min_list9,[list(int),int]).
prim(my_double10,[int,int]).
prim(my_succ11,[int,int]).
prim(my_even12,[int]).
prim(my_last13,[list(T),T]).
prim(my_element14,[list(T),T]).
prim(my_max_list15,[list(int),int]).
prim(my_uppercase16,[char]).
prim(my_toupper17,[char,char]).
prim(my_list_to_set18,[list(T),list(T)]).
prim(my_msort19,[list(int),list(int)]).
prim(my_odd20,[int]).
prim(my_sumlist21,[list(int),int]).
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
p([['Q','Z','Q'],['P','e','j'],['E','J','I','b'],['Q','t','G']],[['Q','Z'],['P','e'],['E','J','I'],['Q','t']]).
p([['m','l','a'],['k','m','m','s'],['o','N','I','X'],['y','g','H','R']],[['m','l'],['k','m','m'],['o','N','I'],['y','g','H']]).
p([['c','F','p'],['O','S','z']],[['c','F'],['O','S']]).
p([['V','y','J'],['p','k','v','W'],['P','b','b','L']],[['V','y'],['p','k','v'],['P','b','b']]).
p([['R','C','J'],['d','y','n','A']],[['R','C'],['d','y','n']]).
q([['L','b','v'],['A','U','w','K'],['a','M','N'],['I','x','Z','J']],[['L','b','v'],['A','U','w','K'],['a','M','N'],['I','x','Z']]).
q([['v','l','w'],['z','H','u'],['e','k','D','E'],['F','p','u']],[['v','l','w'],['z','H','u'],['e','k','D'],['F','p','u']]).
q([['D','m','k','H'],['Q','o','p'],['D','Q','D'],['Q','T','N','f']],[['D','m','k','H'],['Q','o'],['D','Q'],['Q','T','N','f']]).
q([['S','b','y','t'],['C','q','f','S'],['c','O','f','r'],['B','t','j']],[['S','b','y'],['C','q','f'],['c','O','f','r'],['B','t','j']]).
q([['B','a','y'],['F','T','r','V'],['J','O','O','E'],['F','K','o']],[['B','a'],['F','T','r'],['J','O','O','E'],['F','K','o']]).
