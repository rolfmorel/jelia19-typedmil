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

my_double3(N,M):-M is 2*N,M =< 10.
my_element4(A,B):-member(B,A).
my_sumlist5(A,B):-sumlist(A,B).
my_toupper6(A,B):-upcase_atom(A,B).
my_msort7(A,B):-msort(A,B).
my_max_list8(A,B):-max_list(A,B).
my_tolower9(A,B):-downcase_atom(A,B).
my_list_to_set10(A,B):-list_to_set(A,B).
my_last11(A,B):-last(A,B).
my_set12(A):-list_to_set(A,A).

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

my_min_list14(A,B):-min_list(A,B).
my_succ15(A,B):-succ(A,B),B =< 10.
my_flatten16(A,B):-flatten(A,B).
my_lowercase17(A):-downcase_atom(A,A).
my_pred18(A,B):-succ(B,A),A > 0.
my_len19(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_double3,[int,int]).
prim(my_element4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_toupper6,[char,char]).
prim(my_msort7,[list(int),list(int)]).
prim(my_max_list8,[list(int),int]).
prim(my_tolower9,[char,char]).
prim(my_list_to_set10,[list(T),list(T)]).
prim(my_last11,[list(T),T]).
prim(my_set12,[list(_)]).
prim(my_min_list14,[list(int),int]).
prim(my_succ15,[int,int]).
prim(my_flatten16,[list(list(T)),list(T)]).
prim(my_lowercase17,[char]).
prim(my_pred18,[int,int]).
prim(my_len19,[list(_),int]).
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
p([['v','Z','y'],['x','f','b','D'],['R','K','F']],[['v','Z'],['x','f','b'],['R','K']]).
p([['l','T','t'],['L','x','R']],[['l','T'],['L','x']]).
p([['b','M','j'],['J','S','G','v'],['d','D','O']],[['b','M'],['J','S','G'],['d','D']]).
p([['P','G','x'],['P','K','e'],['H','W','t']],[['P','G'],['P','K'],['H','W']]).
p([['R','O','f'],['a','I','B','P'],['c','A','w'],['q','z','P']],[['R','O'],['a','I','B'],['c','A'],['q','z']]).
q([['a','p','w'],['Y','p','C','H']],[['a','p'],['Y','p','C','H']]).
q([['i','B','P','v'],['v','C','A']],[['i','B','P'],['v','C','A']]).
q([['E','f','w','a'],['r','b','e','f'],['m','T','s']],[['E','f','w','a'],['r','b','e'],['m','T','s']]).
q([['D','n','L','w'],['W','c','B','w'],['D','a','k','u']],[['D','n','L','w'],['W','c','B','w'],['D','a','k']]).
q([['N','k','p'],['K','w','w'],['e','y','e']],[['N','k','p'],['K','w','w'],['e','y']]).
