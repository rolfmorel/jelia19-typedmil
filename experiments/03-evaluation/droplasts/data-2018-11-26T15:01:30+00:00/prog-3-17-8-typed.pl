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
my_min_list5(A,B):-min_list(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_flatten8(A,B):-flatten(A,B).
my_msort9(A,B):-msort(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_head11([H|_],H).
my_element12(A,B):-member(B,A).
my_odd13(A):-1 is A mod 2.
my_sumlist14(A,B):-sumlist(A,B).
my_pred15(A,B):-succ(B,A),A > 0.
my_uppercase16(A):-upcase_atom(A,A).

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

my_even18(A):-0 is A mod 2.
my_double19(N,M):-M is 2*N,M =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_len3,[list(_),int]).
prim(my_tolower4,[char,char]).
prim(my_min_list5,[list(int),int]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_toupper7,[char,char]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_msort9,[list(int),list(int)]).
prim(my_lowercase10,[char]).
prim(my_head11,[list(T),T]).
prim(my_element12,[list(T),T]).
prim(my_odd13,[int]).
prim(my_sumlist14,[list(int),int]).
prim(my_pred15,[int,int]).
prim(my_uppercase16,[char]).
prim(my_even18,[int]).
prim(my_double19,[int,int]).
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
p([['b','x','e','E'],['e','h','x','X']],[['b','x','e'],['e','h','x']]).
p([['a','h','i'],['G','V','p'],['k','B','C']],[['a','h'],['G','V'],['k','B']]).
p([['n','l','h'],['v','E','D','Z']],[['n','l'],['v','E','D']]).
p([['V','r','X'],['L','Z','B','e'],['X','m','Q']],[['V','r'],['L','Z','B'],['X','m']]).
p([['J','c','q','n'],['K','R','e']],[['J','c','q'],['K','R']]).
q([['w','X','Y','b'],['u','s','a'],['s','a','r','N']],[['w','X','Y','b'],['u','s','a'],['s','a','r']]).
q([['Y','m','L','m'],['D','y','u'],['d','M','Q']],[['Y','m','L','m'],['D','y','u'],['d','M']]).
q([['h','u','T'],['G','x','U','t'],['q','b','a']],[['h','u'],['G','x','U','t'],['q','b','a']]).
q([['O','L','e','J'],['U','W','J','m'],['W','K','d','w'],['f','w','t']],[['O','L','e'],['U','W','J','m'],['W','K','d','w'],['f','w']]).
q([['h','l','V'],['N','z','H','v'],['s','Q','y','J']],[['h','l','V'],['N','z','H'],['s','Q','y','J']]).
