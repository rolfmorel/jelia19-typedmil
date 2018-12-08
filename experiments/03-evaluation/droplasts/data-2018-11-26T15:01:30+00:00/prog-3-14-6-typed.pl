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
my_lowercase4(A):-downcase_atom(A,A).
my_head5([H|_],H).
my_toupper6(A,B):-upcase_atom(A,B).
my_element7(A,B):-member(B,A).
my_list_to_set8(A,B):-list_to_set(A,B).
my_even9(A):-0 is A mod 2.
my_sumlist10(A,B):-sumlist(A,B).
my_set11(A):-list_to_set(A,A).

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

my_odd13(A):-1 is A mod 2.
my_min_list14(A,B):-min_list(A,B).
my_uppercase15(A):-upcase_atom(A,A).
my_double16(N,M):-M is 2*N,M =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_len3,[list(_),int]).
prim(my_lowercase4,[char]).
prim(my_head5,[list(T),T]).
prim(my_toupper6,[char,char]).
prim(my_element7,[list(T),T]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_even9,[int]).
prim(my_sumlist10,[list(int),int]).
prim(my_set11,[list(_)]).
prim(my_odd13,[int]).
prim(my_min_list14,[list(int),int]).
prim(my_uppercase15,[char]).
prim(my_double16,[int,int]).
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
p([['q','T','G'],['A','u','s'],['M','A','E','B'],['P','t','I','R']],[['q','T'],['A','u'],['M','A','E'],['P','t','I']]).
p([['n','f','S','z'],['N','E','g','l'],['m','M','z','U']],[['n','f','S'],['N','E','g'],['m','M','z']]).
p([['o','W','E'],['m','q','y'],['k','K','u'],['D','i','O']],[['o','W'],['m','q'],['k','K'],['D','i']]).
p([['i','w','h'],['k','C','E']],[['i','w'],['k','C']]).
p([['X','P','i'],['C','V','r','N']],[['X','P'],['C','V','r']]).
q([['F','c','p','R'],['o','B','V'],['b','D','d']],[['F','c','p'],['o','B','V'],['b','D','d']]).
q([['H','E','f','o'],['B','D','v','N']],[['H','E','f'],['B','D','v','N']]).
q([['d','q','U','p'],['R','d','f','R'],['a','Y','J'],['q','q','s','g']],[['d','q','U','p'],['R','d','f'],['a','Y','J'],['q','q','s']]).
q([['I','P','v','L'],['m','A','b'],['L','L','X','o'],['W','d','r','z']],[['I','P','v'],['m','A','b'],['L','L','X'],['W','d','r','z']]).
q([['Q','F','G','g'],['e','P','i'],['m','s','v','K'],['R','F','u','q']],[['Q','F','G','g'],['e','P','i'],['m','s','v','K'],['R','F','u']]).
