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

my_even3(A):-0 is A mod 2.
my_element4(A,B):-member(B,A).
my_toupper5(A,B):-upcase_atom(A,B).
my_odd6(A):-1 is A mod 2.
my_flatten7(A,B):-flatten(A,B).
my_uppercase8(A):-upcase_atom(A,A).
my_set9(A):-list_to_set(A,A).
my_min_list10(A,B):-min_list(A,B).
my_head11([H|_],H).
my_max_list12(A,B):-max_list(A,B).
my_tolower13(A,B):-downcase_atom(A,B).

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

my_list_to_set15(A,B):-list_to_set(A,B).
my_last16(A,B):-last(A,B).
my_double17(N,M):-M is 2*N,M =< 10.
my_msort18(A,B):-msort(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_even3,[int]).
prim(my_element4,[list(T),T]).
prim(my_toupper5,[char,char]).
prim(my_odd6,[int]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_uppercase8,[char]).
prim(my_set9,[list(_)]).
prim(my_min_list10,[list(int),int]).
prim(my_head11,[list(T),T]).
prim(my_max_list12,[list(int),int]).
prim(my_tolower13,[char,char]).
prim(my_list_to_set15,[list(T),list(T)]).
prim(my_last16,[list(T),T]).
prim(my_double17,[int,int]).
prim(my_msort18,[list(int),list(int)]).
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
p([['p','h','K','e'],['F','w','y'],['L','W','d']],[['p','h','K'],['F','w'],['L','W']]).
p([['L','C','C','d'],['K','h','v','H'],['L','p','f']],[['L','C','C'],['K','h','v'],['L','p']]).
p([['K','f','L'],['Q','v','R','C']],[['K','f'],['Q','v','R']]).
p([['Q','t','R'],['S','D','B','W'],['t','M','k','k']],[['Q','t'],['S','D','B'],['t','M','k']]).
p([['P','O','e','N'],['W','V','R','d'],['r','I','n']],[['P','O','e'],['W','V','R'],['r','I']]).
q([['B','r','m','g'],['q','o','s','W'],['l','k','D'],['U','e','S']],[['B','r','m','g'],['q','o','s','W'],['l','k'],['U','e','S']]).
q([['Z','h','i'],['p','A','r','n']],[['Z','h','i'],['p','A','r']]).
q([['r','Y','h','Z'],['W','U','E'],['s','T','Y']],[['r','Y','h'],['W','U','E'],['s','T','Y']]).
q([['l','T','S'],['g','a','R','D'],['g','l','Y']],[['l','T','S'],['g','a','R','D'],['g','l']]).
q([['l','h','w'],['a','i','J','b'],['r','W','z','k']],[['l','h','w'],['a','i','J'],['r','W','z','k']]).
