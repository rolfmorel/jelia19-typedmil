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

my_element3(A,B):-member(B,A).
my_last4(A,B):-last(A,B).
my_uppercase5(A):-upcase_atom(A,A).
my_odd6(A):-1 is A mod 2.
my_min_list7(A,B):-min_list(A,B).
my_double8(N,M):-M is 2*N,M =< 10.

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

my_tolower10(A,B):-downcase_atom(A,B).
my_head11([H|_],H).
my_even12(A):-0 is A mod 2.
my_list_to_set13(A,B):-list_to_set(A,B).
my_lowercase14(A):-downcase_atom(A,A).
my_set15(A):-list_to_set(A,A).
my_max_list16(A,B):-max_list(A,B).
my_toupper17(A,B):-upcase_atom(A,B).
my_pred18(A,B):-succ(B,A),A > 0.
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_element3,[list(T),T]).
prim(my_last4,[list(T),T]).
prim(my_uppercase5,[char]).
prim(my_odd6,[int]).
prim(my_min_list7,[list(int),int]).
prim(my_double8,[int,int]).
prim(my_tolower10,[char,char]).
prim(my_head11,[list(T),T]).
prim(my_even12,[int]).
prim(my_list_to_set13,[list(T),list(T)]).
prim(my_lowercase14,[char]).
prim(my_set15,[list(_)]).
prim(my_max_list16,[list(int),int]).
prim(my_toupper17,[char,char]).
prim(my_pred18,[int,int]).
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
p([['p','I','b'],['X','j','e','T'],['D','k','s'],['x','n','k','j']],[['p','I'],['X','j','e'],['D','k'],['x','n','k']]).
p([['V','P','p','p'],['K','r','f','m'],['C','V','M','L'],['A','i','p','c']],[['V','P','p'],['K','r','f'],['C','V','M'],['A','i','p']]).
p([['W','X','x','A'],['l','V','K'],['m','n','W','E']],[['W','X','x'],['l','V'],['m','n','W']]).
p([['e','k','L','o'],['f','F','W','F'],['p','s','a','I']],[['e','k','L'],['f','F','W'],['p','s','a']]).
p([['r','Z','y','a'],['S','Z','I','v'],['r','p','p']],[['r','Z','y'],['S','Z','I'],['r','p']]).
q([['H','t','N','r'],['M','N','D'],['l','V','l']],[['H','t','N','r'],['M','N','D'],['l','V']]).
q([['r','T','B','g'],['o','D','E']],[['r','T','B'],['o','D','E']]).
q([['r','s','w'],['o','H','g'],['z','h','M'],['b','R','w','m']],[['r','s','w'],['o','H','g'],['z','h'],['b','R','w']]).
q([['m','f','U','J'],['Q','P','J']],[['m','f','U'],['Q','P','J']]).
q([['M','p','d'],['Q','i','F','b'],['m','i','w'],['a','m','w']],[['M','p','d'],['Q','i','F'],['m','i','w'],['a','m','w']]).
