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

my_succ3(A,B):-succ(A,B),B =< 10.
my_even4(A):-0 is A mod 2.
my_max_list5(A,B):-max_list(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_flatten7(A,B):-flatten(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_element9(A,B):-member(B,A).
my_toupper10(A,B):-upcase_atom(A,B).
my_len11(A,B):-length(A,B).
my_set12(A):-list_to_set(A,A).
my_tolower13(A,B):-downcase_atom(A,B).
my_lowercase14(A):-downcase_atom(A,A).
my_odd15(A):-1 is A mod 2.
my_min_list16(A,B):-min_list(A,B).
my_head17([H|_],H).
my_last18(A,B):-last(A,B).
my_list_to_set19(A,B):-list_to_set(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_succ3,[int,int]).
prim(my_even4,[int]).
prim(my_max_list5,[list(int),int]).
prim(my_uppercase6,[char]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_pred8,[int,int]).
prim(my_element9,[list(T),T]).
prim(my_toupper10,[char,char]).
prim(my_len11,[list(_),int]).
prim(my_set12,[list(_)]).
prim(my_tolower13,[char,char]).
prim(my_lowercase14,[char]).
prim(my_odd15,[int]).
prim(my_min_list16,[list(int),int]).
prim(my_head17,[list(T),T]).
prim(my_last18,[list(T),T]).
prim(my_list_to_set19,[list(T),list(T)]).
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
p([['B','r','s'],['r','w','u','T']],[['B','r'],['r','w','u']]).
p([['D','a','I','m'],['g','r','A','O']],[['D','a','I'],['g','r','A']]).
p([['e','i','i','d'],['J','R','L','x']],[['e','i','i'],['J','R','L']]).
p([['H','r','b'],['A','F','D'],['J','S','l','g'],['R','q','i']],[['H','r'],['A','F'],['J','S','l'],['R','q']]).
p([['W','m','s','R'],['d','A','i']],[['W','m','s'],['d','A']]).
q([['I','S','J','a'],['D','c','M','d'],['q','a','e']],[['I','S','J','a'],['D','c','M'],['q','a','e']]).
q([['o','i','P','Z'],['Y','F','l','b'],['A','h','B']],[['o','i','P','Z'],['Y','F','l'],['A','h','B']]).
q([['f','n','J'],['m','J','y'],['H','X','q'],['T','H','O']],[['f','n'],['m','J','y'],['H','X','q'],['T','H','O']]).
q([['g','N','a','n'],['s','c','R','b'],['X','P','H'],['b','I','J','V']],[['g','N','a'],['s','c','R','b'],['X','P','H'],['b','I','J','V']]).
q([['J','c','O','i'],['s','P','e','Z']],[['J','c','O','i'],['s','P','e']]).
