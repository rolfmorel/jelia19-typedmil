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

my_set3(A):-list_to_set(A,A).
my_double4(N,M):-M is 2*N,M =< 10.
my_element5(A,B):-member(B,A).
my_even6(A):-0 is A mod 2.
my_succ7(A,B):-succ(A,B),B =< 10.
my_max_list8(A,B):-max_list(A,B).
my_flatten9(A,B):-flatten(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_msort11(A,B):-msort(A,B).
my_toupper12(A,B):-upcase_atom(A,B).
my_min_list13(A,B):-min_list(A,B).
my_last14(A,B):-last(A,B).
my_odd15(A):-1 is A mod 2.
my_uppercase16(A):-upcase_atom(A,A).
my_len17(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_set3,[list(_)]).
prim(my_double4,[int,int]).
prim(my_element5,[list(T),T]).
prim(my_even6,[int]).
prim(my_succ7,[int,int]).
prim(my_max_list8,[list(int),int]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_lowercase10,[char]).
prim(my_msort11,[list(int),list(int)]).
prim(my_toupper12,[char,char]).
prim(my_min_list13,[list(int),int]).
prim(my_last14,[list(T),T]).
prim(my_odd15,[int]).
prim(my_uppercase16,[char]).
prim(my_len17,[list(_),int]).
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
p([['z','H','A'],['y','G','d','x']],[['z','H'],['y','G','d']]).
p([['J','Y','b','h'],['K','d','I','F'],['n','C','G','s']],[['J','Y','b'],['K','d','I'],['n','C','G']]).
p([['F','p','E'],['u','H','v','l'],['H','Y','s']],[['F','p'],['u','H','v'],['H','Y']]).
p([['d','n','e','C'],['t','X','h','M'],['u','s','O']],[['d','n','e'],['t','X','h'],['u','s']]).
p([['B','T','S'],['s','B','o','B'],['H','X','F'],['s','s','s']],[['B','T'],['s','B','o'],['H','X'],['s','s']]).
q([['g','u','G'],['e','G','w'],['n','T','n']],[['g','u','G'],['e','G','w'],['n','T']]).
q([['S','N','o'],['G','P','H']],[['S','N'],['G','P','H']]).
q([['u','I','W'],['E','R','d','t'],['n','z','F']],[['u','I'],['E','R','d','t'],['n','z','F']]).
q([['h','C','F','T'],['M','Y','r','S'],['X','h','X'],['f','y','J']],[['h','C','F','T'],['M','Y','r','S'],['X','h'],['f','y','J']]).
q([['p','W','r'],['I','V','H'],['w','K','s','Y'],['K','l','s']],[['p','W'],['I','V','H'],['w','K','s','Y'],['K','l']]).
