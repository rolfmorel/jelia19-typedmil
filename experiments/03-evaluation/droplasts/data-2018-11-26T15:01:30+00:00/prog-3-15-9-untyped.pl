:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
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
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_set3/1).
prim(my_double4/2).
prim(my_element5/2).
prim(my_even6/1).
prim(my_succ7/2).
prim(my_max_list8/2).
prim(my_flatten9/2).
prim(my_lowercase10/1).
prim(my_msort11/2).
prim(my_toupper12/2).
prim(my_min_list13/2).
prim(my_last14/2).
prim(my_odd15/1).
prim(my_uppercase16/1).
prim(my_len17/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
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
