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

my_odd3(A):-1 is A mod 2.
my_len4(A,B):-length(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_sumlist6(A,B):-sumlist(A,B).
my_max_list7(A,B):-max_list(A,B).
my_lowercase8(A):-downcase_atom(A,A).
my_tolower9(A,B):-downcase_atom(A,B).
my_list_to_set10(A,B):-list_to_set(A,B).
my_head11([H|_],H).
my_double12(N,M):-M is 2*N,M =< 10.
my_flatten13(A,B):-flatten(A,B).
my_element14(A,B):-member(B,A).
my_pred15(A,B):-succ(B,A),A > 0.
my_last16(A,B):-last(A,B).
my_even17(A):-0 is A mod 2.
my_msort18(A,B):-msort(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_odd3/1).
prim(my_len4/2).
prim(my_succ5/2).
prim(my_sumlist6/2).
prim(my_max_list7/2).
prim(my_lowercase8/1).
prim(my_tolower9/2).
prim(my_list_to_set10/2).
prim(my_head11/2).
prim(my_double12/2).
prim(my_flatten13/2).
prim(my_element14/2).
prim(my_pred15/2).
prim(my_last16/2).
prim(my_even17/1).
prim(my_msort18/2).
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
p([['E','t','r'],['V','G','b'],['v','U','O','M'],['N','H','v']],[['E','t'],['V','G'],['v','U','O'],['N','H']]).
p([['J','s','z'],['j','s','A'],['M','Q','F','w'],['n','O','b','N']],[['J','s'],['j','s'],['M','Q','F'],['n','O','b']]).
p([['X','e','C','D'],['l','Y','N','q'],['r','Z','S','B'],['T','T','K']],[['X','e','C'],['l','Y','N'],['r','Z','S'],['T','T']]).
p([['o','l','h'],['b','I','T','Y'],['j','H','B']],[['o','l'],['b','I','T'],['j','H']]).
p([['S','M','d','H'],['P','U','j']],[['S','M','d'],['P','U']]).
q([['r','n','P','F'],['k','t','j'],['E','c','m']],[['r','n','P','F'],['k','t'],['E','c','m']]).
q([['M','X','z','P'],['G','y','y'],['W','Z','X']],[['M','X','z','P'],['G','y'],['W','Z','X']]).
q([['p','F','f'],['N','A','B','Y'],['b','v','X'],['S','u','Q']],[['p','F'],['N','A','B','Y'],['b','v'],['S','u','Q']]).
q([['O','h','Y','n'],['K','Y','w'],['X','e','G'],['R','p','j','u']],[['O','h','Y'],['K','Y','w'],['X','e','G'],['R','p','j','u']]).
q([['l','o','c','I'],['l','V','G'],['V','u','D'],['M','t','O','H']],[['l','o','c'],['l','V','G'],['V','u'],['M','t','O','H']]).
