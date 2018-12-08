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

my_sumlist3(A,B):-sumlist(A,B).
my_len4(A,B):-length(A,B).
my_msort5(A,B):-msort(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_flatten7(A,B):-flatten(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_set9(A):-list_to_set(A,A).
my_uppercase10(A):-upcase_atom(A,A).
my_element11(A,B):-member(B,A).
my_last12(A,B):-last(A,B).
my_even13(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_sumlist3/2).
prim(my_len4/2).
prim(my_msort5/2).
prim(my_succ6/2).
prim(my_flatten7/2).
prim(my_double8/2).
prim(my_set9/1).
prim(my_uppercase10/1).
prim(my_element11/2).
prim(my_last12/2).
prim(my_even13/1).
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
p([['k','m','L'],['Q','V','P'],['i','F','D']],[['k','m'],['Q','V'],['i','F']]).
p([['J','O','s'],['S','n','u','b']],[['J','O'],['S','n','u']]).
p([['h','R','w'],['a','Y','t','w'],['Q','b','Q','M']],[['h','R'],['a','Y','t'],['Q','b','Q']]).
p([['N','b','o','q'],['u','n','u','W'],['V','s','s'],['M','x','y']],[['N','b','o'],['u','n','u'],['V','s'],['M','x']]).
p([['x','E','I','V'],['d','J','y','k'],['g','g','O','q']],[['x','E','I'],['d','J','y'],['g','g','O']]).
q([['L','C','v','u'],['h','O','H'],['a','c','O','P'],['L','J','X','q']],[['L','C','v'],['h','O','H'],['a','c','O','P'],['L','J','X','q']]).
q([['Z','t','Q','z'],['d','r','A','L'],['K','A','S']],[['Z','t','Q','z'],['d','r','A','L'],['K','A']]).
q([['K','g','z'],['Y','n','l'],['J','l','s','Q']],[['K','g','z'],['Y','n','l'],['J','l','s']]).
q([['z','r','z'],['u','L','I']],[['z','r'],['u','L','I']]).
q([['T','P','L','t'],['v','b','G'],['F','Q','n']],[['T','P','L'],['v','b','G'],['F','Q','n']]).
