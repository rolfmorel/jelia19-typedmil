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

my_list_to_set3(A,B):-list_to_set(A,B).
my_last4(A,B):-last(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_msort6(A,B):-msort(A,B).
my_set7(A):-list_to_set(A,A).
my_element8(A,B):-member(B,A).
my_uppercase9(A):-upcase_atom(A,A).
my_toupper10(A,B):-upcase_atom(A,B).
my_double11(N,M):-M is 2*N,M =< 10.
my_pred12(A,B):-succ(B,A),A > 0.
my_succ13(A,B):-succ(A,B),B =< 10.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_list_to_set3/2).
prim(my_last4/2).
prim(my_lowercase5/1).
prim(my_msort6/2).
prim(my_set7/1).
prim(my_element8/2).
prim(my_uppercase9/1).
prim(my_toupper10/2).
prim(my_double11/2).
prim(my_pred12/2).
prim(my_succ13/2).
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
p([['H','r','B'],['h','h','u','E'],['a','p','G','K'],['I','p','d']],[['H','r'],['h','h','u'],['a','p','G'],['I','p']]).
p([['b','B','A','V'],['o','j','o'],['o','M','v'],['M','n','A']],[['b','B','A'],['o','j'],['o','M'],['M','n']]).
p([['G','n','H'],['N','r','n'],['h','Z','P']],[['G','n'],['N','r'],['h','Z']]).
p([['R','U','V'],['d','X','m','m'],['Q','h','W'],['S','K','I','v']],[['R','U'],['d','X','m'],['Q','h'],['S','K','I']]).
p([['c','k','j','X'],['u','R','C']],[['c','k','j'],['u','R']]).
q([['H','Z','I','Z'],['r','e','L','T'],['T','X','V']],[['H','Z','I'],['r','e','L','T'],['T','X','V']]).
q([['h','p','S'],['i','v','m']],[['h','p'],['i','v','m']]).
q([['u','U','s'],['O','h','c','s'],['P','F','G'],['M','m','m','z']],[['u','U'],['O','h','c'],['P','F','G'],['M','m','m','z']]).
q([['q','z','E','V'],['x','q','H'],['I','v','q','J'],['q','e','Y','r']],[['q','z','E','V'],['x','q','H'],['I','v','q'],['q','e','Y','r']]).
q([['W','C','g'],['r','Q','r','O']],[['W','C'],['r','Q','r','O']]).
