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

my_flatten3(A,B):-flatten(A,B).
my_msort4(A,B):-msort(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_even6(A):-0 is A mod 2.
my_uppercase7(A):-upcase_atom(A,A).
my_head8([H|_],H).
my_odd9(A):-1 is A mod 2.
my_lowercase10(A):-downcase_atom(A,A).
my_len11(A,B):-length(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_pred13(A,B):-succ(B,A),A > 0.
my_toupper14(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_flatten3/2).
prim(my_msort4/2).
prim(my_double5/2).
prim(my_even6/1).
prim(my_uppercase7/1).
prim(my_head8/2).
prim(my_odd9/1).
prim(my_lowercase10/1).
prim(my_len11/2).
prim(my_succ12/2).
prim(my_pred13/2).
prim(my_toupper14/2).
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
p([['i','N','X','N'],['h','Q','i'],['V','a','k'],['M','l','H']],[['i','N','X'],['h','Q'],['V','a'],['M','l']]).
p([['Q','M','L'],['Z','W','o','M'],['a','V','u'],['Z','U','l','l']],[['Q','M'],['Z','W','o'],['a','V'],['Z','U','l']]).
p([['b','R','a'],['D','Q','P'],['b','O','g','L']],[['b','R'],['D','Q'],['b','O','g']]).
p([['e','A','p'],['N','L','M','u'],['r','U','t'],['O','I','B']],[['e','A'],['N','L','M'],['r','U'],['O','I']]).
p([['S','b','P'],['t','q','E']],[['S','b'],['t','q']]).
q([['P','i','m'],['a','e','a'],['k','W','t','l'],['r','Q','I','n']],[['P','i'],['a','e','a'],['k','W','t','l'],['r','Q','I','n']]).
q([['T','x','t','h'],['P','c','R'],['Z','M','u']],[['T','x','t','h'],['P','c'],['Z','M','u']]).
q([['r','u','q','z'],['I','v','A','u'],['j','K','w','q'],['N','w','j','q']],[['r','u','q','z'],['I','v','A'],['j','K','w'],['N','w','j','q']]).
q([['g','M','m'],['l','T','v'],['o','X','d','C']],[['g','M','m'],['l','T'],['o','X','d','C']]).
q([['k','R','C','f'],['N','y','B','v'],['W','M','W','m']],[['k','R','C','f'],['N','y','B'],['W','M','W','m']]).
