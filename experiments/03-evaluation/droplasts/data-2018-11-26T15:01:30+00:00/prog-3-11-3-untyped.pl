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

my_len3(A,B):-length(A,B).
my_double4(N,M):-M is 2*N,M =< 10.
my_max_list5(A,B):-max_list(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_sumlist7(A,B):-sumlist(A,B).
my_head8([H|_],H).
my_tolower9(A,B):-downcase_atom(A,B).
my_odd10(A):-1 is A mod 2.
my_last11(A,B):-last(A,B).
my_element12(A,B):-member(B,A).
my_pred13(A,B):-succ(B,A),A > 0.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_len3/2).
prim(my_double4/2).
prim(my_max_list5/2).
prim(my_succ6/2).
prim(my_sumlist7/2).
prim(my_head8/2).
prim(my_tolower9/2).
prim(my_odd10/1).
prim(my_last11/2).
prim(my_element12/2).
prim(my_pred13/2).
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
p([['Y','w','f','Y'],['N','a','B','v']],[['Y','w','f'],['N','a','B']]).
p([['L','M','U'],['y','R','w','q'],['a','a','V']],[['L','M'],['y','R','w'],['a','a']]).
p([['K','v','t','c'],['s','Z','V','z'],['i','J','h','V'],['W','x','v']],[['K','v','t'],['s','Z','V'],['i','J','h'],['W','x']]).
p([['P','i','L'],['z','P','q'],['x','U','H']],[['P','i'],['z','P'],['x','U']]).
p([['i','y','V'],['b','M','j']],[['i','y'],['b','M']]).
q([['P','P','U','q'],['W','I','o'],['u','G','h']],[['P','P','U','q'],['W','I'],['u','G','h']]).
q([['Q','d','H','w'],['P','o','O'],['t','T','K','r']],[['Q','d','H','w'],['P','o'],['t','T','K','r']]).
q([['A','M','q'],['x','q','Z']],[['A','M','q'],['x','q']]).
q([['Z','B','x','Y'],['U','G','n','Y'],['S','Y','F'],['T','o','q']],[['Z','B','x','Y'],['U','G','n','Y'],['S','Y'],['T','o','q']]).
q([['u','X','B','k'],['r','u','m','f']],[['u','X','B'],['r','u','m','f']]).
