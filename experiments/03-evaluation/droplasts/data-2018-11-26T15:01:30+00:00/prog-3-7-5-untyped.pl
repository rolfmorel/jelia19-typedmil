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
my_last4(A,B):-last(A,B).
my_even5(A):-0 is A mod 2.
my_msort6(A,B):-msort(A,B).
my_head7([H|_],H).
my_list_to_set8(A,B):-list_to_set(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_set3/1).
prim(my_last4/2).
prim(my_even5/1).
prim(my_msort6/2).
prim(my_head7/2).
prim(my_list_to_set8/2).
prim(my_succ9/2).
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
p([['w','G','N','L'],['w','d','a'],['D','z','M','I']],[['w','G','N'],['w','d'],['D','z','M']]).
p([['N','x','a','K'],['p','U','f','V'],['t','Y','o','Y']],[['N','x','a'],['p','U','f'],['t','Y','o']]).
p([['o','J','n','W'],['m','u','k','E']],[['o','J','n'],['m','u','k']]).
p([['c','g','c'],['Y','M','s']],[['c','g'],['Y','M']]).
p([['R','K','r'],['y','T','b','b'],['q','q','m','K'],['i','I','u','b']],[['R','K'],['y','T','b'],['q','q','m'],['i','I','u']]).
q([['E','E','z'],['a','c','d']],[['E','E'],['a','c','d']]).
q([['K','J','m'],['j','w','h','e'],['I','q','P','X']],[['K','J','m'],['j','w','h'],['I','q','P','X']]).
q([['F','v','q','E'],['v','K','k']],[['F','v','q','E'],['v','K']]).
q([['d','G','y'],['x','V','Y']],[['d','G'],['x','V','Y']]).
q([['E','g','n','s'],['s','H','A']],[['E','g','n','s'],['s','H']]).
