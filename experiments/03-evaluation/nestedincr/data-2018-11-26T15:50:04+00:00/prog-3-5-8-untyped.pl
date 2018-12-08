:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_succ1(A,B):-succ(A,B),B =< 10.
my_odd2(A):-1 is A mod 2.
my_double3(N,M):-M is 2*N,M =< 10.
my_last4(A,B):-last(A,B).
my_msort5(A,B):-msort(A,B).
my_max_list6(A,B):-max_list(A,B).
prim(my_succ1/2).
prim(my_odd2/1).
prim(my_double3/2).
prim(my_last4/2).
prim(my_msort5/2).
prim(my_max_list6/2).
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
p([[1,6,0,7],[6,6,3,6],[6,1,4]],[[3,8,2,9],[8,8,5,8],[8,3,6]]).
p([[5,6,0],[2,1,7,7],[0,7,2]],[[7,8,2],[4,3,9,9],[2,9,4]]).
p([[1,6,6,7],[4,4,5,7]],[[3,8,8,9],[6,6,7,9]]).
p([[5,3,0,5],[4,6,4],[1,5,0]],[[7,5,2,7],[6,8,6],[3,7,2]]).
p([[0,0,6,7],[5,6,3],[4,0,0],[7,4,2,4]],[[2,2,8,9],[7,8,5],[6,2,2],[9,6,4,6]]).
q([[4,4,2],[0,7,7],[3,1,4],[1,0,2]],[[6,6,4],[2,9,9],[5,3,6],[1,0,2]]).
q([[2,5,6],[7,4,2,7]],[[2,5,6],[9,6,4,9]]).
q([[5,7,3],[4,6,6,3]],[[7,9,5],[4,6,6,3]]).
q([[7,4,5,7],[1,3,5],[0,7,0,2],[6,6,0,6]],[[9,6,7,9],[1,3,5],[2,9,2,4],[8,8,2,8]]).
q([[1,2,3],[6,6,1,7],[3,5,4]],[[3,4,5],[6,6,1,7],[5,7,6]]).
