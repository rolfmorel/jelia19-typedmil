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
prim(my_succ1/2).
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
p([[6,5,0],[3,4,5,0]],[[8,7,2],[5,6,7,2]]).
p([[2,6,6,3],[0,0,5,6]],[[4,8,8,5],[2,2,7,8]]).
p([[7,6,2],[0,0,4],[4,6,3]],[[9,8,4],[2,2,6],[6,8,5]]).
p([[3,0,6,1],[4,2,1,1],[3,5,0]],[[5,2,8,3],[6,4,3,3],[5,7,2]]).
p([[1,0,5],[7,3,5,1]],[[3,2,7],[9,5,7,3]]).
q([[0,7,5,0],[4,2,6,4],[5,7,0,0]],[[2,9,7,2],[6,4,8,6],[5,7,0,0]]).
q([[2,7,6,0],[3,5,1],[6,6,3,1]],[[4,9,8,2],[5,7,3],[6,6,3,1]]).
q([[1,5,1,6],[1,4,5],[1,2,6]],[[1,5,1,6],[3,6,7],[3,4,8]]).
q([[7,1,6],[4,0,6,3]],[[7,1,6],[6,2,8,5]]).
q([[4,3,1],[4,2,2],[2,3,1],[7,4,5,0]],[[4,3,1],[6,4,4],[4,5,3],[9,6,7,2]]).
