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
p([[0,1,5],[5,3,2,5],[1,7,4,1],[2,0,7,7]],[[2,3,7],[7,5,4,7],[3,9,6,3],[4,2,9,9]]).
p([[1,0,0,2],[4,0,3],[0,5,1]],[[3,2,2,4],[6,2,5],[2,7,3]]).
p([[1,0,1],[4,0,2]],[[3,2,3],[6,2,4]]).
p([[4,2,4],[0,7,1]],[[6,4,6],[2,9,3]]).
p([[1,5,1,3],[1,3,7]],[[3,7,3,5],[3,5,9]]).
q([[5,4,5],[6,7,5],[4,4,0,1],[0,7,2]],[[7,6,7],[8,9,7],[6,6,2,3],[0,7,2]]).
q([[5,6,1],[0,3,1,5],[3,1,1]],[[7,8,3],[2,5,3,7],[3,1,1]]).
q([[4,0,1],[1,6,1],[3,7,3,4],[3,3,3,5]],[[6,2,3],[3,8,3],[3,7,3,4],[5,5,5,7]]).
q([[5,0,0,2],[7,6,3],[6,0,1]],[[7,2,2,4],[7,6,3],[8,2,3]]).
q([[4,4,1,6],[6,7,2],[1,0,0],[3,3,5,5]],[[4,4,1,6],[8,9,4],[3,2,2],[5,5,7,7]]).
