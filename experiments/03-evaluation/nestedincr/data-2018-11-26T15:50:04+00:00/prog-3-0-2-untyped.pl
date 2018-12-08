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
p([[7,7,6,7],[1,2,6],[2,6,4]],[[9,9,8,9],[3,4,8],[4,8,6]]).
p([[4,5,0],[1,7,6]],[[6,7,2],[3,9,8]]).
p([[3,5,2,5],[1,5,0,3]],[[5,7,4,7],[3,7,2,5]]).
p([[0,0,0,3],[5,5,3],[2,3,1]],[[2,2,2,5],[7,7,5],[4,5,3]]).
p([[6,4,3],[2,6,4]],[[8,6,5],[4,8,6]]).
q([[3,3,4],[7,2,7],[7,2,4],[0,7,7]],[[5,5,6],[9,4,9],[7,2,4],[2,9,9]]).
q([[5,3,5],[3,5,3,6]],[[5,3,5],[5,7,5,8]]).
q([[0,7,5,2],[3,0,6,4],[0,3,1]],[[2,9,7,4],[3,0,6,4],[2,5,3]]).
q([[3,7,6],[1,2,0],[4,2,6],[7,4,3,7]],[[3,7,6],[3,4,2],[6,4,8],[9,6,5,9]]).
q([[0,0,1,1],[5,5,7]],[[2,2,3,3],[5,5,7]]).
