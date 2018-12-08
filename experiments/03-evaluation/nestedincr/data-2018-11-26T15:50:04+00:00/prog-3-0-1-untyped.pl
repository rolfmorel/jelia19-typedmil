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
p([[5,3,2],[2,3,4],[6,5,3,6],[1,1,2]],[[7,5,4],[4,5,6],[8,7,5,8],[3,3,4]]).
p([[6,1,6],[4,5,6,5]],[[8,3,8],[6,7,8,7]]).
p([[1,5,3,2],[4,5,0]],[[3,7,5,4],[6,7,2]]).
p([[1,0,6,4],[1,2,7],[2,3,7,6]],[[3,2,8,6],[3,4,9],[4,5,9,8]]).
p([[0,0,7],[4,4,1,7],[0,4,4,0]],[[2,2,9],[6,6,3,9],[2,6,6,2]]).
q([[2,6,2,4],[6,1,3],[3,6,6,7]],[[4,8,4,6],[8,3,5],[3,6,6,7]]).
q([[6,4,2],[7,1,3,2]],[[8,6,4],[7,1,3,2]]).
q([[0,3,3],[2,4,1,2],[4,7,7]],[[2,5,5],[4,6,3,4],[4,7,7]]).
q([[1,1,2,1],[1,3,3]],[[3,3,4,3],[1,3,3]]).
q([[6,3,6,5],[4,0,2],[6,2,4,6],[3,3,3,6]],[[8,5,8,7],[4,0,2],[6,2,4,6],[5,5,5,8]]).
