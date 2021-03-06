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
p([[1,3,2,2],[0,6,6,1],[6,1,4,4],[1,7,5,6]],[[3,5,4,4],[2,8,8,3],[8,3,6,6],[3,9,7,8]]).
p([[5,1,1,3],[5,5,3,5]],[[7,3,3,5],[7,7,5,7]]).
p([[6,2,7],[1,5,7]],[[8,4,9],[3,7,9]]).
p([[4,6,4,6],[3,4,6]],[[6,8,6,8],[5,6,8]]).
p([[0,5,7],[0,7,7,1]],[[2,7,9],[2,9,9,3]]).
q([[6,0,5,3],[6,5,1,6]],[[8,2,7,5],[6,5,1,6]]).
q([[7,2,0,1],[0,6,7,5]],[[7,2,0,1],[2,8,9,7]]).
q([[2,7,5],[1,4,4,7],[5,3,5,4]],[[4,9,7],[1,4,4,7],[7,5,7,6]]).
q([[2,6,6,4],[5,4,7],[3,6,2,1]],[[4,8,8,6],[7,6,9],[3,6,2,1]]).
q([[2,5,3],[5,5,1],[4,6,0]],[[2,5,3],[7,7,3],[6,8,2]]).
