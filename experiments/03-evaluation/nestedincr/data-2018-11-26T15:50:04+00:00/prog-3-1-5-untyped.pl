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
my_len2(A,B):-length(A,B).
prim(my_succ1/2).
prim(my_len2/2).
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
p([[0,5,3,5],[6,4,5,1],[6,0,2]],[[2,7,5,7],[8,6,7,3],[8,2,4]]).
p([[5,7,0,3],[7,4,4]],[[7,9,2,5],[9,6,6]]).
p([[5,5,1,1],[0,2,5,6]],[[7,7,3,3],[2,4,7,8]]).
p([[6,4,2,2],[7,4,6],[6,3,1],[6,5,2]],[[8,6,4,4],[9,6,8],[8,5,3],[8,7,4]]).
p([[4,0,3],[0,5,7,4],[3,0,7,6]],[[6,2,5],[2,7,9,6],[5,2,9,8]]).
q([[1,4,1,7],[1,6,2,3]],[[3,6,3,9],[1,6,2,3]]).
q([[7,3,0,1],[1,1,5,4],[5,1,5],[0,0,5,1]],[[9,5,2,3],[3,3,7,6],[7,3,7],[0,0,5,1]]).
q([[7,3,5],[0,6,0],[4,4,7]],[[9,5,7],[0,6,0],[6,6,9]]).
q([[2,4,7,2],[5,2,7,4],[5,0,4,2],[6,4,4,2]],[[2,4,7,2],[5,2,7,4],[7,2,6,4],[8,6,6,4]]).
q([[5,0,4,3],[3,7,6,6],[0,1,1],[4,7,0]],[[7,2,6,5],[5,9,8,8],[0,1,1],[6,9,2]]).
