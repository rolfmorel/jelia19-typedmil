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
p([[3,2,7],[1,5,2,4]],[[5,4,9],[3,7,4,6]]).
p([[4,4,1],[6,3,3,0],[7,7,0,4]],[[6,6,3],[8,5,5,2],[9,9,2,6]]).
p([[2,3,2,0],[0,2,4,1],[4,6,4,1],[1,0,7,1]],[[4,5,4,2],[2,4,6,3],[6,8,6,3],[3,2,9,3]]).
p([[5,5,7],[5,6,2],[0,3,4],[6,3,0,5]],[[7,7,9],[7,8,4],[2,5,6],[8,5,2,7]]).
p([[7,4,7,5],[0,6,6,6],[0,1,0,6],[1,6,5,6]],[[9,6,9,7],[2,8,8,8],[2,3,2,8],[3,8,7,8]]).
q([[4,7,4,0],[2,1,0],[6,3,2]],[[6,9,6,2],[4,3,2],[6,3,2]]).
q([[6,6,5],[0,3,2],[2,6,3,0],[5,1,7]],[[8,8,7],[2,5,4],[2,6,3,0],[5,1,7]]).
q([[6,6,2,3],[4,3,0],[7,5,7,5],[7,4,1,5]],[[8,8,4,5],[6,5,2],[9,7,9,7],[7,4,1,5]]).
q([[3,6,0,6],[6,7,6,0],[1,6,6]],[[5,8,2,8],[6,7,6,0],[3,8,8]]).
q([[0,7,5,7],[5,7,3],[7,1,1]],[[2,9,7,9],[5,7,3],[9,3,3]]).
