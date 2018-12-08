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
my_pred2(A,B):-succ(B,A),A > 0.
my_head3([H|_],H).
prim(my_succ1/2).
prim(my_pred2/2).
prim(my_head3/2).
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
p([[7,0,1],[2,7,6,0]],[[9,2,3],[4,9,8,2]]).
p([[0,5,4],[5,0,2,7],[4,6,3],[6,0,6,2]],[[2,7,6],[7,2,4,9],[6,8,5],[8,2,8,4]]).
p([[6,1,6,4],[0,1,3,7],[7,1,6],[2,0,1,7]],[[8,3,8,6],[2,3,5,9],[9,3,8],[4,2,3,9]]).
p([[3,0,4],[1,6,4],[4,0,6,7]],[[5,2,6],[3,8,6],[6,2,8,9]]).
p([[3,5,1],[1,3,4,6]],[[5,7,3],[3,5,6,8]]).
q([[2,6,7,0],[7,7,1]],[[2,6,7,0],[9,9,3]]).
q([[1,1,7,3],[2,1,0,2],[6,7,6],[0,0,5,2]],[[3,3,9,5],[4,3,2,4],[8,9,8],[0,0,5,2]]).
q([[0,4,3,3],[6,5,2],[0,0,2]],[[2,6,5,5],[6,5,2],[2,2,4]]).
q([[4,2,3,4],[1,2,7,6],[6,6,6]],[[4,2,3,4],[3,4,9,8],[8,8,8]]).
q([[4,0,7],[6,5,7,0]],[[4,0,7],[8,7,9,2]]).
