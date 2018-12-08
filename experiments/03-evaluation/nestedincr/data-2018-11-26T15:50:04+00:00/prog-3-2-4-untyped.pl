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
my_sumlist3(A,B):-sumlist(A,B).
prim(my_succ1/2).
prim(my_pred2/2).
prim(my_sumlist3/2).
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
p([[0,6,1],[4,1,5,5],[0,0,7],[0,3,7,5]],[[2,8,3],[6,3,7,7],[2,2,9],[2,5,9,7]]).
p([[1,2,1,1],[1,0,6],[0,1,3],[3,1,2,4]],[[3,4,3,3],[3,2,8],[2,3,5],[5,3,4,6]]).
p([[7,0,7,2],[2,2,5,2],[4,7,1],[7,6,3,1]],[[9,2,9,4],[4,4,7,4],[6,9,3],[9,8,5,3]]).
p([[2,5,7,3],[7,0,1,6],[4,0,2],[6,1,2]],[[4,7,9,5],[9,2,3,8],[6,2,4],[8,3,4]]).
p([[1,5,0,7],[0,0,5,5],[6,3,5,4],[7,1,1]],[[3,7,2,9],[2,2,7,7],[8,5,7,6],[9,3,3]]).
q([[4,1,5,1],[0,7,6,2]],[[6,3,7,3],[0,7,6,2]]).
q([[6,1,6],[0,1,1],[7,6,5],[0,7,7]],[[8,3,8],[2,3,3],[9,8,7],[0,7,7]]).
q([[5,6,4],[0,6,5,4]],[[5,6,4],[2,8,7,6]]).
q([[5,4,3],[3,4,7,7],[4,0,0]],[[5,4,3],[5,6,9,9],[6,2,2]]).
q([[0,5,7],[6,2,4,6],[0,3,6],[6,0,5]],[[2,7,9],[8,4,6,8],[2,5,8],[6,0,5]]).
