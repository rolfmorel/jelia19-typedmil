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
my_set4(A):-list_to_set(A,A).
my_len5(A,B):-length(A,B).
my_last6(A,B):-last(A,B).
prim(my_succ1/2).
prim(my_pred2/2).
prim(my_sumlist3/2).
prim(my_set4/1).
prim(my_len5/2).
prim(my_last6/2).
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
p([[4,1,5,5],[3,1,4,6]],[[6,3,7,7],[5,3,6,8]]).
p([[7,2,3],[1,1,6,4],[2,7,0,1],[4,1,7,6]],[[9,4,5],[3,3,8,6],[4,9,2,3],[6,3,9,8]]).
p([[6,5,4],[2,6,3,4],[3,5,5,7]],[[8,7,6],[4,8,5,6],[5,7,7,9]]).
p([[7,0,0],[3,4,3],[5,4,6],[5,1,6,6]],[[9,2,2],[5,6,5],[7,6,8],[7,3,8,8]]).
p([[1,4,4],[1,2,4]],[[3,6,6],[3,4,6]]).
q([[5,1,5,2],[4,1,2]],[[5,1,5,2],[6,3,4]]).
q([[4,4,1],[1,4,5]],[[6,6,3],[1,4,5]]).
q([[7,1,2,2],[6,2,6],[6,5,7,5]],[[9,3,4,4],[6,2,6],[8,7,9,7]]).
q([[3,2,0],[4,2,3,1],[5,3,4,5],[0,1,2]],[[5,4,2],[6,4,5,3],[7,5,6,7],[0,1,2]]).
q([[4,0,3,2],[4,2,4,3],[4,1,6,0],[0,2,2]],[[4,0,3,2],[6,4,6,5],[6,3,8,2],[2,4,4]]).
