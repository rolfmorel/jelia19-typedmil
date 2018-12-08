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
my_reverse4(A,B):-reverse(A,B).
prim(my_succ1/2).
prim(my_pred2/2).
prim(my_sumlist3/2).
prim(my_reverse4/2).
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
p([[4,1,6,5],[2,3,1],[0,6,6]],[[6,3,8,7],[4,5,3],[2,8,8]]).
p([[3,7,2],[6,2,1],[7,4,1],[1,3,3]],[[5,9,4],[8,4,3],[9,6,3],[3,5,5]]).
p([[0,2,0,3],[4,1,1,5],[2,2,2]],[[2,4,2,5],[6,3,3,7],[4,4,4]]).
p([[6,6,1,1],[3,7,4,4],[5,5,4],[2,0,2]],[[8,8,3,3],[5,9,6,6],[7,7,6],[4,2,4]]).
p([[5,6,5,0],[6,1,3,6]],[[7,8,7,2],[8,3,5,8]]).
q([[7,5,4],[2,2,4],[1,2,0,2],[7,0,3]],[[7,5,4],[4,4,6],[1,2,0,2],[9,2,5]]).
q([[3,6,1],[1,6,3],[4,4,7,7]],[[5,8,3],[3,8,5],[4,4,7,7]]).
q([[5,2,5],[3,7,1,6]],[[5,2,5],[5,9,3,8]]).
q([[3,3,5],[2,5,5,0],[5,1,6,2],[2,0,1]],[[5,5,7],[2,5,5,0],[5,1,6,2],[4,2,3]]).
q([[7,2,6],[1,1,0,5],[6,3,2,3],[6,5,2]],[[9,4,8],[1,1,0,5],[8,5,4,5],[8,7,4]]).
