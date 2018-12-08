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
my_reverse2(A,B):-reverse(A,B).
my_sumlist3(A,B):-sumlist(A,B).
prim(my_succ1/2).
prim(my_reverse2/2).
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
p([[6,1,6],[1,7,0],[1,0,3]],[[8,3,8],[3,9,2],[3,2,5]]).
p([[7,6,4],[0,5,0,2],[3,4,6,6]],[[9,8,6],[2,7,2,4],[5,6,8,8]]).
p([[5,1,0,5],[4,0,6,1]],[[7,3,2,7],[6,2,8,3]]).
p([[4,4,1,1],[1,1,5,2],[4,0,6,4],[0,3,1,3]],[[6,6,3,3],[3,3,7,4],[6,2,8,6],[2,5,3,5]]).
p([[2,0,1],[0,4,5,6],[3,0,1,2]],[[4,2,3],[2,6,7,8],[5,2,3,4]]).
q([[2,6,1],[7,3,3,1],[6,5,0]],[[2,6,1],[9,5,5,3],[8,7,2]]).
q([[4,0,0,2],[4,5,5],[2,2,4]],[[6,2,2,4],[6,7,7],[2,2,4]]).
q([[7,3,5,3],[6,5,2,1],[3,7,6,4]],[[7,3,5,3],[8,7,4,3],[5,9,8,6]]).
q([[5,5,4,7],[6,5,7],[1,2,7,5],[6,2,4]],[[7,7,6,9],[6,5,7],[1,2,7,5],[8,4,6]]).
q([[3,5,0,0],[1,0,3],[1,5,4,7]],[[5,7,2,2],[3,2,5],[1,5,4,7]]).
