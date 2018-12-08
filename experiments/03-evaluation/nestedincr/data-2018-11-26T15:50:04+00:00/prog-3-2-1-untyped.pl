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
my_tail2([_|TL],TL).
my_odd3(A):-1 is A mod 2.
prim(my_succ1/2).
prim(my_tail2/2).
prim(my_odd3/1).
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
p([[6,4,0],[6,2,2],[2,3,1]],[[8,6,2],[8,4,4],[4,5,3]]).
p([[0,7,4],[4,7,7]],[[2,9,6],[6,9,9]]).
p([[4,6,4,1],[4,1,2],[1,4,5,2],[6,2,2,4]],[[6,8,6,3],[6,3,4],[3,6,7,4],[8,4,4,6]]).
p([[1,1,0],[5,4,5,2],[3,2,0,4],[7,2,5]],[[3,3,2],[7,6,7,4],[5,4,2,6],[9,4,7]]).
p([[4,0,4,7],[2,4,5],[2,5,3,6],[3,5,1,1]],[[6,2,6,9],[4,6,7],[4,7,5,8],[5,7,3,3]]).
q([[4,6,2],[2,6,1,6],[1,5,7],[1,5,0,0]],[[6,8,4],[2,6,1,6],[1,5,7],[3,7,2,2]]).
q([[3,7,5],[4,6,1,0],[2,5,6,1],[6,0,4,1]],[[5,9,7],[4,6,1,0],[4,7,8,3],[8,2,6,3]]).
q([[2,4,2,7],[7,6,7]],[[4,6,4,9],[7,6,7]]).
q([[7,4,4],[1,7,5,0],[3,5,6],[4,5,0]],[[9,6,6],[3,9,7,2],[3,5,6],[6,7,2]]).
q([[1,7,1,7],[7,4,4,4],[7,7,5],[5,4,4,7]],[[1,7,1,7],[7,4,4,4],[9,9,7],[7,6,6,9]]).
