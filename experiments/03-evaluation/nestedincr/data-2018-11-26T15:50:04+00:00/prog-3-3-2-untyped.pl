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
my_tail3([_|TL],TL).
my_flatten4(A,B):-flatten(A,B).
prim(my_succ1/2).
prim(my_reverse2/2).
prim(my_tail3/2).
prim(my_flatten4/2).
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
p([[6,4,4,2],[1,3,2,3]],[[8,6,6,4],[3,5,4,5]]).
p([[1,0,2],[6,3,0],[1,1,5]],[[3,2,4],[8,5,2],[3,3,7]]).
p([[6,6,1],[3,2,3,0],[2,7,2],[3,3,4]],[[8,8,3],[5,4,5,2],[4,9,4],[5,5,6]]).
p([[6,3,4,3],[6,5,4]],[[8,5,6,5],[8,7,6]]).
p([[2,4,1,1],[6,2,3],[2,0,0,5],[7,6,1,5]],[[4,6,3,3],[8,4,5],[4,2,2,7],[9,8,3,7]]).
q([[0,6,1,6],[7,6,0]],[[2,8,3,8],[7,6,0]]).
q([[5,6,2,5],[6,0,4]],[[7,8,4,7],[6,0,4]]).
q([[0,6,5],[1,5,3,4],[5,5,0,2],[6,1,5,5]],[[2,8,7],[3,7,5,6],[7,7,2,4],[6,1,5,5]]).
q([[3,4,5],[7,7,2],[4,3,6,7],[5,7,5,4]],[[3,4,5],[9,9,4],[6,5,8,9],[5,7,5,4]]).
q([[3,4,6],[7,7,2],[6,5,6,4]],[[3,4,6],[9,9,4],[8,7,8,6]]).
