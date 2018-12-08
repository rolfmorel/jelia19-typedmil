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
my_element2(A,B):-member(B,A).
prim(my_succ1/2).
prim(my_element2/2).
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
p([[5,1,0],[4,6,0,3],[3,1,4,4]],[[7,3,2],[6,8,2,5],[5,3,6,6]]).
p([[0,4,2],[6,3,3],[5,7,3,5]],[[2,6,4],[8,5,5],[7,9,5,7]]).
p([[1,1,5],[0,3,2],[7,1,7],[2,5,6]],[[3,3,7],[2,5,4],[9,3,9],[4,7,8]]).
p([[3,4,5],[1,0,6],[1,7,7],[4,1,7]],[[5,6,7],[3,2,8],[3,9,9],[6,3,9]]).
p([[0,3,0,0],[1,3,3,6],[3,0,3,6],[7,7,6,3]],[[2,5,2,2],[3,5,5,8],[5,2,5,8],[9,9,8,5]]).
q([[2,3,0],[4,2,5,2]],[[2,3,0],[6,4,7,4]]).
q([[0,0,2],[1,3,0],[1,5,3]],[[2,2,4],[1,3,0],[3,7,5]]).
q([[7,0,6],[0,1,0,5],[7,5,1,2],[2,3,1,3]],[[9,2,8],[2,3,2,7],[7,5,1,2],[4,5,3,5]]).
q([[6,1,7,2],[6,2,4,7]],[[6,1,7,2],[8,4,6,9]]).
q([[6,1,1,7],[0,4,2]],[[6,1,1,7],[2,6,4]]).
