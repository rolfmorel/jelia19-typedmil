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
my_min_list2(A,B):-min_list(A,B).
my_reverse3(A,B):-reverse(A,B).
prim(my_succ1/2).
prim(my_min_list2/2).
prim(my_reverse3/2).
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
p([[0,1,4,3],[0,1,4,6]],[[2,3,6,5],[2,3,6,8]]).
p([[4,1,3],[1,7,3]],[[6,3,5],[3,9,5]]).
p([[1,3,0,0],[2,1,6,6],[7,6,4,2]],[[3,5,2,2],[4,3,8,8],[9,8,6,4]]).
p([[6,4,7,4],[5,4,4]],[[8,6,9,6],[7,6,6]]).
p([[5,6,5],[7,2,2]],[[7,8,7],[9,4,4]]).
q([[6,7,6],[5,0,3],[1,1,3,1]],[[6,7,6],[7,2,5],[3,3,5,3]]).
q([[1,4,7],[7,6,7,1],[5,3,7,5]],[[3,6,9],[9,8,9,3],[5,3,7,5]]).
q([[7,2,2],[0,3,7,5]],[[7,2,2],[2,5,9,7]]).
q([[1,4,4],[1,4,7],[7,3,4],[2,3,7,6]],[[1,4,4],[1,4,7],[9,5,6],[4,5,9,8]]).
q([[5,6,2],[6,7,0],[3,0,7,2],[6,3,1]],[[7,8,4],[8,9,2],[5,2,9,4],[6,3,1]]).
