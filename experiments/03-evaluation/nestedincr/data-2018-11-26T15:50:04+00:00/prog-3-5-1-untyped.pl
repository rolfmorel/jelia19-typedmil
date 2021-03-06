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
my_max_list2(A,B):-max_list(A,B).
my_reverse3(A,B):-reverse(A,B).
my_head4([H|_],H).
my_min_list5(A,B):-min_list(A,B).
my_even6(A):-0 is A mod 2.
prim(my_succ1/2).
prim(my_max_list2/2).
prim(my_reverse3/2).
prim(my_head4/2).
prim(my_min_list5/2).
prim(my_even6/1).
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
p([[6,6,2,5],[3,3,2,1]],[[8,8,4,7],[5,5,4,3]]).
p([[1,7,6,7],[4,4,7,0],[6,6,0,1]],[[3,9,8,9],[6,6,9,2],[8,8,2,3]]).
p([[1,3,4,0],[0,6,6,4]],[[3,5,6,2],[2,8,8,6]]).
p([[7,4,2,1],[5,3,4],[5,1,1,3]],[[9,6,4,3],[7,5,6],[7,3,3,5]]).
p([[0,5,1,2],[3,3,6],[4,3,4]],[[2,7,3,4],[5,5,8],[6,5,6]]).
q([[4,6,4,0],[7,3,0],[3,0,3],[4,5,0,0]],[[6,8,6,2],[7,3,0],[5,2,5],[6,7,2,2]]).
q([[2,1,4,2],[4,6,1,2],[2,1,5],[7,2,5,0]],[[4,3,6,4],[4,6,1,2],[2,1,5],[9,4,7,2]]).
q([[2,6,1],[1,0,3],[6,1,4],[5,3,2]],[[2,6,1],[3,2,5],[6,1,4],[7,5,4]]).
q([[5,0,1,4],[4,4,5,3],[5,5,7],[4,7,6,4]],[[7,2,3,6],[4,4,5,3],[7,7,9],[4,7,6,4]]).
q([[3,4,2],[1,3,4,3],[2,3,7,0],[3,1,0]],[[3,4,2],[3,5,6,5],[4,5,9,2],[3,1,0]]).
