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
my_set4(A):-list_to_set(A,A).
my_len5(A,B):-length(A,B).
my_max_list6(A,B):-max_list(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
prim(my_succ1/2).
prim(my_min_list2/2).
prim(my_reverse3/2).
prim(my_set4/1).
prim(my_len5/2).
prim(my_max_list6/2).
prim(my_list_to_set7/2).
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
p([[3,4,7],[2,6,5],[0,4,5,2],[5,3,6]],[[5,6,9],[4,8,7],[2,6,7,4],[7,5,8]]).
p([[2,2,4],[5,7,1],[6,3,6,3],[4,3,6,2]],[[4,4,6],[7,9,3],[8,5,8,5],[6,5,8,4]]).
p([[3,3,1],[1,2,3],[6,4,7,4],[6,4,0,6]],[[5,5,3],[3,4,5],[8,6,9,6],[8,6,2,8]]).
p([[6,0,2],[5,5,1,6],[1,1,7,0]],[[8,2,4],[7,7,3,8],[3,3,9,2]]).
p([[4,6,0,6],[2,5,0,6],[1,0,3,7]],[[6,8,2,8],[4,7,2,8],[3,2,5,9]]).
q([[2,0,1,7],[4,2,4]],[[2,0,1,7],[6,4,6]]).
q([[7,5,6,1],[4,3,2],[0,6,7,6]],[[9,7,8,3],[6,5,4],[0,6,7,6]]).
q([[3,7,6],[1,3,3,5]],[[3,7,6],[3,5,5,7]]).
q([[3,3,3,5],[5,4,0],[6,2,4,4],[6,2,4,6]],[[5,5,5,7],[7,6,2],[8,4,6,6],[6,2,4,6]]).
q([[6,1,1,0],[7,6,4,3]],[[8,3,3,2],[7,6,4,3]]).
