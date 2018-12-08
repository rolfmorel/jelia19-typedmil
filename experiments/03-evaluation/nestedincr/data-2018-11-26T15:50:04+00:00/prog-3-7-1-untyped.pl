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
my_msort2(A,B):-msort(A,B).
my_head3([H|_],H).
my_reverse4(A,B):-reverse(A,B).
my_odd5(A):-1 is A mod 2.
my_min_list6(A,B):-min_list(A,B).
my_tail7([_|TL],TL).
my_flatten8(A,B):-flatten(A,B).
prim(my_succ1/2).
prim(my_msort2/2).
prim(my_head3/2).
prim(my_reverse4/2).
prim(my_odd5/1).
prim(my_min_list6/2).
prim(my_tail7/2).
prim(my_flatten8/2).
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
p([[1,3,5],[6,3,5,3]],[[3,5,7],[8,5,7,5]]).
p([[0,3,6],[5,2,4,6],[7,7,0],[3,4,7]],[[2,5,8],[7,4,6,8],[9,9,2],[5,6,9]]).
p([[6,5,5],[6,7,5,1]],[[8,7,7],[8,9,7,3]]).
p([[5,4,0],[6,4,7,4]],[[7,6,2],[8,6,9,6]]).
p([[5,6,5,1],[0,4,5],[0,6,1]],[[7,8,7,3],[2,6,7],[2,8,3]]).
q([[3,6,1],[0,3,0]],[[5,8,3],[0,3,0]]).
q([[2,4,5],[1,2,0],[3,7,1],[7,4,3,7]],[[4,6,7],[3,4,2],[3,7,1],[7,4,3,7]]).
q([[3,2,2],[4,6,1]],[[5,4,4],[4,6,1]]).
q([[2,1,3],[7,1,1,1]],[[2,1,3],[9,3,3,3]]).
q([[7,3,6,5],[7,5,5,2],[5,2,0],[6,1,4]],[[9,5,8,7],[9,7,7,4],[5,2,0],[8,3,6]]).
