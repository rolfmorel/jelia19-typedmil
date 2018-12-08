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
my_double3(N,M):-M is 2*N,M =< 10.
my_tolower4(A,B):-downcase_atom(A,B),char_code(A,_).
my_tail5([_|TL],TL).
prim(my_succ1/2).
prim(my_pred2/2).
prim(my_double3/2).
prim(my_tolower4/2).
prim(my_tail5/2).
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
p([[1,1,5],[2,5,5],[1,2,4,2]],[[3,3,7],[4,7,7],[3,4,6,4]]).
p([[1,3,6],[0,6,0],[1,1,0]],[[3,5,8],[2,8,2],[3,3,2]]).
p([[0,0,5],[2,3,1]],[[2,2,7],[4,5,3]]).
p([[0,2,2],[4,7,1,5]],[[2,4,4],[6,9,3,7]]).
p([[6,6,0],[7,2,3],[6,6,6]],[[8,8,2],[9,4,5],[8,8,8]]).
q([[5,2,5],[5,1,0],[2,0,5],[0,0,2]],[[5,2,5],[7,3,2],[4,2,7],[2,2,4]]).
q([[6,5,2,5],[2,0,6,3],[7,2,0]],[[6,5,2,5],[4,2,8,5],[9,4,2]]).
q([[1,2,6],[1,7,5,3],[3,7,0],[5,0,5]],[[1,2,6],[3,9,7,5],[5,9,2],[7,2,7]]).
q([[1,5,1,2],[6,2,2,7],[0,4,0,4],[3,2,1]],[[3,7,3,4],[8,4,4,9],[0,4,0,4],[5,4,3]]).
q([[4,6,4,4],[7,0,7]],[[6,8,6,6],[7,0,7]]).
