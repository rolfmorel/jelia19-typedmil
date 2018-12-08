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
my_sumlist2(A,B):-sumlist(A,B).
my_toupper3(A,B):-upcase_atom(A,B),char_code(A,_).
my_min_list4(A,B):-min_list(A,B).
prim(my_succ1/2).
prim(my_sumlist2/2).
prim(my_toupper3/2).
prim(my_min_list4/2).
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
p([[2,3,0,6],[0,7,7,0],[4,5,1],[4,3,2]],[[4,5,2,8],[2,9,9,2],[6,7,3],[6,5,4]]).
p([[5,0,3],[4,1,2],[2,7,2]],[[7,2,5],[6,3,4],[4,9,4]]).
p([[3,3,6,7],[5,7,0,7]],[[5,5,8,9],[7,9,2,9]]).
p([[2,3,1,0],[2,5,2],[3,3,2]],[[4,5,3,2],[4,7,4],[5,5,4]]).
p([[3,7,7],[6,0,0]],[[5,9,9],[8,2,2]]).
q([[2,1,5],[6,5,7,3],[7,2,4],[7,7,5,3]],[[4,3,7],[8,7,9,5],[7,2,4],[7,7,5,3]]).
q([[6,5,7,6],[1,3,5,3]],[[8,7,9,8],[1,3,5,3]]).
q([[6,1,5],[7,3,1]],[[6,1,5],[9,5,3]]).
q([[4,4,3],[6,0,1,6]],[[4,4,3],[8,2,3,8]]).
q([[1,5,3,2],[4,0,5],[6,4,4]],[[1,5,3,2],[6,2,7],[8,6,6]]).
