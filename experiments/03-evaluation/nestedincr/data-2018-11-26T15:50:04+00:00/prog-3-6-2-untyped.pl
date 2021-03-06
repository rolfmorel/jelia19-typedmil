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
my_tolower2(A,B):-downcase_atom(A,B),char_code(A,_).
my_list_to_set3(A,B):-list_to_set(A,B).
my_element4(A,B):-member(B,A).
my_min_list5(A,B):-min_list(A,B).
my_set6(A):-list_to_set(A,A).
my_lowercase7(A):-downcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_tolower2/2).
prim(my_list_to_set3/2).
prim(my_element4/2).
prim(my_min_list5/2).
prim(my_set6/1).
prim(my_lowercase7/1).
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
p([[6,6,0],[4,6,2],[5,0,5],[1,0,3,5]],[[8,8,2],[6,8,4],[7,2,7],[3,2,5,7]]).
p([[7,7,4],[1,7,5,2]],[[9,9,6],[3,9,7,4]]).
p([[5,1,5],[0,5,2,1]],[[7,3,7],[2,7,4,3]]).
p([[3,2,6],[6,2,3],[6,7,0]],[[5,4,8],[8,4,5],[8,9,2]]).
p([[2,7,3,5],[5,5,2],[7,2,3,2]],[[4,9,5,7],[7,7,4],[9,4,5,4]]).
q([[2,3,2,7],[3,3,3,3]],[[4,5,4,9],[3,3,3,3]]).
q([[1,7,0],[2,6,4,3]],[[1,7,0],[4,8,6,5]]).
q([[1,1,5,0],[1,0,4,3],[0,6,2,0]],[[1,1,5,0],[3,2,6,5],[2,8,4,2]]).
q([[3,4,5,1],[1,2,5,5],[6,1,7,1]],[[3,4,5,1],[3,4,7,7],[8,3,9,3]]).
q([[3,1,1],[1,2,4,3]],[[3,1,1],[3,4,6,5]]).
