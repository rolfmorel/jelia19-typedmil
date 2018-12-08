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
my_last2(A,B):-last(A,B).
my_lowercase3(A):-downcase_atom(A,A),char_code(A,_).
my_uppercase4(A):-upcase_atom(A,A),char_code(A,_).
my_len5(A,B):-length(A,B).
my_flatten6(A,B):-flatten(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
prim(my_succ1/2).
prim(my_last2/2).
prim(my_lowercase3/1).
prim(my_uppercase4/1).
prim(my_len5/2).
prim(my_flatten6/2).
prim(my_pred7/2).
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
p([[0,0,4,6],[6,2,6,0]],[[2,2,6,8],[8,4,8,2]]).
p([[2,1,3],[6,0,2,4]],[[4,3,5],[8,2,4,6]]).
p([[6,4,3],[4,1,0,5]],[[8,6,5],[6,3,2,7]]).
p([[7,7,1],[2,1,0,7]],[[9,9,3],[4,3,2,9]]).
p([[3,0,5,6],[3,1,0,4]],[[5,2,7,8],[5,3,2,6]]).
q([[4,7,1,0],[6,0,1],[5,3,2],[6,5,4]],[[6,9,3,2],[8,2,3],[5,3,2],[8,7,6]]).
q([[0,7,0],[3,5,6,6],[3,7,1,2],[2,7,7,1]],[[2,9,2],[3,5,6,6],[5,9,3,4],[2,7,7,1]]).
q([[5,2,2],[5,3,5,5],[2,4,7,6],[5,7,6]],[[5,2,2],[5,3,5,5],[4,6,9,8],[7,9,8]]).
q([[5,5,5,7],[6,0,0,0]],[[7,7,7,9],[6,0,0,0]]).
q([[4,2,7,4],[7,4,2]],[[6,4,9,6],[7,4,2]]).
