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
my_set2(A):-list_to_set(A,A).
my_msort3(A,B):-msort(A,B).
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_reverse5(A,B):-reverse(A,B).
prim(my_succ1/2).
prim(my_set2/1).
prim(my_msort3/2).
prim(my_lowercase4/1).
prim(my_reverse5/2).
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
p([[1,3,4,4],[0,6,7,0]],[[3,5,6,6],[2,8,9,2]]).
p([[2,0,6,3],[2,3,1,2],[3,5,5],[6,4,4,7]],[[4,2,8,5],[4,5,3,4],[5,7,7],[8,6,6,9]]).
p([[3,5,2,5],[3,1,7,7]],[[5,7,4,7],[5,3,9,9]]).
p([[1,1,1,0],[4,2,7,2],[1,4,1,2],[4,0,0,7]],[[3,3,3,2],[6,4,9,4],[3,6,3,4],[6,2,2,9]]).
p([[0,0,2],[0,1,6]],[[2,2,4],[2,3,8]]).
q([[5,5,0],[2,5,0,7]],[[7,7,2],[2,5,0,7]]).
q([[7,4,4,4],[7,0,4,4],[2,7,1,2]],[[7,4,4,4],[9,2,6,6],[4,9,3,4]]).
q([[4,7,1],[3,1,5],[3,4,4],[4,2,5,6]],[[6,9,3],[3,1,5],[5,6,6],[4,2,5,6]]).
q([[2,4,6],[3,3,7]],[[2,4,6],[5,5,9]]).
q([[6,1,4,5],[7,7,6]],[[6,1,4,5],[9,9,8]]).
