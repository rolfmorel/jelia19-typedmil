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
my_tail2([_|TL],TL).
my_list_to_set3(A,B):-list_to_set(A,B).
my_reverse4(A,B):-reverse(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_tolower6(A,B):-downcase_atom(A,B),char_code(A,_).
prim(my_succ1/2).
prim(my_tail2/2).
prim(my_list_to_set3/2).
prim(my_reverse4/2).
prim(my_double5/2).
prim(my_tolower6/2).
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
p([[4,4,6],[5,4,7,4]],[[6,6,8],[7,6,9,6]]).
p([[6,0,5,1],[6,2,3,0]],[[8,2,7,3],[8,4,5,2]]).
p([[1,4,4],[0,6,4],[4,4,5]],[[3,6,6],[2,8,6],[6,6,7]]).
p([[3,1,2,3],[1,7,3],[4,4,0],[5,3,3]],[[5,3,4,5],[3,9,5],[6,6,2],[7,5,5]]).
p([[2,1,0,6],[1,0,5,3]],[[4,3,2,8],[3,2,7,5]]).
q([[1,3,3],[2,5,5,5]],[[3,5,5],[2,5,5,5]]).
q([[4,4,7,3],[5,1,2,7],[6,6,1],[2,1,5]],[[6,6,9,5],[5,1,2,7],[8,8,3],[4,3,7]]).
q([[1,3,3,4],[0,5,4,2],[4,6,2,2],[2,3,5,2]],[[3,5,5,6],[2,7,6,4],[4,6,2,2],[4,5,7,4]]).
q([[4,2,1],[2,0,0],[1,5,6,2]],[[4,2,1],[4,2,2],[3,7,8,4]]).
q([[5,2,1],[7,2,0,7]],[[5,2,1],[9,4,2,9]]).
