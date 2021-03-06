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
my_odd3(A):-1 is A mod 2.
my_flatten4(A,B):-flatten(A,B).
my_set5(A):-list_to_set(A,A).
my_max_list6(A,B):-max_list(A,B).
my_element7(A,B):-member(B,A).
my_list_to_set8(A,B):-list_to_set(A,B).
my_msort9(A,B):-msort(A,B).
prim(my_succ1/2).
prim(my_tolower2/2).
prim(my_odd3/1).
prim(my_flatten4/2).
prim(my_set5/1).
prim(my_max_list6/2).
prim(my_element7/2).
prim(my_list_to_set8/2).
prim(my_msort9/2).
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
p([[4,3,1,3],[2,2,1],[0,1,2,7],[6,6,7]],[[6,5,3,5],[4,4,3],[2,3,4,9],[8,8,9]]).
p([[5,3,5,7],[5,5,0],[2,5,3,2],[6,0,0,1]],[[7,5,7,9],[7,7,2],[4,7,5,4],[8,2,2,3]]).
p([[0,2,6,0],[6,0,7]],[[2,4,8,2],[8,2,9]]).
p([[4,5,3],[4,5,6],[4,4,1,5]],[[6,7,5],[6,7,8],[6,6,3,7]]).
p([[5,5,4,7],[6,0,4,0],[0,5,6],[7,1,6]],[[7,7,6,9],[8,2,6,2],[2,7,8],[9,3,8]]).
q([[6,4,2,7],[7,7,1,1],[5,4,5]],[[8,6,4,9],[9,9,3,3],[5,4,5]]).
q([[5,5,5],[0,0,3,5],[7,3,6],[4,3,6,4]],[[7,7,7],[0,0,3,5],[9,5,8],[6,5,8,6]]).
q([[1,5,1],[3,5,2],[1,0,3]],[[3,7,3],[5,7,4],[1,0,3]]).
q([[6,1,2],[0,2,7],[6,0,0,3],[4,2,6,6]],[[8,3,4],[2,4,9],[6,0,0,3],[4,2,6,6]]).
q([[3,6,4,4],[3,5,5]],[[5,8,6,6],[3,5,5]]).
