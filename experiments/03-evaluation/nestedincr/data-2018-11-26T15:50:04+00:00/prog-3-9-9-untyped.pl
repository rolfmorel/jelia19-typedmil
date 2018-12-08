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
my_set3(A):-list_to_set(A,A).
my_last4(A,B):-last(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
my_double6(N,M):-M is 2*N,M =< 10.
my_uppercase7(A):-upcase_atom(A,A),char_code(A,_).
my_element8(A,B):-member(B,A).
my_odd9(A):-1 is A mod 2.
my_len10(A,B):-length(A,B).
prim(my_succ1/2).
prim(my_min_list2/2).
prim(my_set3/1).
prim(my_last4/2).
prim(my_pred5/2).
prim(my_double6/2).
prim(my_uppercase7/1).
prim(my_element8/2).
prim(my_odd9/1).
prim(my_len10/2).
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
p([[2,0,2],[7,3,4]],[[4,2,4],[9,5,6]]).
p([[6,0,3,6],[1,7,4],[6,3,3,1]],[[8,2,5,8],[3,9,6],[8,5,5,3]]).
p([[3,2,7,7],[2,7,4]],[[5,4,9,9],[4,9,6]]).
p([[1,4,3,2],[7,1,4,3]],[[3,6,5,4],[9,3,6,5]]).
p([[6,7,0],[2,5,4,3],[3,0,1]],[[8,9,2],[4,7,6,5],[5,2,3]]).
q([[2,6,0,0],[6,3,6,7],[2,7,4],[1,0,7]],[[2,6,0,0],[8,5,8,9],[2,7,4],[3,2,9]]).
q([[7,0,0,7],[0,5,1,3],[4,5,5,4]],[[9,2,2,9],[0,5,1,3],[6,7,7,6]]).
q([[0,6,7,1],[1,4,6],[5,3,5,4]],[[2,8,9,3],[1,4,6],[7,5,7,6]]).
q([[6,4,4],[5,2,4],[0,3,1,5]],[[8,6,6],[5,2,4],[2,5,3,7]]).
q([[1,5,7],[4,5,2,1]],[[3,7,9],[4,5,2,1]]).
