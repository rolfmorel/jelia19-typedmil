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
my_head3([H|_],H).
my_even4(A):-0 is A mod 2.
my_list_to_set5(A,B):-list_to_set(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_uppercase7(A):-upcase_atom(A,A),char_code(A,_).
my_set8(A):-list_to_set(A,A).
my_pred9(A,B):-succ(B,A),A > 0.
my_flatten10(A,B):-flatten(A,B).
my_max_list11(A,B):-max_list(A,B).
prim(my_succ1/2).
prim(my_tolower2/2).
prim(my_head3/2).
prim(my_even4/1).
prim(my_list_to_set5/2).
prim(my_sumlist6/2).
prim(my_uppercase7/1).
prim(my_set8/1).
prim(my_pred9/2).
prim(my_flatten10/2).
prim(my_max_list11/2).
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
p([[4,4,0],[6,1,3],[5,0,0,5],[7,1,4]],[[6,6,2],[8,3,5],[7,2,2,7],[9,3,6]]).
p([[3,0,2],[5,5,5,1],[4,5,1,4],[6,2,1,5]],[[5,2,4],[7,7,7,3],[6,7,3,6],[8,4,3,7]]).
p([[0,1,3,7],[7,2,1],[7,4,4,7]],[[2,3,5,9],[9,4,3],[9,6,6,9]]).
p([[2,1,1,5],[5,6,2],[7,7,7],[6,7,5,2]],[[4,3,3,7],[7,8,4],[9,9,9],[8,9,7,4]]).
p([[6,5,2],[5,4,3,2]],[[8,7,4],[7,6,5,4]]).
q([[2,1,7,1],[2,0,1],[6,2,5]],[[4,3,9,3],[4,2,3],[6,2,5]]).
q([[0,1,6,4],[3,2,5]],[[0,1,6,4],[5,4,7]]).
q([[1,4,4,3],[2,4,4],[1,4,4,5],[7,1,0,1]],[[1,4,4,3],[2,4,4],[3,6,6,7],[9,3,2,3]]).
q([[4,6,3,0],[4,3,6,0],[3,4,2]],[[4,6,3,0],[6,5,8,2],[5,6,4]]).
q([[0,5,2,1],[7,5,7,5],[2,3,0]],[[0,5,2,1],[9,7,9,7],[4,5,2]]).
