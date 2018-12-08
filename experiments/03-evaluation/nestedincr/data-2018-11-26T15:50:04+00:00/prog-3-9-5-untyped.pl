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
my_uppercase3(A):-upcase_atom(A,A),char_code(A,_).
my_sumlist4(A,B):-sumlist(A,B).
my_min_list5(A,B):-min_list(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_element7(A,B):-member(B,A).
my_list_to_set8(A,B):-list_to_set(A,B).
my_max_list9(A,B):-max_list(A,B).
my_set10(A):-list_to_set(A,A).
prim(my_succ1/2).
prim(my_msort2/2).
prim(my_uppercase3/1).
prim(my_sumlist4/2).
prim(my_min_list5/2).
prim(my_double6/2).
prim(my_element7/2).
prim(my_list_to_set8/2).
prim(my_max_list9/2).
prim(my_set10/1).
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
p([[0,2,7,2],[7,1,4],[3,7,0,1],[7,5,1,6]],[[2,4,9,4],[9,3,6],[5,9,2,3],[9,7,3,8]]).
p([[2,1,0,1],[3,7,2,6]],[[4,3,2,3],[5,9,4,8]]).
p([[3,2,5],[7,3,4,2],[1,7,4],[2,4,7]],[[5,4,7],[9,5,6,4],[3,9,6],[4,6,9]]).
p([[7,6,7,4],[6,3,5]],[[9,8,9,6],[8,5,7]]).
p([[2,4,4,7],[7,7,4,0]],[[4,6,6,9],[9,9,6,2]]).
q([[7,6,4],[4,1,4],[6,4,4,6],[7,4,2,5]],[[9,8,6],[6,3,6],[6,4,4,6],[7,4,2,5]]).
q([[6,0,5,2],[4,5,0,5]],[[8,2,7,4],[4,5,0,5]]).
q([[2,1,3,4],[2,1,2],[4,3,4],[0,4,4,3]],[[2,1,3,4],[4,3,4],[6,5,6],[0,4,4,3]]).
q([[2,2,6],[7,5,1],[6,0,3,2],[7,5,4,1]],[[2,2,6],[9,7,3],[6,0,3,2],[9,7,6,3]]).
q([[4,1,4],[1,7,4,0],[1,2,6,0]],[[6,3,6],[1,7,4,0],[3,4,8,2]]).
