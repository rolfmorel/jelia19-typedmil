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
my_toupper2(A,B):-upcase_atom(A,B),char_code(A,_).
my_element3(A,B):-member(B,A).
my_double4(N,M):-M is 2*N,M =< 10.
my_sumlist5(A,B):-sumlist(A,B).
my_min_list6(A,B):-min_list(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_set8(A):-list_to_set(A,A).
my_last9(A,B):-last(A,B).
my_even10(A):-0 is A mod 2.
my_msort11(A,B):-msort(A,B).
prim(my_succ1/2).
prim(my_toupper2/2).
prim(my_element3/2).
prim(my_double4/2).
prim(my_sumlist5/2).
prim(my_min_list6/2).
prim(my_list_to_set7/2).
prim(my_set8/1).
prim(my_last9/2).
prim(my_even10/1).
prim(my_msort11/2).
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
p([[4,4,0,2],[0,4,5,6],[2,6,1]],[[6,6,2,4],[2,6,7,8],[4,8,3]]).
p([[5,4,3,5],[6,6,1,6],[4,1,6,0]],[[7,6,5,7],[8,8,3,8],[6,3,8,2]]).
p([[6,3,3],[5,5,0]],[[8,5,5],[7,7,2]]).
p([[3,5,4,3],[7,4,6,4],[0,7,2,4]],[[5,7,6,5],[9,6,8,6],[2,9,4,6]]).
p([[6,6,0,2],[5,6,1],[6,6,4]],[[8,8,2,4],[7,8,3],[8,8,6]]).
q([[4,4,1,2],[4,7,2,7],[5,0,1,0],[3,6,3,5]],[[6,6,3,4],[4,7,2,7],[7,2,3,2],[5,8,5,7]]).
q([[0,1,1],[5,2,6,2],[6,0,6,2]],[[2,3,3],[5,2,6,2],[8,2,8,4]]).
q([[1,5,7,7],[7,6,2,7],[4,0,4],[0,2,5,7]],[[3,7,9,9],[9,8,4,9],[4,0,4],[0,2,5,7]]).
q([[1,6,7],[3,5,2],[4,3,2]],[[3,8,9],[5,7,4],[4,3,2]]).
q([[4,5,1,0],[5,3,4,4]],[[6,7,3,2],[5,3,4,4]]).
