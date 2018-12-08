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
my_len3(A,B):-length(A,B).
my_last4(A,B):-last(A,B).
my_tail5([_|TL],TL).
my_even6(A):-0 is A mod 2.
my_set7(A):-list_to_set(A,A).
my_list_to_set8(A,B):-list_to_set(A,B).
my_head9([H|_],H).
my_double10(N,M):-M is 2*N,M =< 10.
my_tolower11(A,B):-downcase_atom(A,B),char_code(A,_).
my_element12(A,B):-member(B,A).
prim(my_succ1/2).
prim(my_sumlist2/2).
prim(my_len3/2).
prim(my_last4/2).
prim(my_tail5/2).
prim(my_even6/1).
prim(my_set7/1).
prim(my_list_to_set8/2).
prim(my_head9/2).
prim(my_double10/2).
prim(my_tolower11/2).
prim(my_element12/2).
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
p([[4,4,1],[2,1,6],[5,4,1,1]],[[6,6,3],[4,3,8],[7,6,3,3]]).
p([[2,5,4],[7,1,4,6]],[[4,7,6],[9,3,6,8]]).
p([[3,4,0,6],[2,0,4]],[[5,6,2,8],[4,2,6]]).
p([[7,5,5,2],[6,5,5],[4,3,1],[7,6,7,6]],[[9,7,7,4],[8,7,7],[6,5,3],[9,8,9,8]]).
p([[2,7,2,7],[3,3,5,0],[1,0,2]],[[4,9,4,9],[5,5,7,2],[3,2,4]]).
q([[0,2,7],[6,7,7]],[[2,4,9],[6,7,7]]).
q([[7,5,1,1],[3,5,0,5]],[[9,7,3,3],[3,5,0,5]]).
q([[1,4,0],[1,6,5]],[[1,4,0],[3,8,7]]).
q([[4,1,0],[7,1,1]],[[6,3,2],[7,1,1]]).
q([[2,3,5,1],[2,7,4]],[[4,5,7,3],[2,7,4]]).
