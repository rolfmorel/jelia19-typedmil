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
my_max_list2(A,B):-max_list(A,B).
my_len3(A,B):-length(A,B).
my_tail4([_|TL],TL).
my_list_to_set5(A,B):-list_to_set(A,B).
my_last6(A,B):-last(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_odd8(A):-1 is A mod 2.
my_tolower9(A,B):-downcase_atom(A,B),char_code(A,_).
my_even10(A):-0 is A mod 2.
my_double11(N,M):-M is 2*N,M =< 10.
my_head12([H|_],H).
my_lowercase13(A):-downcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_max_list2/2).
prim(my_len3/2).
prim(my_tail4/2).
prim(my_list_to_set5/2).
prim(my_last6/2).
prim(my_sumlist7/2).
prim(my_odd8/1).
prim(my_tolower9/2).
prim(my_even10/1).
prim(my_double11/2).
prim(my_head12/2).
prim(my_lowercase13/1).
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
p([[4,3,5,2],[2,2,4],[5,6,6]],[[6,5,7,4],[4,4,6],[7,8,8]]).
p([[3,4,0,0],[7,2,7],[1,2,2,0]],[[5,6,2,2],[9,4,9],[3,4,4,2]]).
p([[0,3,3],[0,4,5],[4,7,6]],[[2,5,5],[2,6,7],[6,9,8]]).
p([[4,7,1],[6,1,7]],[[6,9,3],[8,3,9]]).
p([[2,3,3],[3,1,4],[0,6,4,7],[7,5,2,4]],[[4,5,5],[5,3,6],[2,8,6,9],[9,7,4,6]]).
q([[7,0,6,7],[7,4,0],[5,5,4]],[[9,2,8,9],[7,4,0],[7,7,6]]).
q([[3,4,7],[4,2,5],[1,5,0]],[[5,6,9],[4,2,5],[3,7,2]]).
q([[7,6,4,6],[1,1,0,5],[7,5,6]],[[9,8,6,8],[3,3,2,7],[7,5,6]]).
q([[1,3,0],[5,4,1]],[[1,3,0],[7,6,3]]).
q([[4,7,2],[2,4,4],[2,2,0,1],[7,7,1]],[[4,7,2],[2,4,4],[4,4,2,3],[9,9,3]]).
