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
my_odd3(A):-1 is A mod 2.
my_toupper4(A,B):-upcase_atom(A,B),char_code(A,_).
my_tolower5(A,B):-downcase_atom(A,B),char_code(A,_).
my_max_list6(A,B):-max_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_head9([H|_],H).
my_last10(A,B):-last(A,B).
prim(my_succ1/2).
prim(my_tail2/2).
prim(my_odd3/1).
prim(my_toupper4/2).
prim(my_tolower5/2).
prim(my_max_list6/2).
prim(my_reverse7/2).
prim(my_pred8/2).
prim(my_head9/2).
prim(my_last10/2).
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
p([[1,1,3,3],[6,0,0],[0,5,4,1],[3,4,5,2]],[[3,3,5,5],[8,2,2],[2,7,6,3],[5,6,7,4]]).
p([[1,1,7],[3,0,5,6]],[[3,3,9],[5,2,7,8]]).
p([[2,7,3],[4,1,5],[3,2,3],[3,4,6,1]],[[4,9,5],[6,3,7],[5,4,5],[5,6,8,3]]).
p([[1,5,7],[2,0,5]],[[3,7,9],[4,2,7]]).
p([[3,2,2,6],[1,4,4,5]],[[5,4,4,8],[3,6,6,7]]).
q([[2,3,6,5],[3,7,4],[3,1,2,6],[2,6,1]],[[4,5,8,7],[5,9,6],[5,3,4,8],[2,6,1]]).
q([[7,0,4],[7,4,5,0],[2,0,4,3],[4,3,1,5]],[[9,2,6],[9,6,7,2],[4,2,6,5],[4,3,1,5]]).
q([[7,6,1,4],[1,2,7,5],[4,6,0],[4,1,1,0]],[[9,8,3,6],[1,2,7,5],[6,8,2],[6,3,3,2]]).
q([[2,6,5],[7,5,4,1],[5,6,5,3]],[[4,8,7],[7,5,4,1],[7,8,7,5]]).
q([[2,4,3],[6,2,5,7],[4,5,2,1],[4,1,1]],[[4,6,5],[6,2,5,7],[6,7,4,3],[6,3,3]]).
