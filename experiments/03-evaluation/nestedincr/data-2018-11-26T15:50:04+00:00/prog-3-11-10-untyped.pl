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
my_even2(A):-0 is A mod 2.
my_lowercase3(A):-downcase_atom(A,A),char_code(A,_).
my_flatten4(A,B):-flatten(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
my_sumlist6(A,B):-sumlist(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_odd8(A):-1 is A mod 2.
my_reverse9(A,B):-reverse(A,B).
my_tail10([_|TL],TL).
my_tolower11(A,B):-downcase_atom(A,B),char_code(A,_).
my_min_list12(A,B):-min_list(A,B).
prim(my_succ1/2).
prim(my_even2/1).
prim(my_lowercase3/1).
prim(my_flatten4/2).
prim(my_pred5/2).
prim(my_sumlist6/2).
prim(my_list_to_set7/2).
prim(my_odd8/1).
prim(my_reverse9/2).
prim(my_tail10/2).
prim(my_tolower11/2).
prim(my_min_list12/2).
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
p([[2,3,4,4],[4,3,5,7],[3,1,1]],[[4,5,6,6],[6,5,7,9],[5,3,3]]).
p([[4,1,1],[1,6,1],[4,7,7]],[[6,3,3],[3,8,3],[6,9,9]]).
p([[2,3,4,2],[6,6,7],[0,1,6],[7,3,7]],[[4,5,6,4],[8,8,9],[2,3,8],[9,5,9]]).
p([[7,3,4,4],[5,3,1,2],[6,7,0]],[[9,5,6,6],[7,5,3,4],[8,9,2]]).
p([[1,4,3],[3,6,0,7],[6,6,2,5],[5,4,6,1]],[[3,6,5],[5,8,2,9],[8,8,4,7],[7,6,8,3]]).
q([[5,5,5,4],[5,4,6,1],[4,4,0,0],[3,7,7,5]],[[5,5,5,4],[7,6,8,3],[4,4,0,0],[5,9,9,7]]).
q([[5,4,4],[6,2,6]],[[5,4,4],[8,4,8]]).
q([[3,1,6],[5,4,3,6],[4,1,7],[2,3,1,0]],[[5,3,8],[5,4,3,6],[4,1,7],[4,5,3,2]]).
q([[5,6,1],[2,7,1,1],[6,0,6],[5,7,1]],[[5,6,1],[4,9,3,3],[6,0,6],[7,9,3]]).
q([[7,5,0],[2,6,5,0],[2,5,2,5]],[[7,5,0],[4,8,7,2],[4,7,4,7]]).
