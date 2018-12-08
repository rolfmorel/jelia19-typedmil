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
my_tail3([_|TL],TL).
my_last4(A,B):-last(A,B).
my_uppercase5(A):-upcase_atom(A,A),char_code(A,_).
my_max_list6(A,B):-max_list(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_msort8(A,B):-msort(A,B).
prim(my_succ1/2).
prim(my_tolower2/2).
prim(my_tail3/2).
prim(my_last4/2).
prim(my_uppercase5/1).
prim(my_max_list6/2).
prim(my_pred7/2).
prim(my_msort8/2).
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
p([[3,5,1],[3,4,5,3],[6,2,4]],[[5,7,3],[5,6,7,5],[8,4,6]]).
p([[6,1,7,7],[6,4,4],[3,1,7,2]],[[8,3,9,9],[8,6,6],[5,3,9,4]]).
p([[1,4,4],[2,0,2,7],[6,4,4,3],[6,2,2]],[[3,6,6],[4,2,4,9],[8,6,6,5],[8,4,4]]).
p([[1,6,2,4],[0,2,6]],[[3,8,4,6],[2,4,8]]).
p([[2,5,4],[2,1,0],[7,2,6,6],[7,0,3]],[[4,7,6],[4,3,2],[9,4,8,8],[9,2,5]]).
q([[1,2,6,0],[6,2,4,2],[0,0,3,5],[1,5,1,1]],[[1,2,6,0],[8,4,6,4],[0,0,3,5],[3,7,3,3]]).
q([[2,3,6],[4,4,3]],[[2,3,6],[6,6,5]]).
q([[1,0,3],[7,2,1,2],[1,3,3,2],[0,7,7]],[[3,2,5],[9,4,3,4],[3,5,5,4],[0,7,7]]).
q([[6,7,4],[1,6,2]],[[6,7,4],[3,8,4]]).
q([[4,7,1],[7,4,3],[7,3,6],[5,7,1,6]],[[4,7,1],[9,6,5],[9,5,8],[5,7,1,6]]).
