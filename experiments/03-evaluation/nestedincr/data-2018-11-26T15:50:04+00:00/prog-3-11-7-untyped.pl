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
my_len2(A,B):-length(A,B).
my_set3(A):-list_to_set(A,A).
my_odd4(A):-1 is A mod 2.
my_tolower5(A,B):-downcase_atom(A,B),char_code(A,_).
my_double6(N,M):-M is 2*N,M =< 10.
my_pred7(A,B):-succ(B,A),A > 0.
my_sumlist8(A,B):-sumlist(A,B).
my_uppercase9(A):-upcase_atom(A,A),char_code(A,_).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_last11(A,B):-last(A,B).
my_list_to_set12(A,B):-list_to_set(A,B).
prim(my_succ1/2).
prim(my_len2/2).
prim(my_set3/1).
prim(my_odd4/1).
prim(my_tolower5/2).
prim(my_double6/2).
prim(my_pred7/2).
prim(my_sumlist8/2).
prim(my_uppercase9/1).
prim(my_toupper10/2).
prim(my_last11/2).
prim(my_list_to_set12/2).
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
p([[6,4,1],[7,4,6]],[[8,6,3],[9,6,8]]).
p([[1,7,4,1],[5,6,6],[2,4,5,7],[1,7,1,0]],[[3,9,6,3],[7,8,8],[4,6,7,9],[3,9,3,2]]).
p([[6,7,4],[1,5,7,1]],[[8,9,6],[3,7,9,3]]).
p([[0,2,4],[5,4,7],[4,2,3],[5,3,2,5]],[[2,4,6],[7,6,9],[6,4,5],[7,5,4,7]]).
p([[4,6,5],[3,6,5],[7,5,6,1],[2,3,5]],[[6,8,7],[5,8,7],[9,7,8,3],[4,5,7]]).
q([[2,0,3,6],[2,0,2,5]],[[2,0,3,6],[4,2,4,7]]).
q([[2,6,4],[2,4,7],[1,2,6],[2,4,1]],[[4,8,6],[4,6,9],[3,4,8],[2,4,1]]).
q([[3,5,0,0],[2,3,2],[7,7,0,6],[4,1,3,3]],[[5,7,2,2],[4,5,4],[9,9,2,8],[4,1,3,3]]).
q([[4,5,6,3],[2,3,6],[2,4,6,2],[6,7,5,5]],[[6,7,8,5],[2,3,6],[4,6,8,4],[8,9,7,7]]).
q([[4,5,2],[5,0,4]],[[4,5,2],[7,2,6]]).
