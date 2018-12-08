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
my_toupper3(A,B):-upcase_atom(A,B),char_code(A,_).
my_double4(N,M):-M is 2*N,M =< 10.
my_tolower5(A,B):-downcase_atom(A,B),char_code(A,_).
my_element6(A,B):-member(B,A).
my_max_list7(A,B):-max_list(A,B).
my_tail8([_|TL],TL).
my_pred9(A,B):-succ(B,A),A > 0.
my_head10([H|_],H).
my_reverse11(A,B):-reverse(A,B).
prim(my_succ1/2).
prim(my_msort2/2).
prim(my_toupper3/2).
prim(my_double4/2).
prim(my_tolower5/2).
prim(my_element6/2).
prim(my_max_list7/2).
prim(my_tail8/2).
prim(my_pred9/2).
prim(my_head10/2).
prim(my_reverse11/2).
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
p([[5,6,2,6],[2,3,1,7],[3,6,1]],[[7,8,4,8],[4,5,3,9],[5,8,3]]).
p([[6,2,4],[4,4,2,5],[7,6,0,0]],[[8,4,6],[6,6,4,7],[9,8,2,2]]).
p([[4,5,7,1],[0,3,6],[6,5,0],[3,7,7,0]],[[6,7,9,3],[2,5,8],[8,7,2],[5,9,9,2]]).
p([[5,3,4,3],[2,7,7]],[[7,5,6,5],[4,9,9]]).
p([[4,7,4,1],[7,6,6,3],[0,4,2,7],[4,1,2]],[[6,9,6,3],[9,8,8,5],[2,6,4,9],[6,3,4]]).
q([[7,6,4,5],[7,1,4],[3,0,0,6]],[[9,8,6,7],[7,1,4],[5,2,2,8]]).
q([[1,5,7],[2,3,3],[2,4,0,5]],[[3,7,9],[2,3,3],[4,6,2,7]]).
q([[4,4,7],[4,6,1],[4,6,1],[7,3,7,3]],[[6,6,9],[4,6,1],[4,6,1],[9,5,9,5]]).
q([[6,6,1,7],[4,2,1,1],[3,4,0],[0,0,5]],[[8,8,3,9],[6,4,3,3],[3,4,0],[2,2,7]]).
q([[4,7,1],[0,3,6,4]],[[6,9,3],[0,3,6,4]]).
