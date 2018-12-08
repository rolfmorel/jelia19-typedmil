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
my_lowercase2(A):-downcase_atom(A,A),char_code(A,_).
my_max_list3(A,B):-max_list(A,B).
my_min_list4(A,B):-min_list(A,B).
my_head5([H|_],H).
my_odd6(A):-1 is A mod 2.
my_tail7([_|TL],TL).
my_msort8(A,B):-msort(A,B).
my_tolower9(A,B):-downcase_atom(A,B),char_code(A,_).
my_last10(A,B):-last(A,B).
my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_lowercase2/1).
prim(my_max_list3/2).
prim(my_min_list4/2).
prim(my_head5/2).
prim(my_odd6/1).
prim(my_tail7/2).
prim(my_msort8/2).
prim(my_tolower9/2).
prim(my_last10/2).
prim(my_uppercase11/1).
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
p([[2,3,1,7],[5,7,7],[1,1,4]],[[4,5,3,9],[7,9,9],[3,3,6]]).
p([[3,4,4,4],[6,0,5],[4,3,5,0],[2,2,6,0]],[[5,6,6,6],[8,2,7],[6,5,7,2],[4,4,8,2]]).
p([[4,3,0,6],[0,5,2]],[[6,5,2,8],[2,7,4]]).
p([[6,3,0,5],[0,7,4,3]],[[8,5,2,7],[2,9,6,5]]).
p([[0,5,0,0],[4,1,5,6],[5,0,7,1],[1,4,1]],[[2,7,2,2],[6,3,7,8],[7,2,9,3],[3,6,3]]).
q([[5,7,4],[4,6,3,5],[2,0,3,0]],[[7,9,6],[4,6,3,5],[4,2,5,2]]).
q([[6,1,3,4],[6,6,2,6],[3,0,3,7],[7,5,7]],[[8,3,5,6],[8,8,4,8],[3,0,3,7],[9,7,9]]).
q([[4,7,0,5],[4,4,4,0],[2,3,7,4]],[[6,9,2,7],[4,4,4,0],[4,5,9,6]]).
q([[2,0,4],[6,6,0],[5,4,6,7],[3,1,2,7]],[[2,0,4],[8,8,2],[7,6,8,9],[3,1,2,7]]).
q([[7,6,0,4],[0,7,7]],[[7,6,0,4],[2,9,9]]).
