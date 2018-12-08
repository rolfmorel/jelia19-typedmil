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
my_tolower3(A,B):-downcase_atom(A,B),char_code(A,_).
my_min_list4(A,B):-min_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_lowercase6(A):-downcase_atom(A,A),char_code(A,_).
my_reverse7(A,B):-reverse(A,B).
my_toupper8(A,B):-upcase_atom(A,B),char_code(A,_).
my_max_list9(A,B):-max_list(A,B).
prim(my_succ1/2).
prim(my_tail2/2).
prim(my_tolower3/2).
prim(my_min_list4/2).
prim(my_sumlist5/2).
prim(my_lowercase6/1).
prim(my_reverse7/2).
prim(my_toupper8/2).
prim(my_max_list9/2).
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
p([[7,2,0,6],[2,3,1],[3,4,1,6],[2,7,0]],[[9,4,2,8],[4,5,3],[5,6,3,8],[4,9,2]]).
p([[0,1,5],[1,3,0,6]],[[2,3,7],[3,5,2,8]]).
p([[1,4,3],[4,6,0]],[[3,6,5],[6,8,2]]).
p([[0,3,7],[4,3,3,5]],[[2,5,9],[6,5,5,7]]).
p([[0,5,5,5],[1,2,2],[5,4,1]],[[2,7,7,7],[3,4,4],[7,6,3]]).
q([[1,3,4],[6,0,5],[5,6,2,3],[0,1,5,5]],[[1,3,4],[6,0,5],[7,8,4,5],[2,3,7,7]]).
q([[6,5,5,3],[7,0,7,3],[1,7,7,3]],[[8,7,7,5],[7,0,7,3],[3,9,9,5]]).
q([[0,2,6],[6,2,3,4]],[[0,2,6],[8,4,5,6]]).
q([[3,0,2],[1,1,6],[1,4,6],[0,5,3,6]],[[3,0,2],[1,1,6],[3,6,8],[2,7,5,8]]).
q([[1,5,5],[3,5,0,1]],[[3,7,7],[3,5,0,1]]).
