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
my_uppercase3(A):-upcase_atom(A,A),char_code(A,_).
my_list_to_set4(A,B):-list_to_set(A,B).
my_head5([H|_],H).
my_len6(A,B):-length(A,B).
my_tolower7(A,B):-downcase_atom(A,B),char_code(A,_).
prim(my_succ1/2).
prim(my_lowercase2/1).
prim(my_uppercase3/1).
prim(my_list_to_set4/2).
prim(my_head5/2).
prim(my_len6/2).
prim(my_tolower7/2).
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
p([[5,2,0,2],[6,3,2,6]],[[7,4,2,4],[8,5,4,8]]).
p([[0,7,6],[1,1,1,7]],[[2,9,8],[3,3,3,9]]).
p([[0,2,3],[2,6,0],[3,0,3]],[[2,4,5],[4,8,2],[5,2,5]]).
p([[0,3,1,4],[0,5,3,7],[1,2,7]],[[2,5,3,6],[2,7,5,9],[3,4,9]]).
p([[4,5,2,0],[1,2,3]],[[6,7,4,2],[3,4,5]]).
q([[1,4,1],[4,5,1],[5,0,2,6],[6,2,2,2]],[[3,6,3],[6,7,3],[5,0,2,6],[6,2,2,2]]).
q([[7,0,3],[3,0,5,7],[4,4,1,3],[4,2,4]],[[7,0,3],[5,2,7,9],[6,6,3,5],[4,2,4]]).
q([[4,0,4,3],[0,1,7]],[[4,0,4,3],[2,3,9]]).
q([[1,0,4],[4,5,0,3],[1,6,6],[1,6,0,2]],[[3,2,6],[6,7,2,5],[1,6,6],[3,8,2,4]]).
q([[1,5,2,5],[7,4,5,6]],[[3,7,4,7],[7,4,5,6]]).
