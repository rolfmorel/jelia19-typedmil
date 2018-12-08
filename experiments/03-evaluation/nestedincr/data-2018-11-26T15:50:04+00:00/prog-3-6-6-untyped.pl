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
my_min_list3(A,B):-min_list(A,B).
my_flatten4(A,B):-flatten(A,B).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).

my_last6(A,B):-last(A,B).
my_lowercase7(A):-downcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_sumlist2/2).
prim(my_min_list3/2).
prim(my_flatten4/2).
prim(my_last6/2).
prim(my_lowercase7/1).
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
p([[1,4,2],[2,0,5,5],[7,4,3,5],[2,1,2,7]],[[3,6,4],[4,2,7,7],[9,6,5,7],[4,3,4,9]]).
p([[7,5,4],[7,0,6,0],[1,5,0],[5,5,5,7]],[[9,7,6],[9,2,8,2],[3,7,2],[7,7,7,9]]).
p([[5,5,5],[1,5,2]],[[7,7,7],[3,7,4]]).
p([[6,5,1,5],[7,6,3,0]],[[8,7,3,7],[9,8,5,2]]).
p([[7,2,2],[0,6,7,7],[7,1,2]],[[9,4,4],[2,8,9,9],[9,3,4]]).
q([[4,5,0],[6,7,0,3],[0,1,5,5]],[[6,7,2],[8,9,2,5],[0,1,5,5]]).
q([[3,0,0,7],[3,1,7],[1,7,7]],[[5,2,2,9],[5,3,9],[1,7,7]]).
q([[6,1,5,1],[1,7,3,3]],[[8,3,7,3],[1,7,3,3]]).
q([[6,2,4,2],[1,7,7,5],[7,2,2],[6,4,5,0]],[[6,2,4,2],[3,9,9,7],[9,4,4],[8,6,7,2]]).
q([[6,0,7],[5,5,2,6],[7,2,7],[1,2,3]],[[6,0,7],[5,5,2,6],[9,4,9],[3,4,5]]).
