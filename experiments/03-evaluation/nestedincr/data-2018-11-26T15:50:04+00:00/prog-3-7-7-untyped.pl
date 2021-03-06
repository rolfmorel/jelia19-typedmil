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

my_element3(A,B):-member(B,A).
my_msort4(A,B):-msort(A,B).
my_len5(A,B):-length(A,B).
my_min_list6(A,B):-min_list(A,B).
my_flatten7(A,B):-flatten(A,B).
my_sumlist8(A,B):-sumlist(A,B).
prim(my_succ1/2).
prim(my_element3/2).
prim(my_msort4/2).
prim(my_len5/2).
prim(my_min_list6/2).
prim(my_flatten7/2).
prim(my_sumlist8/2).
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
p([[6,6,5,0],[1,7,3,4],[4,0,1,4]],[[8,8,7,2],[3,9,5,6],[6,2,3,6]]).
p([[6,4,2,5],[3,3,6],[7,4,0]],[[8,6,4,7],[5,5,8],[9,6,2]]).
p([[7,3,4,2],[3,4,1],[2,1,7],[0,3,0]],[[9,5,6,4],[5,6,3],[4,3,9],[2,5,2]]).
p([[5,6,1,3],[7,7,3]],[[7,8,3,5],[9,9,5]]).
p([[6,2,6,1],[1,3,0]],[[8,4,8,3],[3,5,2]]).
q([[3,0,7,1],[2,5,2,6],[1,7,1]],[[5,2,9,3],[4,7,4,8],[1,7,1]]).
q([[3,1,1],[4,3,3]],[[3,1,1],[6,5,5]]).
q([[5,0,1,5],[6,6,0],[1,6,6,1]],[[5,0,1,5],[8,8,2],[3,8,8,3]]).
q([[0,7,5],[1,4,4,3],[6,4,1,5]],[[0,7,5],[3,6,6,5],[8,6,3,7]]).
q([[2,1,0],[4,7,3],[6,7,6],[0,2,7,5]],[[4,3,2],[4,7,3],[8,9,8],[0,2,7,5]]).
