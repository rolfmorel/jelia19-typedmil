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
my_list_to_set2(A,B):-list_to_set(A,B).
my_sumlist3(A,B):-sumlist(A,B).

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

prim(my_succ1/2).
prim(my_list_to_set2/2).
prim(my_sumlist3/2).
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
p([[1,5,3],[1,6,1]],[[3,7,5],[3,8,3]]).
p([[7,4,3,4],[2,2,0,6],[4,1,6,3]],[[9,6,5,6],[4,4,2,8],[6,3,8,5]]).
p([[0,5,7,7],[2,2,2,7],[5,5,7]],[[2,7,9,9],[4,4,4,9],[7,7,9]]).
p([[7,3,6],[2,1,1],[1,3,3,1]],[[9,5,8],[4,3,3],[3,5,5,3]]).
p([[2,4,2],[7,1,4,4]],[[4,6,4],[9,3,6,6]]).
q([[7,7,6],[6,4,6],[3,4,0,2],[1,6,2]],[[9,9,8],[6,4,6],[5,6,2,4],[3,8,4]]).
q([[7,2,1],[1,2,3]],[[7,2,1],[3,4,5]]).
q([[0,6,1,0],[3,7,2],[0,6,2,0],[2,2,3]],[[2,8,3,2],[5,9,4],[0,6,2,0],[4,4,5]]).
q([[6,1,0,3],[0,1,0],[1,6,1,4],[3,1,5,6]],[[8,3,2,5],[2,3,2],[1,6,1,4],[3,1,5,6]]).
q([[5,0,3,7],[4,3,4,6],[4,1,1,7],[3,6,6,7]],[[7,2,5,9],[6,5,6,8],[6,3,3,9],[3,6,6,7]]).
