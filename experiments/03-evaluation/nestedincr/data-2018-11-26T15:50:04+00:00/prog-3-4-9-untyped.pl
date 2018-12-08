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
my_tail3([_|TL],TL).

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

my_even5(A):-0 is A mod 2.
prim(my_succ1/2).
prim(my_len2/2).
prim(my_tail3/2).
prim(my_even5/1).
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
p([[7,7,7],[4,6,5,2],[1,7,3]],[[9,9,9],[6,8,7,4],[3,9,5]]).
p([[3,4,7,5],[3,4,4,0]],[[5,6,9,7],[5,6,6,2]]).
p([[5,1,5],[7,2,1,6],[6,1,3],[3,6,2]],[[7,3,7],[9,4,3,8],[8,3,5],[5,8,4]]).
p([[1,2,2,1],[2,0,4,7]],[[3,4,4,3],[4,2,6,9]]).
p([[1,0,3,1],[2,4,5,7],[6,3,0,2],[4,4,4,1]],[[3,2,5,3],[4,6,7,9],[8,5,2,4],[6,6,6,3]]).
q([[0,4,6,4],[1,3,1,2]],[[0,4,6,4],[3,5,3,4]]).
q([[3,2,1],[1,6,3,4],[6,0,1,0]],[[5,4,3],[1,6,3,4],[8,2,3,2]]).
q([[3,0,3],[4,6,0,3],[7,5,0,3]],[[5,2,5],[4,6,0,3],[9,7,2,5]]).
q([[7,7,5,2],[0,2,5,7],[7,2,4,1]],[[9,9,7,4],[2,4,7,9],[7,2,4,1]]).
q([[2,6,5,5],[2,5,5,6],[4,7,2,1]],[[2,6,5,5],[4,7,7,8],[6,9,4,3]]).
