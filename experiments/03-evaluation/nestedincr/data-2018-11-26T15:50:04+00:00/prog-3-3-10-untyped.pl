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
my_element2(A,B):-member(B,A).
my_reverse3(A,B):-reverse(A,B).

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
prim(my_element2/2).
prim(my_reverse3/2).
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
p([[7,0,5],[0,4,2],[1,7,6,7],[1,4,1]],[[9,2,7],[2,6,4],[3,9,8,9],[3,6,3]]).
p([[3,6,1],[7,0,6,5],[2,7,4],[3,3,4]],[[5,8,3],[9,2,8,7],[4,9,6],[5,5,6]]).
p([[2,1,5,5],[2,2,2,0],[0,6,3,0],[0,4,1]],[[4,3,7,7],[4,4,4,2],[2,8,5,2],[2,6,3]]).
p([[7,0,7,4],[4,0,3],[7,2,4]],[[9,2,9,6],[6,2,5],[9,4,6]]).
p([[0,6,6],[4,0,3,1]],[[2,8,8],[6,2,5,3]]).
q([[3,0,2,6],[0,1,6]],[[3,0,2,6],[2,3,8]]).
q([[4,1,2,0],[7,4,7],[5,6,1]],[[6,3,4,2],[7,4,7],[7,8,3]]).
q([[1,6,0,1],[3,7,2,1],[5,0,5,4]],[[3,8,2,3],[5,9,4,3],[5,0,5,4]]).
q([[1,6,0],[1,7,2,2],[0,2,7],[5,7,5,2]],[[3,8,2],[3,9,4,4],[0,2,7],[7,9,7,4]]).
q([[1,0,7],[5,7,5],[7,6,4,0]],[[3,2,9],[7,9,7],[7,6,4,0]]).