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
my_sumlist3(A,B):-sumlist(A,B).
my_reverse4(A,B):-reverse(A,B).
my_set5(A):-list_to_set(A,A).
my_max_list6(A,B):-max_list(A,B).
my_last7(A,B):-last(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).

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
prim(my_len2/2).
prim(my_sumlist3/2).
prim(my_reverse4/2).
prim(my_set5/1).
prim(my_max_list6/2).
prim(my_last7/2).
prim(my_list_to_set8/2).
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
p([[1,7,1,2],[7,0,7,3],[3,2,2],[0,3,2]],[[3,9,3,4],[9,2,9,5],[5,4,4],[2,5,4]]).
p([[3,7,4],[3,0,0,0],[4,2,2,3],[7,3,7]],[[5,9,6],[5,2,2,2],[6,4,4,5],[9,5,9]]).
p([[0,0,0],[0,1,6,5],[3,4,5],[6,5,4]],[[2,2,2],[2,3,8,7],[5,6,7],[8,7,6]]).
p([[0,4,4],[4,2,0,0],[1,1,6]],[[2,6,6],[6,4,2,2],[3,3,8]]).
p([[6,5,5],[0,0,1,2]],[[8,7,7],[2,2,3,4]]).
q([[5,5,1,7],[7,7,5,0],[4,6,1],[3,6,1]],[[7,7,3,9],[7,7,5,0],[6,8,3],[5,8,3]]).
q([[0,0,0,3],[6,1,4]],[[0,0,0,3],[8,3,6]]).
q([[7,3,5],[3,7,0,3]],[[9,5,7],[3,7,0,3]]).
q([[1,0,0,4],[4,7,5,5]],[[1,0,0,4],[6,9,7,7]]).
q([[2,6,6],[1,4,1,4]],[[4,8,8],[1,4,1,4]]).
