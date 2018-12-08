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
my_odd2(A):-1 is A mod 2.

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

my_double4(N,M):-M is 2*N,M =< 10.
my_lowercase5(A):-downcase_atom(A,A),char_code(A,_).
my_max_list6(A,B):-max_list(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_tail8([_|TL],TL).
my_set9(A):-list_to_set(A,A).
my_list_to_set10(A,B):-list_to_set(A,B).
my_len11(A,B):-length(A,B).
prim(my_succ1/2).
prim(my_odd2/1).
prim(my_double4/2).
prim(my_lowercase5/1).
prim(my_max_list6/2).
prim(my_sumlist7/2).
prim(my_tail8/2).
prim(my_set9/1).
prim(my_list_to_set10/2).
prim(my_len11/2).
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
p([[5,6,2],[7,6,3,6],[7,2,2],[3,4,1]],[[7,8,4],[9,8,5,8],[9,4,4],[5,6,3]]).
p([[6,6,3],[0,3,6],[2,5,0],[7,0,0,2]],[[8,8,5],[2,5,8],[4,7,2],[9,2,2,4]]).
p([[7,6,0],[1,7,1,5],[1,1,4,2],[5,7,2,7]],[[9,8,2],[3,9,3,7],[3,3,6,4],[7,9,4,9]]).
p([[6,3,5],[4,6,4],[0,0,6,3]],[[8,5,7],[6,8,6],[2,2,8,5]]).
p([[3,6,3,5],[7,4,6,0],[3,0,3,0],[1,4,6,7]],[[5,8,5,7],[9,6,8,2],[5,2,5,2],[3,6,8,9]]).
q([[1,5,4],[3,6,5,2],[4,2,1],[6,0,3,3]],[[3,7,6],[5,8,7,4],[6,4,3],[6,0,3,3]]).
q([[5,3,4,2],[0,1,4],[2,2,5]],[[7,5,6,4],[0,1,4],[4,4,7]]).
q([[0,1,2],[1,7,5,5],[5,0,3,2],[7,3,0]],[[0,1,2],[3,9,7,7],[5,0,3,2],[9,5,2]]).
q([[6,2,0,1],[1,1,1,2],[1,4,1,6],[2,4,3,0]],[[8,4,2,3],[3,3,3,4],[1,4,1,6],[2,4,3,0]]).
q([[0,7,6,0],[6,5,0,1],[1,7,2,6]],[[2,9,8,2],[6,5,0,1],[3,9,4,8]]).
