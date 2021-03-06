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
my_min_list2(A,B):-min_list(A,B).

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
my_uppercase5(A):-upcase_atom(A,A),char_code(A,_).
my_set6(A):-list_to_set(A,A).
my_tolower7(A,B):-downcase_atom(A,B),char_code(A,_).
my_pred8(A,B):-succ(B,A),A > 0.
prim(my_succ1/2).
prim(my_min_list2/2).
prim(my_double4/2).
prim(my_uppercase5/1).
prim(my_set6/1).
prim(my_tolower7/2).
prim(my_pred8/2).
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
p([[0,7,2,0],[5,3,7,0],[7,2,4],[3,1,6]],[[2,9,4,2],[7,5,9,2],[9,4,6],[5,3,8]]).
p([[1,4,1],[4,7,2],[3,5,6,3]],[[3,6,3],[6,9,4],[5,7,8,5]]).
p([[5,4,3],[1,1,5,3],[2,1,4,5],[3,0,4,5]],[[7,6,5],[3,3,7,5],[4,3,6,7],[5,2,6,7]]).
p([[2,4,3,0],[2,0,1]],[[4,6,5,2],[4,2,3]]).
p([[2,6,4,4],[2,4,5]],[[4,8,6,6],[4,6,7]]).
q([[2,0,7],[0,4,0,2],[2,1,6],[7,0,2,5]],[[4,2,9],[0,4,0,2],[4,3,8],[9,2,4,7]]).
q([[7,3,3,7],[5,7,6,4]],[[9,5,5,9],[5,7,6,4]]).
q([[5,7,4,3],[0,6,2],[4,5,6]],[[5,7,4,3],[2,8,4],[6,7,8]]).
q([[4,4,2],[7,7,3]],[[4,4,2],[9,9,5]]).
q([[3,2,1],[7,0,1]],[[5,4,3],[7,0,1]]).
