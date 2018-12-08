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
my_head3([H|_],H).
my_max_list4(A,B):-max_list(A,B).
my_last5(A,B):-last(A,B).
my_flatten6(A,B):-flatten(A,B).
my_even7(A):-0 is A mod 2.

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

my_toupper9(A,B):-upcase_atom(A,B),char_code(A,_).
my_set10(A):-list_to_set(A,A).
prim(my_succ1/2).
prim(my_odd2/1).
prim(my_head3/2).
prim(my_max_list4/2).
prim(my_last5/2).
prim(my_flatten6/2).
prim(my_even7/1).
prim(my_toupper9/2).
prim(my_set10/1).
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
p([[5,3,5],[1,7,5]],[[7,5,7],[3,9,7]]).
p([[5,2,0,5],[0,4,4]],[[7,4,2,7],[2,6,6]]).
p([[5,1,5],[2,3,5]],[[7,3,7],[4,5,7]]).
p([[2,6,7],[3,1,1]],[[4,8,9],[5,3,3]]).
p([[4,5,1,7],[1,2,1,3],[5,4,0,6]],[[6,7,3,9],[3,4,3,5],[7,6,2,8]]).
q([[4,4,4],[5,4,5]],[[4,4,4],[7,6,7]]).
q([[3,6,1,2],[7,1,2],[0,4,0]],[[5,8,3,4],[9,3,4],[0,4,0]]).
q([[0,3,4],[5,2,0,7],[7,0,1,1]],[[2,5,6],[5,2,0,7],[9,2,3,3]]).
q([[6,4,3],[0,7,1,2],[1,7,7,3],[2,7,3,6]],[[6,4,3],[2,9,3,4],[1,7,7,3],[4,9,5,8]]).
q([[7,2,5,3],[3,7,0,0],[6,7,2],[2,2,0,0]],[[9,4,7,5],[3,7,0,0],[6,7,2],[4,4,2,2]]).
