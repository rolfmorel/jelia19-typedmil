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
my_even2(A):-0 is A mod 2.

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

my_last4(A,B):-last(A,B).
my_uppercase5(A):-upcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_even2/1).
prim(my_last4/2).
prim(my_uppercase5/1).
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
p([[2,7,6],[2,3,4,6]],[[4,9,8],[4,5,6,8]]).
p([[4,0,1,4],[5,0,0],[5,5,1],[6,6,7]],[[6,2,3,6],[7,2,2],[7,7,3],[8,8,9]]).
p([[0,6,4],[1,2,6],[3,6,2,6]],[[2,8,6],[3,4,8],[5,8,4,8]]).
p([[0,7,6],[0,4,0],[1,2,5],[1,1,4,6]],[[2,9,8],[2,6,2],[3,4,7],[3,3,6,8]]).
p([[3,4,2,1],[7,4,4],[0,5,7,5]],[[5,6,4,3],[9,6,6],[2,7,9,7]]).
q([[6,5,2,0],[3,3,7,3]],[[6,5,2,0],[5,5,9,5]]).
q([[3,0,6,1],[6,4,6],[3,2,5]],[[3,0,6,1],[8,6,8],[5,4,7]]).
q([[1,6,2,2],[0,7,4,5],[7,7,1,6],[1,2,6,1]],[[3,8,4,4],[2,9,6,7],[7,7,1,6],[1,2,6,1]]).
q([[7,2,3],[0,7,7]],[[7,2,3],[2,9,9]]).
q([[3,4,4,3],[3,3,6],[4,6,6,2]],[[3,4,4,3],[5,5,8],[6,8,8,4]]).
