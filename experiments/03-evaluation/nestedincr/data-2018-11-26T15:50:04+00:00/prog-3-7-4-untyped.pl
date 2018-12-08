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

my_set3(A):-list_to_set(A,A).
my_list_to_set4(A,B):-list_to_set(A,B).
my_reverse5(A,B):-reverse(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_odd7(A):-1 is A mod 2.
my_toupper8(A,B):-upcase_atom(A,B),char_code(A,_).
prim(my_succ1/2).
prim(my_set3/1).
prim(my_list_to_set4/2).
prim(my_reverse5/2).
prim(my_pred6/2).
prim(my_odd7/1).
prim(my_toupper8/2).
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
p([[3,1,0,6],[3,0,0,4],[4,7,1],[4,5,1]],[[5,3,2,8],[5,2,2,6],[6,9,3],[6,7,3]]).
p([[3,5,4,0],[1,5,7],[4,7,4],[4,0,0]],[[5,7,6,2],[3,7,9],[6,9,6],[6,2,2]]).
p([[5,0,4],[2,7,0,0],[5,7,0,7]],[[7,2,6],[4,9,2,2],[7,9,2,9]]).
p([[5,6,4,4],[2,2,7]],[[7,8,6,6],[4,4,9]]).
p([[5,4,1,3],[7,3,1],[2,6,7,2]],[[7,6,3,5],[9,5,3],[4,8,9,4]]).
q([[6,7,4,4],[7,6,5],[3,5,2,3]],[[8,9,6,6],[9,8,7],[3,5,2,3]]).
q([[1,6,2,3],[1,4,7,4],[5,6,6]],[[3,8,4,5],[3,6,9,6],[5,6,6]]).
q([[6,5,2],[4,5,7]],[[6,5,2],[6,7,9]]).
q([[2,7,7,5],[5,1,4,0],[2,1,5,6]],[[2,7,7,5],[7,3,6,2],[4,3,7,8]]).
q([[6,0,4],[0,3,7],[2,4,0]],[[8,2,6],[2,5,9],[2,4,0]]).
