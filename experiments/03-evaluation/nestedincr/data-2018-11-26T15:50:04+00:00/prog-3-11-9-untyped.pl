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
my_element3(A,B):-member(B,A).
my_pred4(A,B):-succ(B,A),A > 0.
my_head5([H|_],H).
my_msort6(A,B):-msort(A,B).
my_flatten7(A,B):-flatten(A,B).
my_reverse8(A,B):-reverse(A,B).
my_max_list9(A,B):-max_list(A,B).

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

my_odd11(A):-1 is A mod 2.
my_last12(A,B):-last(A,B).
prim(my_succ1/2).
prim(my_even2/1).
prim(my_element3/2).
prim(my_pred4/2).
prim(my_head5/2).
prim(my_msort6/2).
prim(my_flatten7/2).
prim(my_reverse8/2).
prim(my_max_list9/2).
prim(my_odd11/1).
prim(my_last12/2).
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
p([[3,3,5,3],[2,4,1],[5,1,5,6],[6,3,4]],[[5,5,7,5],[4,6,3],[7,3,7,8],[8,5,6]]).
p([[3,0,4,1],[3,5,0,0],[1,1,3]],[[5,2,6,3],[5,7,2,2],[3,3,5]]).
p([[2,3,2,1],[7,1,5,3]],[[4,5,4,3],[9,3,7,5]]).
p([[2,3,6,1],[2,5,5],[1,3,4],[0,0,6]],[[4,5,8,3],[4,7,7],[3,5,6],[2,2,8]]).
p([[6,4,1],[0,3,0]],[[8,6,3],[2,5,2]]).
q([[1,5,5,3],[7,1,2],[7,7,5,4],[7,6,4]],[[3,7,7,5],[9,3,4],[7,7,5,4],[7,6,4]]).
q([[0,0,5],[6,5,6,1]],[[2,2,7],[6,5,6,1]]).
q([[4,7,0],[1,7,4,7]],[[4,7,0],[3,9,6,9]]).
q([[7,3,4],[5,5,3]],[[7,3,4],[7,7,5]]).
q([[4,1,2],[3,2,7,2]],[[4,1,2],[5,4,9,4]]).
