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
my_tolower2(A,B):-downcase_atom(A,B),char_code(A,_).
my_head3([H|_],H).
my_last4(A,B):-last(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_flatten6(A,B):-flatten(A,B).
my_odd7(A):-1 is A mod 2.
my_tail8([_|TL],TL).
my_element9(A,B):-member(B,A).
my_sumlist10(A,B):-sumlist(A,B).
my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).

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

my_even13(A):-0 is A mod 2.
my_max_list14(A,B):-max_list(A,B).
my_len15(A,B):-length(A,B).
my_set16(A):-list_to_set(A,A).
my_list_to_set17(A,B):-list_to_set(A,B).
my_msort18(A,B):-msort(A,B).
my_min_list19(A,B):-min_list(A,B).
prim(my_succ1/2).
prim(my_tolower2/2).
prim(my_head3/2).
prim(my_last4/2).
prim(my_double5/2).
prim(my_flatten6/2).
prim(my_odd7/1).
prim(my_tail8/2).
prim(my_element9/2).
prim(my_sumlist10/2).
prim(my_uppercase11/1).
prim(my_even13/1).
prim(my_max_list14/2).
prim(my_len15/2).
prim(my_set16/1).
prim(my_list_to_set17/2).
prim(my_msort18/2).
prim(my_min_list19/2).
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
p([[3,3,0],[3,2,0,4]],[[5,5,2],[5,4,2,6]]).
p([[1,7,7,6],[5,2,5],[0,0,6,0],[6,3,1]],[[3,9,9,8],[7,4,7],[2,2,8,2],[8,5,3]]).
p([[1,6,6],[0,2,1]],[[3,8,8],[2,4,3]]).
p([[7,0,1,5],[7,3,4,4],[4,6,4,5]],[[9,2,3,7],[9,5,6,6],[6,8,6,7]]).
p([[1,3,3],[4,6,0]],[[3,5,5],[6,8,2]]).
q([[4,1,0,3],[3,3,4]],[[6,3,2,5],[3,3,4]]).
q([[7,4,0,1],[1,7,2,7]],[[9,6,2,3],[1,7,2,7]]).
q([[2,5,1],[6,5,2,3],[3,4,0,4]],[[2,5,1],[8,7,4,5],[5,6,2,6]]).
q([[3,5,0],[4,3,5,0],[0,2,1],[4,6,4,2]],[[3,5,0],[4,3,5,0],[2,4,3],[6,8,6,4]]).
q([[0,7,4,7],[4,3,4,2]],[[0,7,4,7],[6,5,6,4]]).
