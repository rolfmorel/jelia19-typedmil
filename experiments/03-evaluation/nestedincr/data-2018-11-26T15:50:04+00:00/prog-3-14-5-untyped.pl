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
my_set2(A):-list_to_set(A,A).
my_len3(A,B):-length(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_pred6(A,B):-succ(B,A),A > 0.
my_uppercase7(A):-upcase_atom(A,A),char_code(A,_).
my_element8(A,B):-member(B,A).
my_head9([H|_],H).
my_min_list10(A,B):-min_list(A,B).
my_odd11(A):-1 is A mod 2.

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

my_tail13([_|TL],TL).
my_msort14(A,B):-msort(A,B).
my_even15(A):-0 is A mod 2.
prim(my_succ1/2).
prim(my_set2/1).
prim(my_len3/2).
prim(my_sumlist4/2).
prim(my_double5/2).
prim(my_pred6/2).
prim(my_uppercase7/1).
prim(my_element8/2).
prim(my_head9/2).
prim(my_min_list10/2).
prim(my_odd11/1).
prim(my_tail13/2).
prim(my_msort14/2).
prim(my_even15/1).
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
p([[7,1,0],[7,7,5,6]],[[9,3,2],[9,9,7,8]]).
p([[2,5,5,5],[4,2,0,3]],[[4,7,7,7],[6,4,2,5]]).
p([[4,0,0],[1,6,1],[3,1,7,1],[2,7,3]],[[6,2,2],[3,8,3],[5,3,9,3],[4,9,5]]).
p([[0,2,0],[0,1,3],[7,4,6,3]],[[2,4,2],[2,3,5],[9,6,8,5]]).
p([[3,2,5,1],[7,3,0],[0,7,4,1],[1,6,7,3]],[[5,4,7,3],[9,5,2],[2,9,6,3],[3,8,9,5]]).
q([[1,2,0],[4,2,5,2]],[[1,2,0],[6,4,7,4]]).
q([[5,4,5],[3,4,2],[5,3,4]],[[7,6,7],[5,6,4],[5,3,4]]).
q([[3,3,5,1],[2,4,5],[4,2,3]],[[3,3,5,1],[4,6,7],[6,4,5]]).
q([[1,2,1],[2,4,5]],[[3,4,3],[2,4,5]]).
q([[7,4,4,4],[7,6,7],[1,0,0]],[[9,6,6,6],[9,8,9],[1,0,0]]).
