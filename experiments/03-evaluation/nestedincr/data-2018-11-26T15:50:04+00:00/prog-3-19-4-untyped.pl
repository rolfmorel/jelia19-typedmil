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
my_flatten2(A,B):-flatten(A,B).
my_tolower3(A,B):-downcase_atom(A,B),char_code(A,_).
my_double4(N,M):-M is 2*N,M =< 10.
my_even5(A):-0 is A mod 2.
my_set6(A):-list_to_set(A,A).
my_lowercase7(A):-downcase_atom(A,A),char_code(A,_).
my_sumlist8(A,B):-sumlist(A,B).
my_last9(A,B):-last(A,B).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_head11([H|_],H).

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
my_uppercase14(A):-upcase_atom(A,A),char_code(A,_).
my_max_list15(A,B):-max_list(A,B).
my_odd16(A):-1 is A mod 2.
my_reverse17(A,B):-reverse(A,B).
my_list_to_set18(A,B):-list_to_set(A,B).
my_element19(A,B):-member(B,A).
my_pred20(A,B):-succ(B,A),A > 0.
prim(my_succ1/2).
prim(my_flatten2/2).
prim(my_tolower3/2).
prim(my_double4/2).
prim(my_even5/1).
prim(my_set6/1).
prim(my_lowercase7/1).
prim(my_sumlist8/2).
prim(my_last9/2).
prim(my_toupper10/2).
prim(my_head11/2).
prim(my_tail13/2).
prim(my_uppercase14/1).
prim(my_max_list15/2).
prim(my_odd16/1).
prim(my_reverse17/2).
prim(my_list_to_set18/2).
prim(my_element19/2).
prim(my_pred20/2).
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
p([[2,2,2,4],[0,5,6,5],[7,6,1]],[[4,4,4,6],[2,7,8,7],[9,8,3]]).
p([[2,3,3,2],[2,7,5,3]],[[4,5,5,4],[4,9,7,5]]).
p([[2,7,1,2],[0,6,5,7]],[[4,9,3,4],[2,8,7,9]]).
p([[4,4,7],[7,2,6],[2,4,4],[2,4,2]],[[6,6,9],[9,4,8],[4,6,6],[4,6,4]]).
p([[3,1,3],[3,2,2,7]],[[5,3,5],[5,4,4,9]]).
q([[7,2,6],[2,2,5]],[[7,2,6],[4,4,7]]).
q([[1,1,1,1],[6,7,6,0]],[[3,3,3,3],[6,7,6,0]]).
q([[4,2,3,0],[0,6,1,5],[1,5,1],[2,2,2]],[[6,4,5,2],[2,8,3,7],[1,5,1],[2,2,2]]).
q([[6,2,7,0],[1,4,0]],[[6,2,7,0],[3,6,2]]).
q([[1,6,5,0],[1,5,2,5]],[[1,6,5,0],[3,7,4,7]]).
