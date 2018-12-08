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
my_pred2(A,B):-succ(B,A),A > 0.
my_set3(A):-list_to_set(A,A).
my_msort4(A,B):-msort(A,B).
my_tail5([_|TL],TL).
my_list_to_set6(A,B):-list_to_set(A,B).
my_uppercase7(A):-upcase_atom(A,A),char_code(A,_).

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

my_sumlist9(A,B):-sumlist(A,B).
my_len10(A,B):-length(A,B).
my_double11(N,M):-M is 2*N,M =< 10.
my_max_list12(A,B):-max_list(A,B).
my_reverse13(A,B):-reverse(A,B).
my_toupper14(A,B):-upcase_atom(A,B),char_code(A,_).
my_lowercase15(A):-downcase_atom(A,A),char_code(A,_).
my_element16(A,B):-member(B,A).
my_odd17(A):-1 is A mod 2.
my_even18(A):-0 is A mod 2.
my_last19(A,B):-last(A,B).
my_min_list20(A,B):-min_list(A,B).
my_tolower21(A,B):-downcase_atom(A,B),char_code(A,_).
my_flatten22(A,B):-flatten(A,B).
prim(my_succ1/2).
prim(my_pred2/2).
prim(my_set3/1).
prim(my_msort4/2).
prim(my_tail5/2).
prim(my_list_to_set6/2).
prim(my_uppercase7/1).
prim(my_sumlist9/2).
prim(my_len10/2).
prim(my_double11/2).
prim(my_max_list12/2).
prim(my_reverse13/2).
prim(my_toupper14/2).
prim(my_lowercase15/1).
prim(my_element16/2).
prim(my_odd17/1).
prim(my_even18/1).
prim(my_last19/2).
prim(my_min_list20/2).
prim(my_tolower21/2).
prim(my_flatten22/2).
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
p([[7,1,2],[5,4,3],[5,7,6]],[[9,3,4],[7,6,5],[7,9,8]]).
p([[2,4,7,3],[4,3,6,1]],[[4,6,9,5],[6,5,8,3]]).
p([[0,4,3,5],[3,2,7]],[[2,6,5,7],[5,4,9]]).
p([[6,2,2],[6,6,6,6],[3,2,1,0],[0,7,6,3]],[[8,4,4],[8,8,8,8],[5,4,3,2],[2,9,8,5]]).
p([[7,5,6],[2,0,1,3],[1,2,6,4]],[[9,7,8],[4,2,3,5],[3,4,8,6]]).
q([[6,0,2,0],[5,2,6],[2,6,3],[0,0,4,4]],[[6,0,2,0],[7,4,8],[4,8,5],[2,2,6,6]]).
q([[6,2,2],[1,3,6]],[[6,2,2],[3,5,8]]).
q([[0,7,6,4],[1,7,3],[2,5,0]],[[2,9,8,6],[3,9,5],[2,5,0]]).
q([[4,0,2,4],[1,3,6,5],[6,7,6,6],[3,2,6,5]],[[6,2,4,6],[1,3,6,5],[8,9,8,8],[5,4,8,7]]).
q([[0,3,1,7],[6,3,5]],[[0,3,1,7],[8,5,7]]).
