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
my_len2(A,B):-length(A,B).
my_set3(A):-list_to_set(A,A).
my_reverse4(A,B):-reverse(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_tolower6(A,B):-downcase_atom(A,B),char_code(A,_).
my_flatten7(A,B):-flatten(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_min_list9(A,B):-min_list(A,B).
my_tail10([_|TL],TL).
my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).
my_max_list12(A,B):-max_list(A,B).

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

my_list_to_set14(A,B):-list_to_set(A,B).
my_element15(A,B):-member(B,A).
my_sumlist16(A,B):-sumlist(A,B).
my_toupper17(A,B):-upcase_atom(A,B),char_code(A,_).
prim(my_succ1/2).
prim(my_len2/2).
prim(my_set3/1).
prim(my_reverse4/2).
prim(my_double5/2).
prim(my_tolower6/2).
prim(my_flatten7/2).
prim(my_pred8/2).
prim(my_min_list9/2).
prim(my_tail10/2).
prim(my_uppercase11/1).
prim(my_max_list12/2).
prim(my_list_to_set14/2).
prim(my_element15/2).
prim(my_sumlist16/2).
prim(my_toupper17/2).
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
p([[1,0,3],[1,0,7,1]],[[3,2,5],[3,2,9,3]]).
p([[3,2,7],[5,0,2,7]],[[5,4,9],[7,2,4,9]]).
p([[6,6,7],[0,4,6,1]],[[8,8,9],[2,6,8,3]]).
p([[6,3,2,3],[3,0,7]],[[8,5,4,5],[5,2,9]]).
p([[2,0,2],[2,1,4,7],[0,7,2],[7,1,1,3]],[[4,2,4],[4,3,6,9],[2,9,4],[9,3,3,5]]).
q([[7,2,5,2],[7,2,0],[2,1,2,4]],[[9,4,7,4],[7,2,0],[4,3,4,6]]).
q([[0,0,1,2],[2,6,0],[1,2,7,1]],[[2,2,3,4],[4,8,2],[1,2,7,1]]).
q([[0,5,5,4],[4,2,5],[5,0,2,1]],[[2,7,7,6],[6,4,7],[5,0,2,1]]).
q([[4,0,0,4],[0,1,6,4],[3,7,1,5]],[[6,2,2,6],[2,3,8,6],[3,7,1,5]]).
q([[1,1,0],[2,6,2,0]],[[3,3,2],[2,6,2,0]]).
