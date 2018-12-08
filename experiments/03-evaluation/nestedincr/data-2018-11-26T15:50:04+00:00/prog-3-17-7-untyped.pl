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
my_msort2(A,B):-msort(A,B).
my_set3(A):-list_to_set(A,A).
my_max_list4(A,B):-max_list(A,B).
my_len5(A,B):-length(A,B).
my_toupper6(A,B):-upcase_atom(A,B),char_code(A,_).
my_tail7([_|TL],TL).
my_list_to_set8(A,B):-list_to_set(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_sumlist10(A,B):-sumlist(A,B).
my_flatten11(A,B):-flatten(A,B).
my_head12([H|_],H).
my_even13(A):-0 is A mod 2.
my_lowercase14(A):-downcase_atom(A,A),char_code(A,_).
my_uppercase15(A):-upcase_atom(A,A),char_code(A,_).
my_element16(A,B):-member(B,A).

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

my_double18(N,M):-M is 2*N,M =< 10.
prim(my_succ1/2).
prim(my_msort2/2).
prim(my_set3/1).
prim(my_max_list4/2).
prim(my_len5/2).
prim(my_toupper6/2).
prim(my_tail7/2).
prim(my_list_to_set8/2).
prim(my_pred9/2).
prim(my_sumlist10/2).
prim(my_flatten11/2).
prim(my_head12/2).
prim(my_even13/1).
prim(my_lowercase14/1).
prim(my_uppercase15/1).
prim(my_element16/2).
prim(my_double18/2).
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
p([[6,3,1,5],[0,4,4],[3,6,3,2],[4,7,1]],[[8,5,3,7],[2,6,6],[5,8,5,4],[6,9,3]]).
p([[6,7,2,5],[0,2,0],[2,6,5],[5,5,1]],[[8,9,4,7],[2,4,2],[4,8,7],[7,7,3]]).
p([[5,5,4,5],[3,1,1]],[[7,7,6,7],[5,3,3]]).
p([[0,7,6],[0,3,7,7],[0,0,3,7]],[[2,9,8],[2,5,9,9],[2,2,5,9]]).
p([[0,0,2,4],[6,3,7],[0,3,7],[1,4,4]],[[2,2,4,6],[8,5,9],[2,5,9],[3,6,6]]).
q([[2,6,4],[3,6,6],[0,4,5,0]],[[4,8,6],[3,6,6],[2,6,7,2]]).
q([[0,6,4,7],[2,5,3,6],[1,3,7],[5,0,6,0]],[[2,8,6,9],[2,5,3,6],[3,5,9],[5,0,6,0]]).
q([[0,4,2,0],[0,5,2]],[[0,4,2,0],[2,7,4]]).
q([[6,3,1],[0,3,7]],[[6,3,1],[2,5,9]]).
q([[1,6,6,0],[7,3,4,7],[3,3,5,1],[3,3,6]],[[1,6,6,0],[9,5,6,9],[3,3,5,1],[5,5,8]]).
