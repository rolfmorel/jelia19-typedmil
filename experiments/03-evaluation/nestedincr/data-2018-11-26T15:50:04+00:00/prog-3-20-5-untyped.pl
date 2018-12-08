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
my_tail2([_|TL],TL).
my_tolower3(A,B):-downcase_atom(A,B),char_code(A,_).
my_pred4(A,B):-succ(B,A),A > 0.
my_even5(A):-0 is A mod 2.
my_element6(A,B):-member(B,A).
my_msort7(A,B):-msort(A,B).
my_last8(A,B):-last(A,B).
my_uppercase9(A):-upcase_atom(A,A),char_code(A,_).
my_len10(A,B):-length(A,B).
my_toupper11(A,B):-upcase_atom(A,B),char_code(A,_).
my_flatten12(A,B):-flatten(A,B).
my_reverse13(A,B):-reverse(A,B).
my_max_list14(A,B):-max_list(A,B).

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

my_min_list16(A,B):-min_list(A,B).
my_set17(A):-list_to_set(A,A).
my_head18([H|_],H).
my_double19(N,M):-M is 2*N,M =< 10.
my_lowercase20(A):-downcase_atom(A,A),char_code(A,_).
my_list_to_set21(A,B):-list_to_set(A,B).
prim(my_succ1/2).
prim(my_tail2/2).
prim(my_tolower3/2).
prim(my_pred4/2).
prim(my_even5/1).
prim(my_element6/2).
prim(my_msort7/2).
prim(my_last8/2).
prim(my_uppercase9/1).
prim(my_len10/2).
prim(my_toupper11/2).
prim(my_flatten12/2).
prim(my_reverse13/2).
prim(my_max_list14/2).
prim(my_min_list16/2).
prim(my_set17/1).
prim(my_head18/2).
prim(my_double19/2).
prim(my_lowercase20/1).
prim(my_list_to_set21/2).
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
p([[6,2,0],[4,4,6]],[[8,4,2],[6,6,8]]).
p([[3,4,5],[5,2,4],[4,7,3,0],[1,1,3]],[[5,6,7],[7,4,6],[6,9,5,2],[3,3,5]]).
p([[2,2,2],[6,2,7,7],[5,1,2],[4,2,6]],[[4,4,4],[8,4,9,9],[7,3,4],[6,4,8]]).
p([[4,1,1,5],[2,0,6],[4,2,4],[6,0,3,7]],[[6,3,3,7],[4,2,8],[6,4,6],[8,2,5,9]]).
p([[7,5,4,4],[6,5,5,6]],[[9,7,6,6],[8,7,7,8]]).
q([[2,6,4],[1,4,6,4]],[[2,6,4],[3,6,8,6]]).
q([[7,1,2],[5,6,0,4],[6,7,2],[0,0,5,3]],[[9,3,4],[7,8,2,6],[8,9,4],[0,0,5,3]]).
q([[0,0,0,1],[1,6,2,3],[4,4,4],[0,4,3,6]],[[2,2,2,3],[1,6,2,3],[6,6,6],[0,4,3,6]]).
q([[5,1,3],[2,6,6,7],[7,2,2,1],[5,5,0]],[[5,1,3],[4,8,8,9],[9,4,4,3],[5,5,0]]).
q([[7,6,1],[7,3,6,4]],[[7,6,1],[9,5,8,6]]).
