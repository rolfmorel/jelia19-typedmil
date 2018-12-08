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
my_last2(A,B):-last(A,B).
my_pred3(A,B):-succ(B,A),A > 0.
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_msort5(A,B):-msort(A,B).
my_element6(A,B):-member(B,A).
my_tolower7(A,B):-downcase_atom(A,B),char_code(A,_).
my_min_list8(A,B):-min_list(A,B).

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

my_list_to_set10(A,B):-list_to_set(A,B).
my_even11(A):-0 is A mod 2.
my_max_list12(A,B):-max_list(A,B).
my_len13(A,B):-length(A,B).
my_uppercase14(A):-upcase_atom(A,A),char_code(A,_).
my_set15(A):-list_to_set(A,A).
my_head16([H|_],H).
my_tail17([_|TL],TL).
my_sumlist18(A,B):-sumlist(A,B).
my_flatten19(A,B):-flatten(A,B).
my_odd20(A):-1 is A mod 2.
my_double21(N,M):-M is 2*N,M =< 10.
my_toupper22(A,B):-upcase_atom(A,B),char_code(A,_).
prim(my_succ1/2).
prim(my_last2/2).
prim(my_pred3/2).
prim(my_lowercase4/1).
prim(my_msort5/2).
prim(my_element6/2).
prim(my_tolower7/2).
prim(my_min_list8/2).
prim(my_list_to_set10/2).
prim(my_even11/1).
prim(my_max_list12/2).
prim(my_len13/2).
prim(my_uppercase14/1).
prim(my_set15/1).
prim(my_head16/2).
prim(my_tail17/2).
prim(my_sumlist18/2).
prim(my_flatten19/2).
prim(my_odd20/1).
prim(my_double21/2).
prim(my_toupper22/2).
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
p([[3,4,1],[5,3,2],[0,3,3,2],[2,5,2,2]],[[5,6,3],[7,5,4],[2,5,5,4],[4,7,4,4]]).
p([[3,6,5],[3,2,6],[2,3,4],[7,6,4]],[[5,8,7],[5,4,8],[4,5,6],[9,8,6]]).
p([[0,4,7,4],[6,6,6],[0,4,4,5],[1,5,2,5]],[[2,6,9,6],[8,8,8],[2,6,6,7],[3,7,4,7]]).
p([[7,4,4,7],[2,5,2,4],[5,2,3,7]],[[9,6,6,9],[4,7,4,6],[7,4,5,9]]).
p([[2,7,1],[2,3,2],[3,5,3,1],[3,1,2]],[[4,9,3],[4,5,4],[5,7,5,3],[5,3,4]]).
q([[1,6,6,0],[7,0,6,3]],[[1,6,6,0],[9,2,8,5]]).
q([[3,2,3,1],[0,1,4,5],[4,4,4,4],[7,1,7,7]],[[5,4,5,3],[0,1,4,5],[6,6,6,6],[9,3,9,9]]).
q([[2,1,0,6],[0,4,3]],[[2,1,0,6],[2,6,5]]).
q([[1,5,1],[1,0,5],[4,6,6,1]],[[3,7,3],[3,2,7],[4,6,6,1]]).
q([[4,7,1,7],[4,6,7]],[[4,7,1,7],[6,8,9]]).
