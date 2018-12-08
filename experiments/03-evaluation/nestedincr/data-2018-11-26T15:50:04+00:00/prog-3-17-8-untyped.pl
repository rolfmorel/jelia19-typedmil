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
my_min_list2(A,B):-min_list(A,B).
my_tail3([_|TL],TL).
my_pred4(A,B):-succ(B,A),A > 0.
my_lowercase5(A):-downcase_atom(A,A),char_code(A,_).
my_head6([H|_],H).
my_flatten7(A,B):-flatten(A,B).
my_toupper8(A,B):-upcase_atom(A,B),char_code(A,_).
my_last9(A,B):-last(A,B).

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
my_reverse12(A,B):-reverse(A,B).
my_len13(A,B):-length(A,B).
my_msort14(A,B):-msort(A,B).
my_even15(A):-0 is A mod 2.
my_set16(A):-list_to_set(A,A).
my_element17(A,B):-member(B,A).
my_list_to_set18(A,B):-list_to_set(A,B).
prim(my_succ1/2).
prim(my_min_list2/2).
prim(my_tail3/2).
prim(my_pred4/2).
prim(my_lowercase5/1).
prim(my_head6/2).
prim(my_flatten7/2).
prim(my_toupper8/2).
prim(my_last9/2).
prim(my_odd11/1).
prim(my_reverse12/2).
prim(my_len13/2).
prim(my_msort14/2).
prim(my_even15/1).
prim(my_set16/1).
prim(my_element17/2).
prim(my_list_to_set18/2).
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
p([[1,0,1],[3,6,4],[5,6,7]],[[3,2,3],[5,8,6],[7,8,9]]).
p([[6,1,2,2],[6,7,2,7],[5,3,3],[3,1,3,1]],[[8,3,4,4],[8,9,4,9],[7,5,5],[5,3,5,3]]).
p([[0,6,0,0],[7,3,1]],[[2,8,2,2],[9,5,3]]).
p([[1,5,0,5],[6,3,0],[5,0,3,6],[1,6,5]],[[3,7,2,7],[8,5,2],[7,2,5,8],[3,8,7]]).
p([[6,6,2,7],[1,2,3,1],[6,6,5],[4,4,0]],[[8,8,4,9],[3,4,5,3],[8,8,7],[6,6,2]]).
q([[6,7,7],[4,2,5,3],[4,1,1,6],[6,4,7]],[[6,7,7],[6,4,7,5],[6,3,3,8],[8,6,9]]).
q([[7,5,0,1],[1,3,3,3],[6,0,0,3],[0,3,3,5]],[[7,5,0,1],[3,5,5,5],[8,2,2,5],[0,3,3,5]]).
q([[5,5,4,0],[7,0,2],[4,1,2,3]],[[5,5,4,0],[9,2,4],[6,3,4,5]]).
q([[6,4,3,7],[3,5,0,2],[7,1,2,0],[1,6,2,7]],[[6,4,3,7],[5,7,2,4],[9,3,4,2],[3,8,4,9]]).
q([[1,1,5],[7,7,3,0],[5,3,6],[6,0,5]],[[3,3,7],[9,9,5,2],[5,3,6],[6,0,5]]).
