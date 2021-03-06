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
my_len3(A,B):-length(A,B).
my_uppercase4(A):-upcase_atom(A,A),char_code(A,_).
my_element5(A,B):-member(B,A).
my_pred6(A,B):-succ(B,A),A > 0.
my_odd7(A):-1 is A mod 2.
my_max_list8(A,B):-max_list(A,B).
my_head9([H|_],H).
my_min_list10(A,B):-min_list(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_toupper13(A,B):-upcase_atom(A,B),char_code(A,_).

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

my_reverse15(A,B):-reverse(A,B).
my_tail16([_|TL],TL).
prim(my_succ1/2).
prim(my_tolower2/2).
prim(my_len3/2).
prim(my_uppercase4/1).
prim(my_element5/2).
prim(my_pred6/2).
prim(my_odd7/1).
prim(my_max_list8/2).
prim(my_head9/2).
prim(my_min_list10/2).
prim(my_list_to_set11/2).
prim(my_sumlist12/2).
prim(my_toupper13/2).
prim(my_reverse15/2).
prim(my_tail16/2).
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
p([[3,2,6,3],[7,1,3],[5,2,6],[4,1,3,5]],[[5,4,8,5],[9,3,5],[7,4,8],[6,3,5,7]]).
p([[1,0,2],[7,2,6]],[[3,2,4],[9,4,8]]).
p([[3,1,7,0],[4,1,4]],[[5,3,9,2],[6,3,6]]).
p([[4,1,5],[4,3,2,0],[3,5,0]],[[6,3,7],[6,5,4,2],[5,7,2]]).
p([[2,7,1,5],[4,7,0]],[[4,9,3,7],[6,9,2]]).
q([[1,2,1],[4,6,7],[4,6,0,6],[2,6,1,2]],[[1,2,1],[6,8,9],[6,8,2,8],[2,6,1,2]]).
q([[4,0,5,5],[5,3,7]],[[6,2,7,7],[5,3,7]]).
q([[2,5,4,7],[6,0,0,7]],[[2,5,4,7],[8,2,2,9]]).
q([[4,6,7],[2,6,0,4],[2,2,1,3]],[[6,8,9],[2,6,0,4],[4,4,3,5]]).
q([[7,1,0],[2,5,4]],[[9,3,2],[2,5,4]]).
