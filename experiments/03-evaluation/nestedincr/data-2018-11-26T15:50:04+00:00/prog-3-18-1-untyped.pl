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
my_element2(A,B):-member(B,A).
my_reverse3(A,B):-reverse(A,B).
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_tolower5(A,B):-downcase_atom(A,B),char_code(A,_).
my_msort6(A,B):-msort(A,B).
my_head7([H|_],H).
my_pred8(A,B):-succ(B,A),A > 0.
my_odd9(A):-1 is A mod 2.
my_list_to_set10(A,B):-list_to_set(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_last12(A,B):-last(A,B).
my_uppercase13(A):-upcase_atom(A,A),char_code(A,_).
my_even14(A):-0 is A mod 2.

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

my_set16(A):-list_to_set(A,A).
my_flatten17(A,B):-flatten(A,B).
my_max_list18(A,B):-max_list(A,B).
my_min_list19(A,B):-min_list(A,B).
prim(my_succ1/2).
prim(my_element2/2).
prim(my_reverse3/2).
prim(my_lowercase4/1).
prim(my_tolower5/2).
prim(my_msort6/2).
prim(my_head7/2).
prim(my_pred8/2).
prim(my_odd9/1).
prim(my_list_to_set10/2).
prim(my_sumlist11/2).
prim(my_last12/2).
prim(my_uppercase13/1).
prim(my_even14/1).
prim(my_set16/1).
prim(my_flatten17/2).
prim(my_max_list18/2).
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
p([[1,5,1],[7,2,3,4],[3,6,0,0],[6,2,3,3]],[[3,7,3],[9,4,5,6],[5,8,2,2],[8,4,5,5]]).
p([[3,2,2,2],[0,5,5,7],[3,5,2],[0,1,6]],[[5,4,4,4],[2,7,7,9],[5,7,4],[2,3,8]]).
p([[0,6,4,0],[6,7,6]],[[2,8,6,2],[8,9,8]]).
p([[3,1,5,5],[7,2,5],[7,2,5]],[[5,3,7,7],[9,4,7],[9,4,7]]).
p([[2,6,6,0],[7,3,3,2],[1,5,7],[6,2,7,0]],[[4,8,8,2],[9,5,5,4],[3,7,9],[8,4,9,2]]).
q([[5,1,5],[0,3,0,5],[1,1,5,4]],[[7,3,7],[0,3,0,5],[3,3,7,6]]).
q([[3,5,5,2],[2,1,0],[1,4,6],[6,7,7]],[[3,5,5,2],[4,3,2],[3,6,8],[8,9,9]]).
q([[7,3,6],[1,2,1,1],[7,3,6]],[[9,5,8],[3,4,3,3],[7,3,6]]).
q([[2,4,3],[4,3,1]],[[2,4,3],[6,5,3]]).
q([[2,0,7,7],[4,4,1,0],[0,0,2,7]],[[4,2,9,9],[4,4,1,0],[2,2,4,9]]).
