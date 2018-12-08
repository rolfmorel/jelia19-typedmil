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
my_even2(A):-0 is A mod 2.
my_lowercase3(A):-downcase_atom(A,A),char_code(A,_).
my_pred4(A,B):-succ(B,A),A > 0.
my_odd5(A):-1 is A mod 2.
my_double6(N,M):-M is 2*N,M =< 10.
my_flatten7(A,B):-flatten(A,B).
my_set8(A):-list_to_set(A,A).
my_tolower9(A,B):-downcase_atom(A,B),char_code(A,_).
my_tail10([_|TL],TL).
my_sumlist11(A,B):-sumlist(A,B).
my_min_list12(A,B):-min_list(A,B).
my_list_to_set13(A,B):-list_to_set(A,B).

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

my_msort15(A,B):-msort(A,B).
my_last16(A,B):-last(A,B).
my_head17([H|_],H).
my_reverse18(A,B):-reverse(A,B).
my_toupper19(A,B):-upcase_atom(A,B),char_code(A,_).
my_element20(A,B):-member(B,A).
prim(my_succ1/2).
prim(my_even2/1).
prim(my_lowercase3/1).
prim(my_pred4/2).
prim(my_odd5/1).
prim(my_double6/2).
prim(my_flatten7/2).
prim(my_set8/1).
prim(my_tolower9/2).
prim(my_tail10/2).
prim(my_sumlist11/2).
prim(my_min_list12/2).
prim(my_list_to_set13/2).
prim(my_msort15/2).
prim(my_last16/2).
prim(my_head17/2).
prim(my_reverse18/2).
prim(my_toupper19/2).
prim(my_element20/2).
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
p([[7,2,0],[2,5,1],[5,0,0]],[[9,4,2],[4,7,3],[7,2,2]]).
p([[1,3,7,1],[4,4,0,2],[0,6,4,4],[2,1,4]],[[3,5,9,3],[6,6,2,4],[2,8,6,6],[4,3,6]]).
p([[6,0,6],[3,2,4,7],[3,0,0]],[[8,2,8],[5,4,6,9],[5,2,2]]).
p([[7,3,7],[6,1,5],[3,7,2]],[[9,5,9],[8,3,7],[5,9,4]]).
p([[5,3,1],[4,7,0,0]],[[7,5,3],[6,9,2,2]]).
q([[6,0,7],[3,5,1],[7,3,3],[5,3,2]],[[8,2,9],[3,5,1],[9,5,5],[5,3,2]]).
q([[6,1,4,0],[5,6,4]],[[6,1,4,0],[7,8,6]]).
q([[3,4,4,6],[4,2,6,1],[4,0,5,5]],[[5,6,6,8],[4,2,6,1],[6,2,7,7]]).
q([[5,6,0,4],[2,0,6],[7,2,4]],[[7,8,2,6],[2,0,6],[9,4,6]]).
q([[1,7,4,7],[7,1,4]],[[1,7,4,7],[9,3,6]]).
