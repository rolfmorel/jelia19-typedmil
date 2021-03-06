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
my_odd2(A):-1 is A mod 2.
my_list_to_set3(A,B):-list_to_set(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_max_list5(A,B):-max_list(A,B).
my_min_list6(A,B):-min_list(A,B).
my_tolower7(A,B):-downcase_atom(A,B),char_code(A,_).

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

my_head9([H|_],H).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_double11(N,M):-M is 2*N,M =< 10.
my_lowercase12(A):-downcase_atom(A,A),char_code(A,_).
my_flatten13(A,B):-flatten(A,B).
my_len14(A,B):-length(A,B).
my_uppercase15(A):-upcase_atom(A,A),char_code(A,_).
my_set16(A):-list_to_set(A,A).
my_msort17(A,B):-msort(A,B).
my_tail18([_|TL],TL).
my_reverse19(A,B):-reverse(A,B).
my_even20(A):-0 is A mod 2.
prim(my_succ1/2).
prim(my_odd2/1).
prim(my_list_to_set3/2).
prim(my_sumlist4/2).
prim(my_max_list5/2).
prim(my_min_list6/2).
prim(my_tolower7/2).
prim(my_head9/2).
prim(my_toupper10/2).
prim(my_double11/2).
prim(my_lowercase12/1).
prim(my_flatten13/2).
prim(my_len14/2).
prim(my_uppercase15/1).
prim(my_set16/1).
prim(my_msort17/2).
prim(my_tail18/2).
prim(my_reverse19/2).
prim(my_even20/1).
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
p([[6,6,2],[4,4,2],[2,2,6],[5,7,4,2]],[[8,8,4],[6,6,4],[4,4,8],[7,9,6,4]]).
p([[4,4,7,3],[6,5,0,5]],[[6,6,9,5],[8,7,2,7]]).
p([[7,7,3,4],[7,6,1],[6,4,5],[1,3,5,0]],[[9,9,5,6],[9,8,3],[8,6,7],[3,5,7,2]]).
p([[2,4,5],[5,4,4,3],[0,3,5],[7,2,1]],[[4,6,7],[7,6,6,5],[2,5,7],[9,4,3]]).
p([[0,5,2,5],[0,2,2]],[[2,7,4,7],[2,4,4]]).
q([[2,7,7],[6,2,1]],[[2,7,7],[8,4,3]]).
q([[5,0,1,6],[6,5,4]],[[7,2,3,8],[6,5,4]]).
q([[3,4,1,3],[0,6,4],[4,7,7],[0,2,4]],[[5,6,3,5],[2,8,6],[6,9,9],[0,2,4]]).
q([[1,6,5,5],[7,1,7],[1,3,0,4],[6,6,1]],[[3,8,7,7],[9,3,9],[3,5,2,6],[6,6,1]]).
q([[5,7,1],[5,4,4],[4,3,3],[6,5,5,3]],[[7,9,3],[5,4,4],[6,5,5],[8,7,7,5]]).
