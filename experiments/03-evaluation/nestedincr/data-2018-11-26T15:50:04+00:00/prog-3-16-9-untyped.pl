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
my_set3(A):-list_to_set(A,A).
my_double4(N,M):-M is 2*N,M =< 10.
my_tail5([_|TL],TL).
my_odd6(A):-1 is A mod 2.
my_even7(A):-0 is A mod 2.
my_len8(A,B):-length(A,B).
my_flatten9(A,B):-flatten(A,B).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_head11([H|_],H).

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

my_min_list13(A,B):-min_list(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_reverse15(A,B):-reverse(A,B).
my_uppercase16(A):-upcase_atom(A,A),char_code(A,_).
my_msort17(A,B):-msort(A,B).
prim(my_succ1/2).
prim(my_last2/2).
prim(my_set3/1).
prim(my_double4/2).
prim(my_tail5/2).
prim(my_odd6/1).
prim(my_even7/1).
prim(my_len8/2).
prim(my_flatten9/2).
prim(my_toupper10/2).
prim(my_head11/2).
prim(my_min_list13/2).
prim(my_sumlist14/2).
prim(my_reverse15/2).
prim(my_uppercase16/1).
prim(my_msort17/2).
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
p([[2,5,5],[6,4,2],[0,1,3],[0,0,3,7]],[[4,7,7],[8,6,4],[2,3,5],[2,2,5,9]]).
p([[1,0,6,1],[3,0,4,7],[6,1,4],[4,1,6,1]],[[3,2,8,3],[5,2,6,9],[8,3,6],[6,3,8,3]]).
p([[6,1,3,3],[0,6,2,3]],[[8,3,5,5],[2,8,4,5]]).
p([[0,3,6,2],[4,3,4],[5,0,5],[5,3,2]],[[2,5,8,4],[6,5,6],[7,2,7],[7,5,4]]).
p([[1,1,5,5],[3,0,6,7],[6,0,7,3]],[[3,3,7,7],[5,2,8,9],[8,2,9,5]]).
q([[6,4,0,6],[6,2,1],[3,0,2]],[[8,6,2,8],[8,4,3],[3,0,2]]).
q([[1,0,3],[1,5,6],[3,1,7,7],[6,7,6]],[[3,2,5],[1,5,6],[3,1,7,7],[8,9,8]]).
q([[5,7,5],[0,0,5,1],[0,7,6]],[[7,9,7],[2,2,7,3],[0,7,6]]).
q([[1,6,0],[1,4,6]],[[3,8,2],[1,4,6]]).
q([[0,5,7],[0,0,5]],[[2,7,9],[0,0,5]]).
