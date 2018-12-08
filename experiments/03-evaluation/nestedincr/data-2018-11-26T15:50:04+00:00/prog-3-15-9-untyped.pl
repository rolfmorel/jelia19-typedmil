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
my_head2([H|_],H).
my_len3(A,B):-length(A,B).
my_double4(N,M):-M is 2*N,M =< 10.
my_reverse5(A,B):-reverse(A,B).
my_set6(A):-list_to_set(A,A).
my_toupper7(A,B):-upcase_atom(A,B),char_code(A,_).
my_even8(A):-0 is A mod 2.
my_min_list9(A,B):-min_list(A,B).

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

my_msort11(A,B):-msort(A,B).
my_uppercase12(A):-upcase_atom(A,A),char_code(A,_).
my_list_to_set13(A,B):-list_to_set(A,B).
my_tail14([_|TL],TL).
my_max_list15(A,B):-max_list(A,B).
my_sumlist16(A,B):-sumlist(A,B).
prim(my_succ1/2).
prim(my_head2/2).
prim(my_len3/2).
prim(my_double4/2).
prim(my_reverse5/2).
prim(my_set6/1).
prim(my_toupper7/2).
prim(my_even8/1).
prim(my_min_list9/2).
prim(my_msort11/2).
prim(my_uppercase12/1).
prim(my_list_to_set13/2).
prim(my_tail14/2).
prim(my_max_list15/2).
prim(my_sumlist16/2).
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
p([[1,3,3],[5,6,7,4],[6,2,3,6],[1,5,7,0]],[[3,5,5],[7,8,9,6],[8,4,5,8],[3,7,9,2]]).
p([[0,1,6],[1,3,6,0]],[[2,3,8],[3,5,8,2]]).
p([[7,1,4],[0,4,0],[2,3,4]],[[9,3,6],[2,6,2],[4,5,6]]).
p([[7,7,1],[4,1,4]],[[9,9,3],[6,3,6]]).
p([[7,2,7,5],[0,5,7,1],[0,1,6]],[[9,4,9,7],[2,7,9,3],[2,3,8]]).
q([[1,3,6],[7,1,2,0]],[[1,3,6],[9,3,4,2]]).
q([[4,3,1,6],[3,2,0,5]],[[4,3,1,6],[5,4,2,7]]).
q([[3,1,0],[1,3,1],[4,4,6],[4,5,4,4]],[[5,3,2],[1,3,1],[6,6,8],[6,7,6,6]]).
q([[2,3,6],[3,5,7]],[[4,5,8],[3,5,7]]).
q([[3,2,1,5],[4,3,0],[6,4,4],[6,6,1,6]],[[5,4,3,7],[4,3,0],[8,6,6],[8,8,3,8]]).
