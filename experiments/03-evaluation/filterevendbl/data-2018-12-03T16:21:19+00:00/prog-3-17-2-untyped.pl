:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).

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


map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_even2(A):-0 is A mod 2.
my_double3(N,M):-M is 2*N,M =< 10.
my_sumlist4(A,B):-sumlist(A,B).
my_tail5([_|TL],TL).
my_max_list6(A,B):-max_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_tolower8(A,B):-downcase_atom(A,B),char_code(A,_).
my_lowercase9(A):-downcase_atom(A,A),char_code(A,_).
my_last10(A,B):-last(A,B).
my_head11([H|_],H).
my_list_to_set12(A,B):-list_to_set(A,B).
my_min_list13(A,B):-min_list(A,B).
my_len14(A,B):-length(A,B).
my_succ15(A,B):-succ(A,B),B =< 10.
my_flatten16(A,B):-flatten(A,B).
my_pred17(A,B):-succ(B,A),A > 0.
my_set18(A):-list_to_set(A,A).
my_odd19(A):-1 is A mod 2.
my_msort20(A,B):-msort(A,B).
prim(my_even2/1).
prim(my_double3/2).
prim(my_sumlist4/2).
prim(my_tail5/2).
prim(my_max_list6/2).
prim(my_reverse7/2).
prim(my_tolower8/2).
prim(my_lowercase9/1).
prim(my_last10/2).
prim(my_head11/2).
prim(my_list_to_set12/2).
prim(my_min_list13/2).
prim(my_len14/2).
prim(my_succ15/2).
prim(my_flatten16/2).
prim(my_pred17/2).
prim(my_set18/1).
prim(my_odd19/1).
prim(my_msort20/2).
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
p([3,3,3,9,4],[8]).
p([2,0,7,0],[4,0,0]).
p([2,0,9,5,4,0,1,5,0],[4,0,8,0,0]).
p([3,1,0,7],[0]).
p([5,9,2,5],[4]).
q([1,2,0,0],[0,6,4,0]).
q([2,9,7,4],[4,7,8]).
q([4,3,3,1,0,0,9,2],[8,4,0,0,1]).
q([2,9,2,4,0],[0,4,4,8,9]).
q([9,7,5,9,7,0],[2,0]).
