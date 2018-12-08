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
my_succ4(A,B):-succ(A,B),B =< 10.
my_sumlist5(A,B):-sumlist(A,B).
my_tolower6(A,B):-downcase_atom(A,B),char_code(A,_).
my_element7(A,B):-member(B,A).
my_list_to_set8(A,B):-list_to_set(A,B).
my_min_list9(A,B):-min_list(A,B).
my_odd10(A):-1 is A mod 2.
my_msort11(A,B):-msort(A,B).
my_last12(A,B):-last(A,B).
my_reverse13(A,B):-reverse(A,B).
my_flatten14(A,B):-flatten(A,B).
my_head15([H|_],H).
my_max_list16(A,B):-max_list(A,B).
my_tail17([_|TL],TL).
my_uppercase18(A):-upcase_atom(A,A),char_code(A,_).
my_toupper19(A,B):-upcase_atom(A,B),char_code(A,_).
my_lowercase20(A):-downcase_atom(A,A),char_code(A,_).
my_len21(A,B):-length(A,B).
my_set22(A):-list_to_set(A,A).
prim(my_even2/1).
prim(my_double3/2).
prim(my_succ4/2).
prim(my_sumlist5/2).
prim(my_tolower6/2).
prim(my_element7/2).
prim(my_list_to_set8/2).
prim(my_min_list9/2).
prim(my_odd10/1).
prim(my_msort11/2).
prim(my_last12/2).
prim(my_reverse13/2).
prim(my_flatten14/2).
prim(my_head15/2).
prim(my_max_list16/2).
prim(my_tail17/2).
prim(my_uppercase18/1).
prim(my_toupper19/2).
prim(my_lowercase20/1).
prim(my_len21/2).
prim(my_set22/1).
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
p([0,1,0,0,7],[0,0,0]).
p([9,9,5,2],[4]).
p([4,4,0,4,4,4,0,3,2],[8,8,0,8,8,8,0,4]).
p([0,1,2,0,1,2,5,2],[0,4,0,4,4]).
p([4,4,3,5],[8,8]).
q([9,5,9,0,0,1,5],[0,0,0]).
q([2,2,7,3,0,0,2,1],[4,0,4,0,9,4]).
q([5,9,2,7],[5,4]).
q([4,7,9,0,4,4,4,0,4],[8,8,0,8,8,0,3,8]).
q([7,7,5,3,4,9,5],[8,7]).
