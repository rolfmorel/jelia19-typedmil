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
my_last4(A,B):-last(A,B).
my_max_list5(A,B):-max_list(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_len7(A,B):-length(A,B).
my_tail8([_|TL],TL).
my_succ9(A,B):-succ(A,B),B =< 10.
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_msort11(A,B):-msort(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_lowercase13(A):-downcase_atom(A,A),char_code(A,_).
my_head14([H|_],H).
my_element15(A,B):-member(B,A).
my_min_list16(A,B):-min_list(A,B).
my_reverse17(A,B):-reverse(A,B).
my_list_to_set18(A,B):-list_to_set(A,B).
prim(my_even2/1).
prim(my_double3/2).
prim(my_last4/2).
prim(my_max_list5/2).
prim(my_sumlist6/2).
prim(my_len7/2).
prim(my_tail8/2).
prim(my_succ9/2).
prim(my_toupper10/2).
prim(my_msort11/2).
prim(my_pred12/2).
prim(my_lowercase13/1).
prim(my_head14/2).
prim(my_element15/2).
prim(my_min_list16/2).
prim(my_reverse17/2).
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
p([1,0,1,1],[0]).
p([1,4,0,7,9],[8,0]).
p([0,4,5,4],[0,8,8]).
p([2,3,1,1,7],[4]).
p([0,5,3,3,7,4,5,9,1],[0,8]).
q([2,4,7,9,9,2,4,7,2],[4,4,8,4,8,1]).
q([5,3,7,3,2,7,1,9,2],[4,4,9]).
q([0,9,3,5],[0,7]).
q([2,9,7,1,0,2,4],[8,0,0,4,4]).
q([2,1,1,5,3,7],[8,4]).
