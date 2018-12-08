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
my_toupper4(A,B):-upcase_atom(A,B),char_code(A,_).
my_min_list5(A,B):-min_list(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_max_list7(A,B):-max_list(A,B).
my_lowercase8(A):-downcase_atom(A,A),char_code(A,_).
my_list_to_set9(A,B):-list_to_set(A,B).
my_head10([H|_],H).
my_odd11(A):-1 is A mod 2.
my_len12(A,B):-length(A,B).
my_tolower13(A,B):-downcase_atom(A,B),char_code(A,_).
my_pred14(A,B):-succ(B,A),A > 0.
my_reverse15(A,B):-reverse(A,B).
my_element16(A,B):-member(B,A).
my_succ17(A,B):-succ(A,B),B =< 10.
my_tail18([_|TL],TL).
my_set19(A):-list_to_set(A,A).
my_flatten20(A,B):-flatten(A,B).
my_msort21(A,B):-msort(A,B).
my_last22(A,B):-last(A,B).
my_uppercase23(A):-upcase_atom(A,A),char_code(A,_).
prim(my_even2/1).
prim(my_double3/2).
prim(my_toupper4/2).
prim(my_min_list5/2).
prim(my_sumlist6/2).
prim(my_max_list7/2).
prim(my_lowercase8/1).
prim(my_list_to_set9/2).
prim(my_head10/2).
prim(my_odd11/1).
prim(my_len12/2).
prim(my_tolower13/2).
prim(my_pred14/2).
prim(my_reverse15/2).
prim(my_element16/2).
prim(my_succ17/2).
prim(my_tail18/2).
prim(my_set19/1).
prim(my_flatten20/2).
prim(my_msort21/2).
prim(my_last22/2).
prim(my_uppercase23/1).
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
p([1,1,1,2,3,4,4,0],[4,8,8,0]).
p([2,7,2,2],[4,4,4]).
p([4,5,5,2],[8,4]).
p([2,5,4,7,3],[4,8]).
p([0,0,0,5,3,7],[0,0,0]).
q([9,7,9,4,2,7,3,7,4],[8,4,9,8]).
q([7,9,7,4],[8,6]).
q([5,9,0,0,4,4],[8,0,0,8,9]).
q([7,2,5,9,5,7],[4,4]).
q([4,0,3,0,2,4,3,1],[0,0,8,4,7,8]).
