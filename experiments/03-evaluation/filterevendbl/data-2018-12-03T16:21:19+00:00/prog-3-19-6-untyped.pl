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
my_element5(A,B):-member(B,A).
my_succ6(A,B):-succ(A,B),B =< 10.
my_set7(A):-list_to_set(A,A).
my_tolower8(A,B):-downcase_atom(A,B),char_code(A,_).
my_last9(A,B):-last(A,B).
my_max_list10(A,B):-max_list(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_len12(A,B):-length(A,B).
my_odd13(A):-1 is A mod 2.
my_msort14(A,B):-msort(A,B).
my_lowercase15(A):-downcase_atom(A,A),char_code(A,_).
my_list_to_set16(A,B):-list_to_set(A,B).
my_flatten17(A,B):-flatten(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_reverse19(A,B):-reverse(A,B).
my_uppercase20(A):-upcase_atom(A,A),char_code(A,_).
my_tail21([_|TL],TL).
my_min_list22(A,B):-min_list(A,B).
prim(my_even2/1).
prim(my_double3/2).
prim(my_toupper4/2).
prim(my_element5/2).
prim(my_succ6/2).
prim(my_set7/1).
prim(my_tolower8/2).
prim(my_last9/2).
prim(my_max_list10/2).
prim(my_pred11/2).
prim(my_len12/2).
prim(my_odd13/1).
prim(my_msort14/2).
prim(my_lowercase15/1).
prim(my_list_to_set16/2).
prim(my_flatten17/2).
prim(my_sumlist18/2).
prim(my_reverse19/2).
prim(my_uppercase20/1).
prim(my_tail21/2).
prim(my_min_list22/2).
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
p([4,0,0,9,5],[8,0,0]).
p([2,7,4,5,0],[4,8,0]).
p([9,4,2,0,7,1,9],[8,4,0]).
p([4,2,3,1,2,5,3,1,5],[8,4,4]).
p([1,0,7,4,2,7,0,7,2],[0,8,4,0,4]).
q([5,0,2,1],[6,4,0]).
q([4,2,2,0,1],[4,4,8,0,3]).
q([9,3,3,7,1,4],[8,8]).
q([0,0,4,0,1,0,7],[0,0,5,0,0,8]).
q([0,0,5,0],[0,8,0,0]).
