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
my_pred4(A,B):-succ(B,A),A > 0.
my_tail5([_|TL],TL).
my_toupper6(A,B):-upcase_atom(A,B),char_code(A,_).
my_max_list7(A,B):-max_list(A,B).
my_tolower8(A,B):-downcase_atom(A,B),char_code(A,_).
my_reverse9(A,B):-reverse(A,B).
my_msort10(A,B):-msort(A,B).
my_head11([H|_],H).
my_succ12(A,B):-succ(A,B),B =< 10.
my_len13(A,B):-length(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_set15(A):-list_to_set(A,A).
my_lowercase16(A):-downcase_atom(A,A),char_code(A,_).
my_flatten17(A,B):-flatten(A,B).
my_last18(A,B):-last(A,B).
my_uppercase19(A):-upcase_atom(A,A),char_code(A,_).
my_odd20(A):-1 is A mod 2.
my_list_to_set21(A,B):-list_to_set(A,B).
prim(my_even2/1).
prim(my_double3/2).
prim(my_pred4/2).
prim(my_tail5/2).
prim(my_toupper6/2).
prim(my_max_list7/2).
prim(my_tolower8/2).
prim(my_reverse9/2).
prim(my_msort10/2).
prim(my_head11/2).
prim(my_succ12/2).
prim(my_len13/2).
prim(my_sumlist14/2).
prim(my_set15/1).
prim(my_lowercase16/1).
prim(my_flatten17/2).
prim(my_last18/2).
prim(my_uppercase19/1).
prim(my_odd20/1).
prim(my_list_to_set21/2).
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
p([2,5,4,2],[4,8,4]).
p([5,1,0,0],[0,0]).
p([3,7,2,4,3,1,2],[4,8,4]).
p([1,3,0,5,7,1,1,0,9],[0,0]).
p([2,4,0,2,7,3,5,9],[4,8,0,4]).
q([7,2,2,0,0],[4,0,0,1,4]).
q([9,2,2,1,7,5,0],[0,4,6,4]).
q([3,5,0,0,5],[0,0,1]).
q([4,0,2,4,2,3,2,1],[4,0,4,4,8,8,4]).
q([7,2,5,7,1,5,3],[4,9]).
