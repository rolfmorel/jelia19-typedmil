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
my_reverse5(A,B):-reverse(A,B).
my_min_list6(A,B):-min_list(A,B).
my_toupper7(A,B):-upcase_atom(A,B),char_code(A,_).
my_last8(A,B):-last(A,B).
my_uppercase9(A):-upcase_atom(A,A),char_code(A,_).
my_flatten10(A,B):-flatten(A,B).
my_len11(A,B):-length(A,B).
my_max_list12(A,B):-max_list(A,B).
my_lowercase13(A):-downcase_atom(A,A),char_code(A,_).
my_msort14(A,B):-msort(A,B).
my_succ15(A,B):-succ(A,B),B =< 10.
my_odd16(A):-1 is A mod 2.
my_sumlist17(A,B):-sumlist(A,B).
my_list_to_set18(A,B):-list_to_set(A,B).
my_head19([H|_],H).
my_set20(A):-list_to_set(A,A).
my_element21(A,B):-member(B,A).
prim(my_even2/1).
prim(my_double3/2).
prim(my_pred4/2).
prim(my_reverse5/2).
prim(my_min_list6/2).
prim(my_toupper7/2).
prim(my_last8/2).
prim(my_uppercase9/1).
prim(my_flatten10/2).
prim(my_len11/2).
prim(my_max_list12/2).
prim(my_lowercase13/1).
prim(my_msort14/2).
prim(my_succ15/2).
prim(my_odd16/1).
prim(my_sumlist17/2).
prim(my_list_to_set18/2).
prim(my_head19/2).
prim(my_set20/1).
prim(my_element21/2).
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
p([7,0,3,0,3,4,0],[0,0,8,0]).
p([0,0,7,0],[0,0,0]).
p([3,2,4,5,4],[4,8,8]).
p([0,4,5,3,1,4,4],[0,8,8,8]).
p([3,9,1,3,2,3,3,3],[4]).
q([2,4,4,4,4,0,1,0],[6,0,8,0,8,4,8,8]).
q([0,7,2,2,2,0,5,4,0],[0,8,4,1,4,0,4,0]).
q([3,0,0,9,4,0,1,0],[0,0,0,0,8,3]).
q([0,7,2,4,0,0,9],[4,8,0,6,0,0]).
q([0,9,4,2,0,1],[0,0,8,4,8]).
