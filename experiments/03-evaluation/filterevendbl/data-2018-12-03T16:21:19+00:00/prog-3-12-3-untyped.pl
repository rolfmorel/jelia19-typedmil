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
my_reverse4(A,B):-reverse(A,B).
my_set5(A):-list_to_set(A,A).
my_msort6(A,B):-msort(A,B).
my_tail7([_|TL],TL).
my_succ8(A,B):-succ(A,B),B =< 10.
my_sumlist9(A,B):-sumlist(A,B).
my_flatten10(A,B):-flatten(A,B).
my_lowercase11(A):-downcase_atom(A,A),char_code(A,_).
my_tolower12(A,B):-downcase_atom(A,B),char_code(A,_).
my_last13(A,B):-last(A,B).
my_min_list14(A,B):-min_list(A,B).
my_head15([H|_],H).
prim(my_even2/1).
prim(my_double3/2).
prim(my_reverse4/2).
prim(my_set5/1).
prim(my_msort6/2).
prim(my_tail7/2).
prim(my_succ8/2).
prim(my_sumlist9/2).
prim(my_flatten10/2).
prim(my_lowercase11/1).
prim(my_tolower12/2).
prim(my_last13/2).
prim(my_min_list14/2).
prim(my_head15/2).
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
p([2,5,2,0],[4,4,0]).
p([7,4,1,2],[8,4]).
p([4,9,4,0,3,7,2,4,0],[8,8,0,4,8,0]).
p([2,5,7,3],[4]).
p([2,5,0,9,9,4,1,4,0],[4,0,8,8,0]).
q([9,7,5,4,4,5,9,0],[0,8,8,0]).
q([0,1,0,9,2],[8,0,0,4]).
q([7,9,7,3,7,0,1,7,9],[5,0]).
q([1,1,7,5,0],[0,0]).
q([3,4,4,4],[8,8,5,8]).
