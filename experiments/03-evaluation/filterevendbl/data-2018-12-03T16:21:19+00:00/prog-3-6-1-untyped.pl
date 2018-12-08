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
my_odd5(A):-1 is A mod 2.
my_pred6(A,B):-succ(B,A),A > 0.
my_list_to_set7(A,B):-list_to_set(A,B).
my_reverse8(A,B):-reverse(A,B).
my_len9(A,B):-length(A,B).
prim(my_even2/1).
prim(my_double3/2).
prim(my_succ4/2).
prim(my_odd5/1).
prim(my_pred6/2).
prim(my_list_to_set7/2).
prim(my_reverse8/2).
prim(my_len9/2).
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
p([0,3,2,3,1,0,3,9,0],[0,4,0,0]).
p([9,4,7,0,9],[8,0]).
p([4,5,4,0,9,0],[8,8,0,0]).
p([1,4,4,0,2,2,2,2,0],[8,8,0,4,4,4,4,0]).
p([5,9,4,9,7],[8]).
q([5,4,2,9],[3,4,8]).
q([5,4,2,0,2,4,5,2],[4,0,0,4,8,4,8]).
q([1,1,1,7,1,2,7],[4,6]).
q([0,1,5,3,5,7,1],[0,7]).
q([2,0,1,0,2,3],[0,7,0,4,4]).
