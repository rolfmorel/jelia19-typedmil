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
my_max_list4(A,B):-max_list(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
my_odd6(A):-1 is A mod 2.
my_element7(A,B):-member(B,A).
my_tail8([_|TL],TL).
my_lowercase9(A):-downcase_atom(A,A),char_code(A,_).
my_uppercase10(A):-upcase_atom(A,A),char_code(A,_).
my_tolower11(A,B):-downcase_atom(A,B),char_code(A,_).
my_sumlist12(A,B):-sumlist(A,B).
my_flatten13(A,B):-flatten(A,B).
my_list_to_set14(A,B):-list_to_set(A,B).
prim(my_even2/1).
prim(my_double3/2).
prim(my_max_list4/2).
prim(my_pred5/2).
prim(my_odd6/1).
prim(my_element7/2).
prim(my_tail8/2).
prim(my_lowercase9/1).
prim(my_uppercase10/1).
prim(my_tolower11/2).
prim(my_sumlist12/2).
prim(my_flatten13/2).
prim(my_list_to_set14/2).
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
p([2,1,5,1,2,5,9],[4,4]).
p([0,2,0,7],[0,4,0]).
p([4,2,4,7,4,4,1,5],[8,4,8,8,8]).
p([2,5,1,2,5,3,2,2,7],[4,4,4,4]).
p([7,0,4,0,0,0],[0,8,0,0,0]).
q([2,4,0,4,3,7],[5,4,8,8,0]).
q([4,9,2,2,7],[4,4,8,3]).
q([2,5,2,9,3,9],[4,6,4]).
q([9,9,1,4],[9,8]).
q([5,0,5,4,4,3],[8,8,0,1]).
