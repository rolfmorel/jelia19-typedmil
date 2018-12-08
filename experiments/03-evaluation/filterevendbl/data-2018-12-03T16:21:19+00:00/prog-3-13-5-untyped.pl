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
my_set4(A):-list_to_set(A,A).
my_max_list5(A,B):-max_list(A,B).
my_tail6([_|TL],TL).
my_reverse7(A,B):-reverse(A,B).
my_odd8(A):-1 is A mod 2.
my_sumlist9(A,B):-sumlist(A,B).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_flatten11(A,B):-flatten(A,B).
my_last12(A,B):-last(A,B).
my_uppercase13(A):-upcase_atom(A,A),char_code(A,_).
my_lowercase14(A):-downcase_atom(A,A),char_code(A,_).
my_succ15(A,B):-succ(A,B),B =< 10.
my_element16(A,B):-member(B,A).
prim(my_even2/1).
prim(my_double3/2).
prim(my_set4/1).
prim(my_max_list5/2).
prim(my_tail6/2).
prim(my_reverse7/2).
prim(my_odd8/1).
prim(my_sumlist9/2).
prim(my_toupper10/2).
prim(my_flatten11/2).
prim(my_last12/2).
prim(my_uppercase13/1).
prim(my_lowercase14/1).
prim(my_succ15/2).
prim(my_element16/2).
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
p([9,0,1,5,2,2],[0,4,4]).
p([4,5,9,0,4,9,0],[8,0,8,0]).
p([4,4,9,7,3,0],[8,8,0]).
p([4,9,3,2,0,0,4],[8,4,0,0,8]).
p([5,4,3,9,0,4,1,2],[8,0,8,4]).
q([5,0,0,9,7],[0,0,2]).
q([7,2,4,0,0,9,3,0,9],[0,7,0,0,4,8]).
q([0,3,4,4,3],[0,8,0,8]).
q([1,2,7,9,2],[4,4,8]).
q([1,7,5,5,5,2,7,5,9],[4,3]).
