:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_succ1(A,B):-succ(A,B),B =< 10.
my_max_list2(A,B):-max_list(A,B).

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

my_element4(A,B):-member(B,A).
my_msort5(A,B):-msort(A,B).
my_flatten6(A,B):-flatten(A,B).
my_reverse7(A,B):-reverse(A,B).
my_lowercase8(A):-downcase_atom(A,A),char_code(A,_).
my_double9(N,M):-M is 2*N,M =< 10.
my_set10(A):-list_to_set(A,A).
my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_max_list2/2).
prim(my_element4/2).
prim(my_msort5/2).
prim(my_flatten6/2).
prim(my_reverse7/2).
prim(my_lowercase8/1).
prim(my_double9/2).
prim(my_set10/1).
prim(my_uppercase11/1).
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
p([[2,7,6],[5,5,7],[5,3,1]],[[4,9,8],[7,7,9],[7,5,3]]).
p([[1,7,7],[0,4,0],[7,5,1],[1,6,2,4]],[[3,9,9],[2,6,2],[9,7,3],[3,8,4,6]]).
p([[1,0,0],[0,4,4]],[[3,2,2],[2,6,6]]).
p([[5,1,5,4],[5,3,3],[7,0,3]],[[7,3,7,6],[7,5,5],[9,2,5]]).
p([[5,1,4,7],[5,5,4,7],[6,3,5]],[[7,3,6,9],[7,7,6,9],[8,5,7]]).
q([[2,0,0,4],[6,2,1,3],[4,3,6,3]],[[4,2,2,6],[6,2,1,3],[6,5,8,5]]).
q([[5,0,0],[2,6,5],[3,2,7,0]],[[7,2,2],[2,6,5],[5,4,9,2]]).
q([[4,2,2,5],[1,5,0],[7,0,4,2]],[[4,2,2,5],[3,7,2],[9,2,6,4]]).
q([[0,4,7,1],[6,0,6]],[[0,4,7,1],[8,2,8]]).
q([[0,4,6,6],[3,4,6,1],[3,5,2]],[[0,4,6,6],[5,6,8,3],[5,7,4]]).
