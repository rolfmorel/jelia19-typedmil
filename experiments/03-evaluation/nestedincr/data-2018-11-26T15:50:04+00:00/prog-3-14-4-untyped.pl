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
my_lowercase2(A):-downcase_atom(A,A),char_code(A,_).
my_min_list3(A,B):-min_list(A,B).
my_uppercase4(A):-upcase_atom(A,A),char_code(A,_).

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

my_list_to_set6(A,B):-list_to_set(A,B).
my_tail7([_|TL],TL).
my_msort8(A,B):-msort(A,B).
my_set9(A):-list_to_set(A,A).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_odd11(A):-1 is A mod 2.
my_double12(N,M):-M is 2*N,M =< 10.
my_reverse13(A,B):-reverse(A,B).
my_flatten14(A,B):-flatten(A,B).
my_element15(A,B):-member(B,A).
prim(my_succ1/2).
prim(my_lowercase2/1).
prim(my_min_list3/2).
prim(my_uppercase4/1).
prim(my_list_to_set6/2).
prim(my_tail7/2).
prim(my_msort8/2).
prim(my_set9/1).
prim(my_toupper10/2).
prim(my_odd11/1).
prim(my_double12/2).
prim(my_reverse13/2).
prim(my_flatten14/2).
prim(my_element15/2).
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
p([[3,6,2,6],[3,1,1]],[[5,8,4,8],[5,3,3]]).
p([[6,5,2],[0,3,3]],[[8,7,4],[2,5,5]]).
p([[1,3,0,0],[2,7,4],[5,6,1,0]],[[3,5,2,2],[4,9,6],[7,8,3,2]]).
p([[2,4,4],[1,1,3,2]],[[4,6,6],[3,3,5,4]]).
p([[1,5,1],[7,1,1],[1,7,5,5]],[[3,7,3],[9,3,3],[3,9,7,7]]).
q([[5,6,2,3],[2,3,4,3],[2,4,4],[3,3,7,5]],[[5,6,2,3],[4,5,6,5],[4,6,6],[3,3,7,5]]).
q([[7,7,4],[3,6,3],[6,1,4]],[[9,9,6],[5,8,5],[6,1,4]]).
q([[7,5,6,2],[3,1,1,2],[4,4,4,7],[3,0,4]],[[9,7,8,4],[3,1,1,2],[4,4,4,7],[5,2,6]]).
q([[6,0,7],[3,7,4],[4,2,3,6],[0,6,6]],[[8,2,9],[5,9,6],[4,2,3,6],[2,8,8]]).
q([[1,4,1,1],[4,7,7],[4,0,3,1],[6,1,2]],[[3,6,3,3],[6,9,9],[6,2,5,3],[6,1,2]]).
