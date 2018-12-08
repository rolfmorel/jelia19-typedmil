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
my_last2(A,B):-last(A,B).
my_reverse3(A,B):-reverse(A,B).
my_tolower4(A,B):-downcase_atom(A,B),char_code(A,_).
my_even5(A):-0 is A mod 2.
my_lowercase6(A):-downcase_atom(A,A),char_code(A,_).
my_odd7(A):-1 is A mod 2.

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

my_max_list9(A,B):-max_list(A,B).
my_double10(N,M):-M is 2*N,M =< 10.
my_element11(A,B):-member(B,A).
my_uppercase12(A):-upcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_last2/2).
prim(my_reverse3/2).
prim(my_tolower4/2).
prim(my_even5/1).
prim(my_lowercase6/1).
prim(my_odd7/1).
prim(my_max_list9/2).
prim(my_double10/2).
prim(my_element11/2).
prim(my_uppercase12/1).
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
p([[2,1,3],[4,1,3,7]],[[4,3,5],[6,3,5,9]]).
p([[0,7,5,0],[0,2,3,3]],[[2,9,7,2],[2,4,5,5]]).
p([[1,0,2,1],[6,5,6],[4,5,1],[0,5,4,6]],[[3,2,4,3],[8,7,8],[6,7,3],[2,7,6,8]]).
p([[4,0,0],[4,0,2]],[[6,2,2],[6,2,4]]).
p([[7,2,1],[4,7,7,0],[3,4,3,0]],[[9,4,3],[6,9,9,2],[5,6,5,2]]).
q([[5,2,7,7],[1,1,4],[6,5,5,2],[1,3,1,3]],[[7,4,9,9],[3,3,6],[6,5,5,2],[3,5,3,5]]).
q([[7,6,5],[4,4,7,7]],[[7,6,5],[6,6,9,9]]).
q([[3,2,3],[4,4,3,3],[4,1,2]],[[5,4,5],[4,4,3,3],[6,3,4]]).
q([[4,5,3],[3,7,0,4],[4,1,3,6]],[[6,7,5],[5,9,2,6],[4,1,3,6]]).
q([[2,1,4],[0,3,1],[5,6,5]],[[2,1,4],[2,5,3],[7,8,7]]).
