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
my_reverse2(A,B):-reverse(A,B).
my_toupper3(A,B):-upcase_atom(A,B),char_code(A,_).
my_set4(A):-list_to_set(A,A).
my_list_to_set5(A,B):-list_to_set(A,B).

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

my_last7(A,B):-last(A,B).
my_lowercase8(A):-downcase_atom(A,A),char_code(A,_).
my_even9(A):-0 is A mod 2.
my_double10(N,M):-M is 2*N,M =< 10.
prim(my_succ1/2).
prim(my_reverse2/2).
prim(my_toupper3/2).
prim(my_set4/1).
prim(my_list_to_set5/2).
prim(my_last7/2).
prim(my_lowercase8/1).
prim(my_even9/1).
prim(my_double10/2).
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
p([[5,7,5],[0,0,5],[5,4,6,7],[0,5,4]],[[7,9,7],[2,2,7],[7,6,8,9],[2,7,6]]).
p([[7,2,7],[7,6,4,6],[1,5,4],[7,3,6,6]],[[9,4,9],[9,8,6,8],[3,7,6],[9,5,8,8]]).
p([[1,7,6],[1,0,7],[0,2,4],[4,3,2]],[[3,9,8],[3,2,9],[2,4,6],[6,5,4]]).
p([[4,5,4],[5,1,4],[7,6,5,6]],[[6,7,6],[7,3,6],[9,8,7,8]]).
p([[3,7,2,4],[7,0,1,5],[5,6,3,6]],[[5,9,4,6],[9,2,3,7],[7,8,5,8]]).
q([[4,6,1,7],[1,2,5,6],[7,3,6,2],[5,0,6]],[[6,8,3,9],[3,4,7,8],[7,3,6,2],[5,0,6]]).
q([[2,3,5,5],[3,6,1]],[[4,5,7,7],[3,6,1]]).
q([[5,1,2,6],[4,5,4,3],[3,6,0,1],[1,4,2]],[[5,1,2,6],[6,7,6,5],[5,8,2,3],[3,6,4]]).
q([[3,3,2,0],[1,7,0,7]],[[3,3,2,0],[3,9,2,9]]).
q([[0,5,6],[7,5,1,1],[6,3,4]],[[0,5,6],[9,7,3,3],[8,5,6]]).
