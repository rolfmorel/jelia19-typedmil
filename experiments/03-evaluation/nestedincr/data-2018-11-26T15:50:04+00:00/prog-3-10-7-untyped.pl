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
my_list_to_set2(A,B):-list_to_set(A,B).
my_last3(A,B):-last(A,B).
my_double4(N,M):-M is 2*N,M =< 10.
my_min_list5(A,B):-min_list(A,B).
my_lowercase6(A):-downcase_atom(A,A),char_code(A,_).
my_sumlist7(A,B):-sumlist(A,B).
my_max_list8(A,B):-max_list(A,B).
my_even9(A):-0 is A mod 2.
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).

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

prim(my_succ1/2).
prim(my_list_to_set2/2).
prim(my_last3/2).
prim(my_double4/2).
prim(my_min_list5/2).
prim(my_lowercase6/1).
prim(my_sumlist7/2).
prim(my_max_list8/2).
prim(my_even9/1).
prim(my_toupper10/2).
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
p([[0,7,7,5],[5,7,7],[2,6,3],[4,5,3]],[[2,9,9,7],[7,9,9],[4,8,5],[6,7,5]]).
p([[1,2,5],[5,4,7,3]],[[3,4,7],[7,6,9,5]]).
p([[7,1,6,7],[7,7,6],[6,1,3]],[[9,3,8,9],[9,9,8],[8,3,5]]).
p([[5,2,6,3],[0,5,1,6],[3,2,0,5]],[[7,4,8,5],[2,7,3,8],[5,4,2,7]]).
p([[7,2,0],[4,4,3,6],[4,5,1,5],[3,0,7]],[[9,4,2],[6,6,5,8],[6,7,3,7],[5,2,9]]).
q([[5,3,6,4],[7,5,4,7]],[[5,3,6,4],[9,7,6,9]]).
q([[7,7,7],[0,4,4],[3,5,2,3],[6,6,7,4]],[[9,9,9],[0,4,4],[5,7,4,5],[8,8,9,6]]).
q([[7,4,4],[6,6,6,5]],[[7,4,4],[8,8,8,7]]).
q([[7,5,6,6],[6,4,2]],[[9,7,8,8],[6,4,2]]).
q([[1,2,3],[4,5,2,3]],[[1,2,3],[6,7,4,5]]).
