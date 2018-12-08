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
my_even2(A):-0 is A mod 2.
my_last3(A,B):-last(A,B).
my_reverse4(A,B):-reverse(A,B).
my_set5(A):-list_to_set(A,A).
my_tolower6(A,B):-downcase_atom(A,B),char_code(A,_).

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

my_max_list8(A,B):-max_list(A,B).
my_uppercase9(A):-upcase_atom(A,A),char_code(A,_).
my_list_to_set10(A,B):-list_to_set(A,B).
my_msort11(A,B):-msort(A,B).
my_lowercase12(A):-downcase_atom(A,A),char_code(A,_).
my_flatten13(A,B):-flatten(A,B).
my_double14(N,M):-M is 2*N,M =< 10.
my_tail15([_|TL],TL).
my_toupper16(A,B):-upcase_atom(A,B),char_code(A,_).
prim(my_succ1/2).
prim(my_even2/1).
prim(my_last3/2).
prim(my_reverse4/2).
prim(my_set5/1).
prim(my_tolower6/2).
prim(my_max_list8/2).
prim(my_uppercase9/1).
prim(my_list_to_set10/2).
prim(my_msort11/2).
prim(my_lowercase12/1).
prim(my_flatten13/2).
prim(my_double14/2).
prim(my_tail15/2).
prim(my_toupper16/2).
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
p([[1,0,0,3],[0,6,3,0],[5,6,1,3],[1,6,4]],[[3,2,2,5],[2,8,5,2],[7,8,3,5],[3,8,6]]).
p([[2,3,2],[5,2,1],[2,7,7],[4,5,7,1]],[[4,5,4],[7,4,3],[4,9,9],[6,7,9,3]]).
p([[1,1,0,0],[6,3,1],[5,7,3,3]],[[3,3,2,2],[8,5,3],[7,9,5,5]]).
p([[5,7,0,5],[1,7,1,5],[3,3,6]],[[7,9,2,7],[3,9,3,7],[5,5,8]]).
p([[3,2,7,6],[7,6,1,0]],[[5,4,9,8],[9,8,3,2]]).
q([[1,2,5,7],[2,7,1,5]],[[1,2,5,7],[4,9,3,7]]).
q([[2,6,2],[0,3,1,5]],[[4,8,4],[0,3,1,5]]).
q([[5,6,7,5],[2,4,4,5],[5,1,2,0],[0,0,5]],[[7,8,9,7],[4,6,6,7],[5,1,2,0],[2,2,7]]).
q([[0,6,5],[3,3,0,7],[1,2,2,5]],[[0,6,5],[5,5,2,9],[3,4,4,7]]).
q([[7,6,0],[0,7,1],[5,5,1],[7,0,3,0]],[[9,8,2],[2,9,3],[5,5,1],[9,2,5,2]]).
