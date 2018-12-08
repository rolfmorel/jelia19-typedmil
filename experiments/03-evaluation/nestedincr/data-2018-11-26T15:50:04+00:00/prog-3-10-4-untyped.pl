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
my_msort2(A,B):-msort(A,B).
my_toupper3(A,B):-upcase_atom(A,B),char_code(A,_).
my_flatten4(A,B):-flatten(A,B).
my_len5(A,B):-length(A,B).
my_tail6([_|TL],TL).
my_reverse7(A,B):-reverse(A,B).

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

my_uppercase9(A):-upcase_atom(A,A),char_code(A,_).
my_pred10(A,B):-succ(B,A),A > 0.
my_min_list11(A,B):-min_list(A,B).
prim(my_succ1/2).
prim(my_msort2/2).
prim(my_toupper3/2).
prim(my_flatten4/2).
prim(my_len5/2).
prim(my_tail6/2).
prim(my_reverse7/2).
prim(my_uppercase9/1).
prim(my_pred10/2).
prim(my_min_list11/2).
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
p([[5,2,1,4],[0,4,1]],[[7,4,3,6],[2,6,3]]).
p([[4,4,3],[7,5,0,5],[0,1,6]],[[6,6,5],[9,7,2,7],[2,3,8]]).
p([[1,4,2,0],[2,3,6],[3,2,7],[6,0,2]],[[3,6,4,2],[4,5,8],[5,4,9],[8,2,4]]).
p([[4,0,2,3],[7,3,7],[7,2,1],[5,5,5]],[[6,2,4,5],[9,5,9],[9,4,3],[7,7,7]]).
p([[6,6,7,5],[4,0,1],[6,4,3]],[[8,8,9,7],[6,2,3],[8,6,5]]).
q([[3,7,6],[4,1,6,7],[5,4,3]],[[5,9,8],[4,1,6,7],[7,6,5]]).
q([[1,2,4,7],[5,7,0],[2,3,1,1],[1,1,0,2]],[[3,4,6,9],[5,7,0],[4,5,3,3],[1,1,0,2]]).
q([[5,1,5,2],[4,5,5,0],[7,5,7,5]],[[7,3,7,4],[6,7,7,2],[7,5,7,5]]).
q([[7,2,6],[6,3,7],[4,2,1,2]],[[7,2,6],[8,5,9],[6,4,3,4]]).
q([[7,6,1],[7,1,7],[5,4,7],[6,7,6]],[[9,8,3],[7,1,7],[7,6,9],[6,7,6]]).
