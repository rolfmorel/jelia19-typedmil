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
my_min_list2(A,B):-min_list(A,B).
my_flatten3(A,B):-flatten(A,B).
my_element4(A,B):-member(B,A).
my_odd5(A):-1 is A mod 2.
my_lowercase6(A):-downcase_atom(A,A),char_code(A,_).

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

my_set8(A):-list_to_set(A,A).
my_reverse9(A,B):-reverse(A,B).
my_tolower10(A,B):-downcase_atom(A,B),char_code(A,_).
my_even11(A):-0 is A mod 2.
my_pred12(A,B):-succ(B,A),A > 0.
my_len13(A,B):-length(A,B).
prim(my_succ1/2).
prim(my_min_list2/2).
prim(my_flatten3/2).
prim(my_element4/2).
prim(my_odd5/1).
prim(my_lowercase6/1).
prim(my_set8/1).
prim(my_reverse9/2).
prim(my_tolower10/2).
prim(my_even11/1).
prim(my_pred12/2).
prim(my_len13/2).
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
p([[4,4,7,3],[7,1,0],[2,5,7,7]],[[6,6,9,5],[9,3,2],[4,7,9,9]]).
p([[2,6,1,3],[1,1,0,1]],[[4,8,3,5],[3,3,2,3]]).
p([[7,3,6],[1,6,1,7],[2,3,2]],[[9,5,8],[3,8,3,9],[4,5,4]]).
p([[1,1,2,4],[1,2,4]],[[3,3,4,6],[3,4,6]]).
p([[2,3,2,4],[2,6,3,1],[2,7,4]],[[4,5,4,6],[4,8,5,3],[4,9,6]]).
q([[4,0,2,6],[0,3,2],[6,7,7,4],[7,5,5,7]],[[6,2,4,8],[2,5,4],[6,7,7,4],[7,5,5,7]]).
q([[7,2,6,4],[4,0,3],[0,6,0],[5,4,3]],[[9,4,8,6],[6,2,5],[0,6,0],[5,4,3]]).
q([[2,5,3],[7,0,4]],[[2,5,3],[9,2,6]]).
q([[4,6,6,4],[5,7,6],[4,0,4,3]],[[6,8,8,6],[5,7,6],[6,2,6,5]]).
q([[4,4,7,4],[2,4,5],[2,1,2,0]],[[6,6,9,6],[2,4,5],[4,3,4,2]]).
