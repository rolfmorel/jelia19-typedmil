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
my_flatten2(A,B):-flatten(A,B).
my_tolower3(A,B):-downcase_atom(A,B),char_code(A,_).
my_last4(A,B):-last(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_reverse6(A,B):-reverse(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_uppercase8(A):-upcase_atom(A,A),char_code(A,_).
my_min_list9(A,B):-min_list(A,B).
my_even10(A):-0 is A mod 2.
my_head11([H|_],H).
my_set12(A):-list_to_set(A,A).
my_element13(A,B):-member(B,A).

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

my_max_list15(A,B):-max_list(A,B).
my_lowercase16(A):-downcase_atom(A,A),char_code(A,_).
my_len17(A,B):-length(A,B).
prim(my_succ1/2).
prim(my_flatten2/2).
prim(my_tolower3/2).
prim(my_last4/2).
prim(my_list_to_set5/2).
prim(my_reverse6/2).
prim(my_sumlist7/2).
prim(my_uppercase8/1).
prim(my_min_list9/2).
prim(my_even10/1).
prim(my_head11/2).
prim(my_set12/1).
prim(my_element13/2).
prim(my_max_list15/2).
prim(my_lowercase16/1).
prim(my_len17/2).
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
p([[3,4,7],[0,0,1,7]],[[5,6,9],[2,2,3,9]]).
p([[4,4,1,3],[2,6,7,3],[7,3,7],[2,1,4]],[[6,6,3,5],[4,8,9,5],[9,5,9],[4,3,6]]).
p([[7,6,1],[6,7,6],[3,1,5]],[[9,8,3],[8,9,8],[5,3,7]]).
p([[5,5,1,7],[6,2,4,6],[2,7,2,4],[6,5,0,7]],[[7,7,3,9],[8,4,6,8],[4,9,4,6],[8,7,2,9]]).
p([[4,2,3,7],[1,2,2,5]],[[6,4,5,9],[3,4,4,7]]).
q([[7,5,6,2],[5,4,1,7],[3,5,1]],[[9,7,8,4],[7,6,3,9],[3,5,1]]).
q([[2,6,6],[4,6,2],[6,7,2,3],[4,0,4,4]],[[4,8,8],[6,8,4],[6,7,2,3],[6,2,6,6]]).
q([[2,4,7],[3,2,7,5],[3,0,2,7],[3,4,6,4]],[[4,6,9],[3,2,7,5],[5,2,4,9],[3,4,6,4]]).
q([[0,1,1],[1,0,0,5]],[[2,3,3],[1,0,0,5]]).
q([[5,0,0],[4,4,4],[3,2,2,4]],[[7,2,2],[4,4,4],[5,4,4,6]]).
