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
my_head2([H|_],H).
my_min_list3(A,B):-min_list(A,B).
my_last4(A,B):-last(A,B).
my_odd5(A):-1 is A mod 2.
my_lowercase6(A):-downcase_atom(A,A),char_code(A,_).
my_len7(A,B):-length(A,B).
my_uppercase8(A):-upcase_atom(A,A),char_code(A,_).
my_sumlist9(A,B):-sumlist(A,B).
my_reverse10(A,B):-reverse(A,B).
my_flatten11(A,B):-flatten(A,B).
my_element12(A,B):-member(B,A).
my_tail13([_|TL],TL).
my_msort14(A,B):-msort(A,B).
my_tolower15(A,B):-downcase_atom(A,B),char_code(A,_).
my_even16(A):-0 is A mod 2.

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

my_set18(A):-list_to_set(A,A).
prim(my_succ1/2).
prim(my_head2/2).
prim(my_min_list3/2).
prim(my_last4/2).
prim(my_odd5/1).
prim(my_lowercase6/1).
prim(my_len7/2).
prim(my_uppercase8/1).
prim(my_sumlist9/2).
prim(my_reverse10/2).
prim(my_flatten11/2).
prim(my_element12/2).
prim(my_tail13/2).
prim(my_msort14/2).
prim(my_tolower15/2).
prim(my_even16/1).
prim(my_set18/1).
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
p([[7,1,1,7],[3,7,1]],[[9,3,3,9],[5,9,3]]).
p([[6,3,4,2],[3,7,5,5],[5,4,4],[1,5,2]],[[8,5,6,4],[5,9,7,7],[7,6,6],[3,7,4]]).
p([[7,2,7,6],[5,5,5,0],[6,0,2,4],[2,4,3,6]],[[9,4,9,8],[7,7,7,2],[8,2,4,6],[4,6,5,8]]).
p([[7,1,1],[6,6,1,0],[0,7,5,0]],[[9,3,3],[8,8,3,2],[2,9,7,2]]).
p([[1,3,1],[0,0,7]],[[3,5,3],[2,2,9]]).
q([[0,2,2,6],[7,2,2,2]],[[2,4,4,8],[7,2,2,2]]).
q([[3,0,3],[4,7,7,2]],[[3,0,3],[6,9,9,4]]).
q([[7,6,2],[6,4,4]],[[7,6,2],[8,6,6]]).
q([[4,3,7,7],[0,7,6,2],[0,2,4]],[[6,5,9,9],[2,9,8,4],[0,2,4]]).
q([[5,3,1],[1,4,6],[2,4,0],[3,7,4]],[[7,5,3],[1,4,6],[4,6,2],[5,9,6]]).
