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
my_pred3(A,B):-succ(B,A),A > 0.
my_odd4(A):-1 is A mod 2.
my_min_list5(A,B):-min_list(A,B).
my_head6([H|_],H).
my_sumlist7(A,B):-sumlist(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).

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

my_lowercase10(A):-downcase_atom(A,A),char_code(A,_).
my_double11(N,M):-M is 2*N,M =< 10.
my_max_list12(A,B):-max_list(A,B).
my_msort13(A,B):-msort(A,B).
my_even14(A):-0 is A mod 2.
my_toupper15(A,B):-upcase_atom(A,B),char_code(A,_).
my_set16(A):-list_to_set(A,A).
my_tolower17(A,B):-downcase_atom(A,B),char_code(A,_).
my_len18(A,B):-length(A,B).
my_tail19([_|TL],TL).
prim(my_succ1/2).
prim(my_reverse2/2).
prim(my_pred3/2).
prim(my_odd4/1).
prim(my_min_list5/2).
prim(my_head6/2).
prim(my_sumlist7/2).
prim(my_list_to_set8/2).
prim(my_lowercase10/1).
prim(my_double11/2).
prim(my_max_list12/2).
prim(my_msort13/2).
prim(my_even14/1).
prim(my_toupper15/2).
prim(my_set16/1).
prim(my_tolower17/2).
prim(my_len18/2).
prim(my_tail19/2).
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
p([[5,4,7],[3,0,3,4],[2,5,1],[5,3,3,5]],[[7,6,9],[5,2,5,6],[4,7,3],[7,5,5,7]]).
p([[4,0,6,4],[1,6,6,0],[4,4,4,2],[2,5,2,5]],[[6,2,8,6],[3,8,8,2],[6,6,6,4],[4,7,4,7]]).
p([[0,3,5],[0,3,2,2],[7,3,5],[7,4,4]],[[2,5,7],[2,5,4,4],[9,5,7],[9,6,6]]).
p([[5,7,6,0],[2,7,7],[4,1,7,6]],[[7,9,8,2],[4,9,9],[6,3,9,8]]).
p([[1,0,6],[6,5,3,5],[6,5,5,2]],[[3,2,8],[8,7,5,7],[8,7,7,4]]).
q([[0,0,5],[4,2,5,4]],[[2,2,7],[4,2,5,4]]).
q([[5,1,0],[4,5,0]],[[7,3,2],[4,5,0]]).
q([[4,1,1],[3,1,1,5]],[[4,1,1],[5,3,3,7]]).
q([[3,6,6,1],[7,0,1]],[[5,8,8,3],[7,0,1]]).
q([[0,6,6],[5,0,7],[7,5,1],[3,5,1]],[[0,6,6],[7,2,9],[9,7,3],[5,7,3]]).
