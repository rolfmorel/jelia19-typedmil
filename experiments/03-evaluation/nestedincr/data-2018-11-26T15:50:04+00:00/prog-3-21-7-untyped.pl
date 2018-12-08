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
my_set2(A):-list_to_set(A,A).
my_flatten3(A,B):-flatten(A,B).
my_tolower4(A,B):-downcase_atom(A,B),char_code(A,_).
my_uppercase5(A):-upcase_atom(A,A),char_code(A,_).
my_min_list6(A,B):-min_list(A,B).
my_tail7([_|TL],TL).

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

my_head9([H|_],H).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_max_list11(A,B):-max_list(A,B).
my_element12(A,B):-member(B,A).
my_len13(A,B):-length(A,B).
my_list_to_set14(A,B):-list_to_set(A,B).
my_lowercase15(A):-downcase_atom(A,A),char_code(A,_).
my_last16(A,B):-last(A,B).
my_double17(N,M):-M is 2*N,M =< 10.
my_odd18(A):-1 is A mod 2.
my_sumlist19(A,B):-sumlist(A,B).
my_pred20(A,B):-succ(B,A),A > 0.
my_msort21(A,B):-msort(A,B).
my_even22(A):-0 is A mod 2.
prim(my_succ1/2).
prim(my_set2/1).
prim(my_flatten3/2).
prim(my_tolower4/2).
prim(my_uppercase5/1).
prim(my_min_list6/2).
prim(my_tail7/2).
prim(my_head9/2).
prim(my_toupper10/2).
prim(my_max_list11/2).
prim(my_element12/2).
prim(my_len13/2).
prim(my_list_to_set14/2).
prim(my_lowercase15/1).
prim(my_last16/2).
prim(my_double17/2).
prim(my_odd18/1).
prim(my_sumlist19/2).
prim(my_pred20/2).
prim(my_msort21/2).
prim(my_even22/1).
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
p([[2,7,4],[5,6,5,3]],[[4,9,6],[7,8,7,5]]).
p([[2,4,3],[4,6,7],[7,2,1]],[[4,6,5],[6,8,9],[9,4,3]]).
p([[3,3,7,3],[3,5,5]],[[5,5,9,5],[5,7,7]]).
p([[1,0,4,7],[0,3,3],[1,0,3,4]],[[3,2,6,9],[2,5,5],[3,2,5,6]]).
p([[3,0,6,1],[1,7,1]],[[5,2,8,3],[3,9,3]]).
q([[2,6,5],[2,4,4]],[[2,6,5],[4,6,6]]).
q([[2,2,3],[6,1,7,3]],[[2,2,3],[8,3,9,5]]).
q([[5,3,2,3],[6,5,4,2]],[[7,5,4,5],[6,5,4,2]]).
q([[0,3,5,1],[3,7,4,1],[0,2,2],[5,5,3]],[[0,3,5,1],[3,7,4,1],[2,4,4],[7,7,5]]).
q([[1,2,2,7],[3,3,4],[5,5,4,3],[3,0,4]],[[3,4,4,9],[3,3,4],[7,7,6,5],[3,0,4]]).
