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
my_msort3(A,B):-msort(A,B).
my_set4(A):-list_to_set(A,A).
my_toupper5(A,B):-upcase_atom(A,B),char_code(A,_).
my_uppercase6(A):-upcase_atom(A,A),char_code(A,_).
my_tail7([_|TL],TL).
my_list_to_set8(A,B):-list_to_set(A,B).
my_head9([H|_],H).
my_double10(N,M):-M is 2*N,M =< 10.
my_max_list11(A,B):-max_list(A,B).
my_flatten12(A,B):-flatten(A,B).
my_tolower13(A,B):-downcase_atom(A,B),char_code(A,_).
my_element14(A,B):-member(B,A).
my_even15(A):-0 is A mod 2.
my_pred16(A,B):-succ(B,A),A > 0.

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

my_reverse18(A,B):-reverse(A,B).
my_len19(A,B):-length(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_last21(A,B):-last(A,B).
my_lowercase22(A):-downcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_min_list2/2).
prim(my_msort3/2).
prim(my_set4/1).
prim(my_toupper5/2).
prim(my_uppercase6/1).
prim(my_tail7/2).
prim(my_list_to_set8/2).
prim(my_head9/2).
prim(my_double10/2).
prim(my_max_list11/2).
prim(my_flatten12/2).
prim(my_tolower13/2).
prim(my_element14/2).
prim(my_even15/1).
prim(my_pred16/2).
prim(my_reverse18/2).
prim(my_len19/2).
prim(my_sumlist20/2).
prim(my_last21/2).
prim(my_lowercase22/1).
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
p([[6,0,6],[7,2,3]],[[8,2,8],[9,4,5]]).
p([[2,3,4,2],[1,1,6],[0,0,5,6]],[[4,5,6,4],[3,3,8],[2,2,7,8]]).
p([[1,1,0,2],[6,6,2],[4,1,1,1],[3,1,1,3]],[[3,3,2,4],[8,8,4],[6,3,3,3],[5,3,3,5]]).
p([[5,0,6,7],[2,4,1,5],[5,5,3],[1,0,3,5]],[[7,2,8,9],[4,6,3,7],[7,7,5],[3,2,5,7]]).
p([[7,5,6,3],[6,0,3,3],[1,3,1],[5,6,6]],[[9,7,8,5],[8,2,5,5],[3,5,3],[7,8,8]]).
q([[3,1,0,4],[1,6,5,6],[1,7,7],[4,6,7,6]],[[5,3,2,6],[3,8,7,8],[1,7,7],[6,8,9,8]]).
q([[7,3,2],[3,0,5],[5,3,1,4]],[[9,5,4],[5,2,7],[5,3,1,4]]).
q([[2,3,6],[1,0,2],[5,0,2]],[[4,5,8],[1,0,2],[7,2,4]]).
q([[4,2,6],[5,5,6],[6,4,6]],[[6,4,8],[5,5,6],[8,6,8]]).
q([[3,6,1],[7,0,0],[0,7,4],[7,7,1,5]],[[3,6,1],[9,2,2],[2,9,6],[7,7,1,5]]).
