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
my_toupper2(A,B):-upcase_atom(A,B),char_code(A,_).
my_len3(A,B):-length(A,B).
my_reverse4(A,B):-reverse(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_pred6(A,B):-succ(B,A),A > 0.
my_tail7([_|TL],TL).
my_element8(A,B):-member(B,A).
my_even9(A):-0 is A mod 2.
my_head10([H|_],H).
my_odd11(A):-1 is A mod 2.
my_flatten12(A,B):-flatten(A,B).
my_msort13(A,B):-msort(A,B).
my_list_to_set14(A,B):-list_to_set(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_lowercase16(A):-downcase_atom(A,A),char_code(A,_).
my_min_list17(A,B):-min_list(A,B).

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

my_tolower19(A,B):-downcase_atom(A,B),char_code(A,_).
my_max_list20(A,B):-max_list(A,B).
my_uppercase21(A):-upcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_toupper2/2).
prim(my_len3/2).
prim(my_reverse4/2).
prim(my_double5/2).
prim(my_pred6/2).
prim(my_tail7/2).
prim(my_element8/2).
prim(my_even9/1).
prim(my_head10/2).
prim(my_odd11/1).
prim(my_flatten12/2).
prim(my_msort13/2).
prim(my_list_to_set14/2).
prim(my_sumlist15/2).
prim(my_lowercase16/1).
prim(my_min_list17/2).
prim(my_tolower19/2).
prim(my_max_list20/2).
prim(my_uppercase21/1).
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
p([[4,1,0],[4,1,6,4],[4,2,4],[3,6,0]],[[6,3,2],[6,3,8,6],[6,4,6],[5,8,2]]).
p([[3,6,7,5],[2,6,5,1],[3,6,7]],[[5,8,9,7],[4,8,7,3],[5,8,9]]).
p([[7,4,1],[4,3,5]],[[9,6,3],[6,5,7]]).
p([[1,1,5,0],[3,0,5],[3,5,0],[5,4,2,3]],[[3,3,7,2],[5,2,7],[5,7,2],[7,6,4,5]]).
p([[0,3,4,1],[2,3,2],[2,6,5,3],[5,0,4]],[[2,5,6,3],[4,5,4],[4,8,7,5],[7,2,6]]).
q([[2,7,5],[3,3,6,3],[4,3,0,5]],[[2,7,5],[5,5,8,5],[6,5,2,7]]).
q([[1,3,0],[1,5,0],[4,7,6],[1,3,4,3]],[[1,3,0],[3,7,2],[4,7,6],[3,5,6,5]]).
q([[7,1,1],[4,0,0],[4,3,1],[6,4,4]],[[9,3,3],[4,0,0],[6,5,3],[6,4,4]]).
q([[2,0,4],[3,2,0],[6,5,5]],[[2,0,4],[5,4,2],[8,7,7]]).
q([[4,6,2],[4,5,6]],[[6,8,4],[4,5,6]]).
