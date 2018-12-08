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
my_double2(N,M):-M is 2*N,M =< 10.
my_len3(A,B):-length(A,B).
my_even4(A):-0 is A mod 2.
my_reverse5(A,B):-reverse(A,B).
my_head6([H|_],H).
my_min_list7(A,B):-min_list(A,B).
my_msort8(A,B):-msort(A,B).
my_tolower9(A,B):-downcase_atom(A,B),char_code(A,_).

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

my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).
my_element12(A,B):-member(B,A).
my_tail13([_|TL],TL).
my_odd14(A):-1 is A mod 2.
my_pred15(A,B):-succ(B,A),A > 0.
my_max_list16(A,B):-max_list(A,B).
my_last17(A,B):-last(A,B).
my_flatten18(A,B):-flatten(A,B).
my_toupper19(A,B):-upcase_atom(A,B),char_code(A,_).
my_sumlist20(A,B):-sumlist(A,B).
my_lowercase21(A):-downcase_atom(A,A),char_code(A,_).
my_set22(A):-list_to_set(A,A).
prim(my_succ1/2).
prim(my_double2/2).
prim(my_len3/2).
prim(my_even4/1).
prim(my_reverse5/2).
prim(my_head6/2).
prim(my_min_list7/2).
prim(my_msort8/2).
prim(my_tolower9/2).
prim(my_uppercase11/1).
prim(my_element12/2).
prim(my_tail13/2).
prim(my_odd14/1).
prim(my_pred15/2).
prim(my_max_list16/2).
prim(my_last17/2).
prim(my_flatten18/2).
prim(my_toupper19/2).
prim(my_sumlist20/2).
prim(my_lowercase21/1).
prim(my_set22/1).
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
p([[3,2,2,4],[0,7,0]],[[5,4,4,6],[2,9,2]]).
p([[2,3,0,2],[5,2,3],[5,5,0,4]],[[4,5,2,4],[7,4,5],[7,7,2,6]]).
p([[7,1,5,6],[4,4,4]],[[9,3,7,8],[6,6,6]]).
p([[2,2,1,4],[6,2,4]],[[4,4,3,6],[8,4,6]]).
p([[5,7,7,2],[0,6,0],[4,6,7,2]],[[7,9,9,4],[2,8,2],[6,8,9,4]]).
q([[4,0,6],[6,2,0],[5,5,5,5],[0,0,1,5]],[[6,2,8],[8,4,2],[7,7,7,7],[0,0,1,5]]).
q([[7,2,1,3],[1,7,0,3],[3,3,1,4]],[[7,2,1,3],[3,9,2,5],[5,5,3,6]]).
q([[2,2,1],[7,7,1,5],[2,1,2]],[[4,4,3],[9,9,3,7],[2,1,2]]).
q([[4,5,7,6],[0,5,5,0],[4,1,3]],[[4,5,7,6],[2,7,7,2],[6,3,5]]).
q([[4,2,5],[4,0,2,3]],[[6,4,7],[4,0,2,3]]).
