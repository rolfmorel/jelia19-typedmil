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
my_uppercase2(A):-upcase_atom(A,A),char_code(A,_).
my_double3(N,M):-M is 2*N,M =< 10.

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

my_head5([H|_],H).
my_msort6(A,B):-msort(A,B).
my_toupper7(A,B):-upcase_atom(A,B),char_code(A,_).
my_tolower8(A,B):-downcase_atom(A,B),char_code(A,_).
my_lowercase9(A):-downcase_atom(A,A),char_code(A,_).
my_odd10(A):-1 is A mod 2.
my_len11(A,B):-length(A,B).
my_flatten12(A,B):-flatten(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_set14(A):-list_to_set(A,A).
my_max_list15(A,B):-max_list(A,B).
my_reverse16(A,B):-reverse(A,B).
my_last17(A,B):-last(A,B).
my_min_list18(A,B):-min_list(A,B).
my_pred19(A,B):-succ(B,A),A > 0.
my_even20(A):-0 is A mod 2.
my_element21(A,B):-member(B,A).
prim(my_succ1/2).
prim(my_uppercase2/1).
prim(my_double3/2).
prim(my_head5/2).
prim(my_msort6/2).
prim(my_toupper7/2).
prim(my_tolower8/2).
prim(my_lowercase9/1).
prim(my_odd10/1).
prim(my_len11/2).
prim(my_flatten12/2).
prim(my_sumlist13/2).
prim(my_set14/1).
prim(my_max_list15/2).
prim(my_reverse16/2).
prim(my_last17/2).
prim(my_min_list18/2).
prim(my_pred19/2).
prim(my_even20/1).
prim(my_element21/2).
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
p([[5,3,5,7],[2,2,1,6],[7,4,3],[0,0,6]],[[7,5,7,9],[4,4,3,8],[9,6,5],[2,2,8]]).
p([[6,2,2],[1,7,7,5],[0,0,6],[1,5,7]],[[8,4,4],[3,9,9,7],[2,2,8],[3,7,9]]).
p([[0,3,6,3],[7,4,1],[2,2,0],[7,1,0,5]],[[2,5,8,5],[9,6,3],[4,4,2],[9,3,2,7]]).
p([[5,2,1],[7,1,5,3],[2,2,5]],[[7,4,3],[9,3,7,5],[4,4,7]]).
p([[0,7,5,3],[1,4,7,4]],[[2,9,7,5],[3,6,9,6]]).
q([[5,2,1],[6,1,2,5]],[[5,2,1],[8,3,4,7]]).
q([[1,6,1],[0,4,7,1]],[[3,8,3],[0,4,7,1]]).
q([[4,6,4],[7,0,3,3]],[[6,8,6],[7,0,3,3]]).
q([[3,1,5],[1,7,6,6]],[[3,1,5],[3,9,8,8]]).
q([[3,3,4,4],[0,0,6,3],[1,3,3],[5,6,0]],[[5,5,6,6],[0,0,6,3],[3,5,5],[5,6,0]]).
