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
my_pred2(A,B):-succ(B,A),A > 0.
my_max_list3(A,B):-max_list(A,B).
my_len4(A,B):-length(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_toupper7(A,B):-upcase_atom(A,B),char_code(A,_).
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

my_tail11([_|TL],TL).
my_min_list12(A,B):-min_list(A,B).
my_reverse13(A,B):-reverse(A,B).
my_flatten14(A,B):-flatten(A,B).
my_head15([H|_],H).
my_uppercase16(A):-upcase_atom(A,A),char_code(A,_).
my_even17(A):-0 is A mod 2.
my_odd18(A):-1 is A mod 2.
my_element19(A,B):-member(B,A).
my_double20(N,M):-M is 2*N,M =< 10.
my_lowercase21(A):-downcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_pred2/2).
prim(my_max_list3/2).
prim(my_len4/2).
prim(my_list_to_set5/2).
prim(my_sumlist6/2).
prim(my_toupper7/2).
prim(my_msort8/2).
prim(my_tolower9/2).
prim(my_tail11/2).
prim(my_min_list12/2).
prim(my_reverse13/2).
prim(my_flatten14/2).
prim(my_head15/2).
prim(my_uppercase16/1).
prim(my_even17/1).
prim(my_odd18/1).
prim(my_element19/2).
prim(my_double20/2).
prim(my_lowercase21/1).
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
p([[3,0,4,6],[4,2,0],[1,3,5,6],[4,2,6]],[[5,2,6,8],[6,4,2],[3,5,7,8],[6,4,8]]).
p([[5,3,7,7],[0,4,0]],[[7,5,9,9],[2,6,2]]).
p([[1,3,4,1],[7,5,6,7],[3,4,4,0],[5,1,7,7]],[[3,5,6,3],[9,7,8,9],[5,6,6,2],[7,3,9,9]]).
p([[5,5,7,5],[5,7,1],[5,2,4,7]],[[7,7,9,7],[7,9,3],[7,4,6,9]]).
p([[3,4,6,5],[2,6,6]],[[5,6,8,7],[4,8,8]]).
q([[6,0,2,5],[3,2,0],[1,6,7,7]],[[8,2,4,7],[3,2,0],[3,8,9,9]]).
q([[1,0,1,0],[2,1,3,5],[4,2,2]],[[3,2,3,2],[4,3,5,7],[4,2,2]]).
q([[5,0,1,3],[0,3,0],[5,0,1],[6,2,7,7]],[[7,2,3,5],[2,5,2],[5,0,1],[8,4,9,9]]).
q([[3,7,2],[3,5,5,2],[3,3,3,6],[0,3,1]],[[3,7,2],[5,7,7,4],[5,5,5,8],[0,3,1]]).
q([[3,2,5,1],[2,1,6,2],[0,1,4,4]],[[3,2,5,1],[4,3,8,4],[2,3,6,6]]).
