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
my_len2(A,B):-length(A,B).
my_list_to_set3(A,B):-list_to_set(A,B).

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

my_msort5(A,B):-msort(A,B).
my_element6(A,B):-member(B,A).
my_odd7(A):-1 is A mod 2.
my_double8(N,M):-M is 2*N,M =< 10.
my_lowercase9(A):-downcase_atom(A,A),char_code(A,_).
my_set10(A):-list_to_set(A,A).
my_even11(A):-0 is A mod 2.
my_min_list12(A,B):-min_list(A,B).
my_tolower13(A,B):-downcase_atom(A,B),char_code(A,_).
my_head14([H|_],H).
my_toupper15(A,B):-upcase_atom(A,B),char_code(A,_).
my_reverse16(A,B):-reverse(A,B).
my_tail17([_|TL],TL).
my_last18(A,B):-last(A,B).
my_flatten19(A,B):-flatten(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_max_list21(A,B):-max_list(A,B).
my_uppercase22(A):-upcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_len2/2).
prim(my_list_to_set3/2).
prim(my_msort5/2).
prim(my_element6/2).
prim(my_odd7/1).
prim(my_double8/2).
prim(my_lowercase9/1).
prim(my_set10/1).
prim(my_even11/1).
prim(my_min_list12/2).
prim(my_tolower13/2).
prim(my_head14/2).
prim(my_toupper15/2).
prim(my_reverse16/2).
prim(my_tail17/2).
prim(my_last18/2).
prim(my_flatten19/2).
prim(my_sumlist20/2).
prim(my_max_list21/2).
prim(my_uppercase22/1).
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
p([[5,0,7,5],[6,4,7],[4,4,0,1],[5,1,6]],[[7,2,9,7],[8,6,9],[6,6,2,3],[7,3,8]]).
p([[6,7,0,7],[5,5,5],[5,2,5,7],[6,4,1,6]],[[8,9,2,9],[7,7,7],[7,4,7,9],[8,6,3,8]]).
p([[5,5,7,7],[7,2,0],[3,1,1,1]],[[7,7,9,9],[9,4,2],[5,3,3,3]]).
p([[5,4,2,6],[5,5,5,6],[3,7,5,3],[6,5,7,7]],[[7,6,4,8],[7,7,7,8],[5,9,7,5],[8,7,9,9]]).
p([[2,4,1],[2,5,0]],[[4,6,3],[4,7,2]]).
q([[3,4,4],[4,2,7],[7,4,7,7],[1,6,1,1]],[[3,4,4],[6,4,9],[9,6,9,9],[1,6,1,1]]).
q([[4,4,4,4],[4,3,2]],[[6,6,6,6],[4,3,2]]).
q([[5,5,6,4],[4,4,6,2],[1,5,5,4],[6,7,4]],[[5,5,6,4],[4,4,6,2],[3,7,7,6],[8,9,6]]).
q([[4,4,3],[0,5,6,5],[3,7,6],[0,0,1]],[[6,6,5],[2,7,8,7],[5,9,8],[0,0,1]]).
q([[7,4,3,5],[4,6,5,6],[7,0,7],[6,2,6,5]],[[9,6,5,7],[6,8,7,8],[7,0,7],[6,2,6,5]]).
