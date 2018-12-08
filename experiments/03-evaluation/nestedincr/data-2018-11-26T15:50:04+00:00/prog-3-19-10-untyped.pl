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
my_list_to_set3(A,B):-list_to_set(A,B).
my_max_list4(A,B):-max_list(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
my_toupper6(A,B):-upcase_atom(A,B),char_code(A,_).
my_len7(A,B):-length(A,B).
my_last8(A,B):-last(A,B).
my_lowercase9(A):-downcase_atom(A,A),char_code(A,_).
my_flatten10(A,B):-flatten(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_head12([H|_],H).
my_odd13(A):-1 is A mod 2.

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

my_tolower15(A,B):-downcase_atom(A,B),char_code(A,_).
my_tail16([_|TL],TL).
my_reverse17(A,B):-reverse(A,B).
my_element18(A,B):-member(B,A).
my_min_list19(A,B):-min_list(A,B).
my_uppercase20(A):-upcase_atom(A,A),char_code(A,_).
prim(my_succ1/2).
prim(my_double2/2).
prim(my_list_to_set3/2).
prim(my_max_list4/2).
prim(my_pred5/2).
prim(my_toupper6/2).
prim(my_len7/2).
prim(my_last8/2).
prim(my_lowercase9/1).
prim(my_flatten10/2).
prim(my_sumlist11/2).
prim(my_head12/2).
prim(my_odd13/1).
prim(my_tolower15/2).
prim(my_tail16/2).
prim(my_reverse17/2).
prim(my_element18/2).
prim(my_min_list19/2).
prim(my_uppercase20/1).
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
p([[1,4,2],[3,6,0],[7,5,3]],[[3,6,4],[5,8,2],[9,7,5]]).
p([[7,5,3],[2,1,2]],[[9,7,5],[4,3,4]]).
p([[4,4,1],[5,2,0]],[[6,6,3],[7,4,2]]).
p([[2,0,6],[7,7,2],[1,1,0,2],[2,5,6]],[[4,2,8],[9,9,4],[3,3,2,4],[4,7,8]]).
p([[0,1,5,7],[6,4,6,1],[0,1,1],[5,2,1,4]],[[2,3,7,9],[8,6,8,3],[2,3,3],[7,4,3,6]]).
q([[0,4,2,0],[3,7,0]],[[2,6,4,2],[3,7,0]]).
q([[4,1,7],[2,3,6,1],[0,4,5,4],[6,1,3,3]],[[6,3,9],[4,5,8,3],[0,4,5,4],[8,3,5,5]]).
q([[6,5,5],[2,5,3]],[[6,5,5],[4,7,5]]).
q([[6,3,4],[0,3,7,0],[7,2,1,0],[2,1,0]],[[6,3,4],[2,5,9,2],[9,4,3,2],[4,3,2]]).
q([[7,6,4],[0,1,0],[5,7,7],[7,6,7,2]],[[9,8,6],[0,1,0],[7,9,9],[9,8,9,4]]).
