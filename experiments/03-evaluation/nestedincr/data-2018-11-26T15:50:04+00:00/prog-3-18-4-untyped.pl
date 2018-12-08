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
my_odd3(A):-1 is A mod 2.
my_toupper4(A,B):-upcase_atom(A,B),char_code(A,_).
my_last5(A,B):-last(A,B).
my_tolower6(A,B):-downcase_atom(A,B),char_code(A,_).
my_lowercase7(A):-downcase_atom(A,A),char_code(A,_).

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

my_uppercase9(A):-upcase_atom(A,A),char_code(A,_).
my_len10(A,B):-length(A,B).
my_reverse11(A,B):-reverse(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_set13(A):-list_to_set(A,A).
my_max_list14(A,B):-max_list(A,B).
my_tail15([_|TL],TL).
my_msort16(A,B):-msort(A,B).
my_head17([H|_],H).
my_element18(A,B):-member(B,A).
my_sumlist19(A,B):-sumlist(A,B).
prim(my_succ1/2).
prim(my_double2/2).
prim(my_odd3/1).
prim(my_toupper4/2).
prim(my_last5/2).
prim(my_tolower6/2).
prim(my_lowercase7/1).
prim(my_uppercase9/1).
prim(my_len10/2).
prim(my_reverse11/2).
prim(my_pred12/2).
prim(my_set13/1).
prim(my_max_list14/2).
prim(my_tail15/2).
prim(my_msort16/2).
prim(my_head17/2).
prim(my_element18/2).
prim(my_sumlist19/2).
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
p([[1,5,0,3],[2,6,1,3]],[[3,7,2,5],[4,8,3,5]]).
p([[7,1,2],[5,7,2,7],[1,2,1,0]],[[9,3,4],[7,9,4,9],[3,4,3,2]]).
p([[1,3,2],[3,6,4],[7,0,0,3],[7,2,5]],[[3,5,4],[5,8,6],[9,2,2,5],[9,4,7]]).
p([[0,5,4,1],[1,3,7]],[[2,7,6,3],[3,5,9]]).
p([[0,5,1,6],[6,0,6,3],[4,5,0],[5,4,1,0]],[[2,7,3,8],[8,2,8,5],[6,7,2],[7,6,3,2]]).
q([[7,3,6,1],[5,3,4],[0,6,6],[2,1,6]],[[7,3,6,1],[7,5,6],[2,8,8],[4,3,8]]).
q([[1,7,5,6],[1,0,4,0],[3,3,7]],[[3,9,7,8],[3,2,6,2],[3,3,7]]).
q([[7,7,2,1],[2,4,3,2],[3,4,4,4]],[[7,7,2,1],[4,6,5,4],[5,6,6,6]]).
q([[0,7,7],[5,1,2],[4,2,5],[2,5,1,2]],[[2,9,9],[5,1,2],[6,4,7],[4,7,3,4]]).
q([[6,7,1,6],[3,3,0,7]],[[6,7,1,6],[5,5,2,9]]).
