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
my_last2(A,B):-last(A,B).
my_tail3([_|TL],TL).
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_uppercase5(A):-upcase_atom(A,A),char_code(A,_).
my_msort6(A,B):-msort(A,B).
my_element7(A,B):-member(B,A).
my_odd8(A):-1 is A mod 2.
my_flatten9(A,B):-flatten(A,B).
my_len10(A,B):-length(A,B).
my_toupper11(A,B):-upcase_atom(A,B),char_code(A,_).
my_tolower12(A,B):-downcase_atom(A,B),char_code(A,_).
my_min_list13(A,B):-min_list(A,B).
my_double14(N,M):-M is 2*N,M =< 10.
my_max_list15(A,B):-max_list(A,B).

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

my_reverse17(A,B):-reverse(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_list_to_set19(A,B):-list_to_set(A,B).
my_set20(A):-list_to_set(A,A).
prim(my_succ1/2).
prim(my_last2/2).
prim(my_tail3/2).
prim(my_lowercase4/1).
prim(my_uppercase5/1).
prim(my_msort6/2).
prim(my_element7/2).
prim(my_odd8/1).
prim(my_flatten9/2).
prim(my_len10/2).
prim(my_toupper11/2).
prim(my_tolower12/2).
prim(my_min_list13/2).
prim(my_double14/2).
prim(my_max_list15/2).
prim(my_reverse17/2).
prim(my_sumlist18/2).
prim(my_list_to_set19/2).
prim(my_set20/1).
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
p([[6,6,6],[5,7,2,0],[3,0,6]],[[8,8,8],[7,9,4,2],[5,2,8]]).
p([[2,6,2],[2,5,0,2]],[[4,8,4],[4,7,2,4]]).
p([[4,0,7,4],[5,2,7],[6,1,1]],[[6,2,9,6],[7,4,9],[8,3,3]]).
p([[3,7,3],[0,3,4],[0,4,7,1],[4,0,5,3]],[[5,9,5],[2,5,6],[2,6,9,3],[6,2,7,5]]).
p([[4,7,0,1],[3,6,5],[1,0,0,0]],[[6,9,2,3],[5,8,7],[3,2,2,2]]).
q([[0,6,0,3],[1,0,7,3],[0,5,6],[7,3,5]],[[0,6,0,3],[3,2,9,5],[2,7,8],[9,5,7]]).
q([[2,1,4,1],[1,5,0]],[[4,3,6,3],[1,5,0]]).
q([[7,4,3,1],[4,6,6,6]],[[7,4,3,1],[6,8,8,8]]).
q([[4,3,2],[2,7,0,1],[4,0,3]],[[4,3,2],[4,9,2,3],[6,2,5]]).
q([[1,2,2],[0,2,3,1],[2,2,7],[3,4,0]],[[1,2,2],[0,2,3,1],[4,4,9],[5,6,2]]).
