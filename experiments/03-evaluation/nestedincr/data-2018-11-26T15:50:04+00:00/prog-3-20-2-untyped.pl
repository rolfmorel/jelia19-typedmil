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
my_tolower2(A,B):-downcase_atom(A,B),char_code(A,_).
my_reverse3(A,B):-reverse(A,B).
my_len4(A,B):-length(A,B).
my_set5(A):-list_to_set(A,A).
my_msort6(A,B):-msort(A,B).
my_head7([H|_],H).

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

my_odd9(A):-1 is A mod 2.
my_element10(A,B):-member(B,A).
my_pred11(A,B):-succ(B,A),A > 0.
my_list_to_set12(A,B):-list_to_set(A,B).
my_min_list13(A,B):-min_list(A,B).
my_uppercase14(A):-upcase_atom(A,A),char_code(A,_).
my_tail15([_|TL],TL).
my_sumlist16(A,B):-sumlist(A,B).
my_lowercase17(A):-downcase_atom(A,A),char_code(A,_).
my_flatten18(A,B):-flatten(A,B).
my_last19(A,B):-last(A,B).
my_toupper20(A,B):-upcase_atom(A,B),char_code(A,_).
my_even21(A):-0 is A mod 2.
prim(my_succ1/2).
prim(my_tolower2/2).
prim(my_reverse3/2).
prim(my_len4/2).
prim(my_set5/1).
prim(my_msort6/2).
prim(my_head7/2).
prim(my_odd9/1).
prim(my_element10/2).
prim(my_pred11/2).
prim(my_list_to_set12/2).
prim(my_min_list13/2).
prim(my_uppercase14/1).
prim(my_tail15/2).
prim(my_sumlist16/2).
prim(my_lowercase17/1).
prim(my_flatten18/2).
prim(my_last19/2).
prim(my_toupper20/2).
prim(my_even21/1).
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
p([[3,6,7,2],[3,1,1],[2,6,6,6]],[[5,8,9,4],[5,3,3],[4,8,8,8]]).
p([[3,1,4],[7,5,7,6],[3,5,5,2],[0,5,6,0]],[[5,3,6],[9,7,9,8],[5,7,7,4],[2,7,8,2]]).
p([[7,7,7,2],[3,4,7],[0,3,4,4]],[[9,9,9,4],[5,6,9],[2,5,6,6]]).
p([[4,6,2],[5,1,3,7],[7,4,3],[0,4,2,0]],[[6,8,4],[7,3,5,9],[9,6,5],[2,6,4,2]]).
p([[1,4,3,7],[0,0,0],[3,6,4]],[[3,6,5,9],[2,2,2],[5,8,6]]).
q([[4,2,0,6],[4,2,4]],[[6,4,2,8],[4,2,4]]).
q([[5,2,0],[0,7,0,1],[1,0,1,2],[3,7,0,0]],[[5,2,0],[0,7,0,1],[3,2,3,4],[5,9,2,2]]).
q([[1,6,3],[6,2,7]],[[1,6,3],[8,4,9]]).
q([[0,3,6],[4,4,6,2]],[[2,5,8],[4,4,6,2]]).
q([[4,1,7,2],[4,2,7,1],[1,3,0,1],[1,4,7,6]],[[6,3,9,4],[6,4,9,3],[1,3,0,1],[1,4,7,6]]).
