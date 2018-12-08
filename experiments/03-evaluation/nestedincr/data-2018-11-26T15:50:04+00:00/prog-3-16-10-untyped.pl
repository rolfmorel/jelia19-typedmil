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
my_sumlist2(A,B):-sumlist(A,B).
my_even3(A):-0 is A mod 2.
my_set4(A):-list_to_set(A,A).
my_lowercase5(A):-downcase_atom(A,A),char_code(A,_).

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

my_odd7(A):-1 is A mod 2.
my_tail8([_|TL],TL).
my_min_list9(A,B):-min_list(A,B).
my_list_to_set10(A,B):-list_to_set(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_toupper12(A,B):-upcase_atom(A,B),char_code(A,_).
my_tolower13(A,B):-downcase_atom(A,B),char_code(A,_).
my_head14([H|_],H).
my_uppercase15(A):-upcase_atom(A,A),char_code(A,_).
my_msort16(A,B):-msort(A,B).
my_element17(A,B):-member(B,A).
prim(my_succ1/2).
prim(my_sumlist2/2).
prim(my_even3/1).
prim(my_set4/1).
prim(my_lowercase5/1).
prim(my_odd7/1).
prim(my_tail8/2).
prim(my_min_list9/2).
prim(my_list_to_set10/2).
prim(my_pred11/2).
prim(my_toupper12/2).
prim(my_tolower13/2).
prim(my_head14/2).
prim(my_uppercase15/1).
prim(my_msort16/2).
prim(my_element17/2).
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
p([[0,6,4,5],[3,0,5,3],[4,1,0]],[[2,8,6,7],[5,2,7,5],[6,3,2]]).
p([[4,2,1,7],[4,1,3],[3,0,1,0]],[[6,4,3,9],[6,3,5],[5,2,3,2]]).
p([[6,5,6],[4,2,5],[3,6,7],[1,3,3,5]],[[8,7,8],[6,4,7],[5,8,9],[3,5,5,7]]).
p([[7,4,4],[5,7,2]],[[9,6,6],[7,9,4]]).
p([[7,6,2,4],[7,5,2],[7,2,2]],[[9,8,4,6],[9,7,4],[9,4,4]]).
q([[1,1,2],[7,0,6,4]],[[3,3,4],[7,0,6,4]]).
q([[1,0,4],[5,1,7],[5,4,0,7]],[[1,0,4],[7,3,9],[7,6,2,9]]).
q([[3,5,4,0],[4,3,5,0]],[[3,5,4,0],[6,5,7,2]]).
q([[5,2,1],[3,1,7],[1,2,1],[7,2,5]],[[5,2,1],[5,3,9],[3,4,3],[9,4,7]]).
q([[3,2,7],[0,3,7,0],[1,7,4,6]],[[5,4,9],[2,5,9,2],[1,7,4,6]]).
