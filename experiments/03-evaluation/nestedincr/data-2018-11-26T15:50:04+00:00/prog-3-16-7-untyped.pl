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
my_msort3(A,B):-msort(A,B).
my_max_list4(A,B):-max_list(A,B).
my_flatten5(A,B):-flatten(A,B).
my_even6(A):-0 is A mod 2.

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

my_element8(A,B):-member(B,A).
my_head9([H|_],H).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_lowercase11(A):-downcase_atom(A,A),char_code(A,_).
my_sumlist12(A,B):-sumlist(A,B).
my_uppercase13(A):-upcase_atom(A,A),char_code(A,_).
my_odd14(A):-1 is A mod 2.
my_set15(A):-list_to_set(A,A).
my_tail16([_|TL],TL).
my_tolower17(A,B):-downcase_atom(A,B),char_code(A,_).
prim(my_succ1/2).
prim(my_len2/2).
prim(my_msort3/2).
prim(my_max_list4/2).
prim(my_flatten5/2).
prim(my_even6/1).
prim(my_element8/2).
prim(my_head9/2).
prim(my_toupper10/2).
prim(my_lowercase11/1).
prim(my_sumlist12/2).
prim(my_uppercase13/1).
prim(my_odd14/1).
prim(my_set15/1).
prim(my_tail16/2).
prim(my_tolower17/2).
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
p([[1,3,3],[4,1,5],[7,7,4],[5,0,5]],[[3,5,5],[6,3,7],[9,9,6],[7,2,7]]).
p([[6,7,7,5],[5,4,7,1],[6,4,4,4]],[[8,9,9,7],[7,6,9,3],[8,6,6,6]]).
p([[1,6,0],[5,7,4,2],[2,1,7]],[[3,8,2],[7,9,6,4],[4,3,9]]).
p([[5,2,2],[6,3,1],[3,2,1],[4,3,4]],[[7,4,4],[8,5,3],[5,4,3],[6,5,6]]).
p([[3,3,5],[3,6,7,0],[0,7,2,0]],[[5,5,7],[5,8,9,2],[2,9,4,2]]).
q([[0,4,7],[3,4,4,0],[2,4,0,0]],[[2,6,9],[5,6,6,2],[2,4,0,0]]).
q([[3,7,5,2],[0,4,1,0],[7,5,5],[1,0,0,7]],[[5,9,7,4],[0,4,1,0],[9,7,7],[1,0,0,7]]).
q([[0,7,1,0],[1,1,2,3]],[[2,9,3,2],[1,1,2,3]]).
q([[4,3,6],[3,5,4,0],[6,7,3],[3,1,2]],[[6,5,8],[5,7,6,2],[6,7,3],[3,1,2]]).
q([[5,0,2,0],[0,0,6,0]],[[5,0,2,0],[2,2,8,2]]).
